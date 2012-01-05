-module(fog).
-behaviour(gen_server).

-export([
        start/0,
        start_link/0, 
        request/4
    ]).

%% gen_server exports
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, { 
        requests :: list(), 
        partial_responses :: list()        
    }).

-type state() :: #state{}.

-spec start() -> ok.
start() ->
    ensure_started(sasl),
    ensure_started(lager),
    ensure_started(ibrowse),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
    ok = application:start(fog).

%% @doc Ensure a dependent OTP application is started.
-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, _}} ->
            ok
    end.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%fog:request("BAADvkLCXkdQBANL9qkYrxldTLPGqbpuvaeGStAZC7PW9g8J7ZC3dWV5cJW7dMZCAkcA1VnYrxxe6zD9Ne6dzsqm3U9XOOM48WyJzBORHRhcajAY0zVS", "me/friends", [], get).
request(AccessToken, GraphPath, Params, HTTPMethod) ->
    gen_server:call(?MODULE, {request, AccessToken, GraphPath, Params, HTTPMethod}).

%% @doc gen_server behaviour entry point.
-spec init(frl_types:config()) -> {ok, state()} | {stop, term()}.
init(_Config) ->

    {ok, #state { requests = [], partial_responses = [] }}.

%% @doc Handle synchronous requests.
-spec handle_call(term(), {pid(), term()}, state()) -> {stop, term(), state()}.
handle_call({request, AccessToken, GraphPath, Params, HTTPMethod}, From, State) when HTTPMethod == get orelse HTTPMethod == post orelse HTTPMethod == delete ->

    ReqParams = [{access_token, AccessToken}|Params],
    Query = mochiweb_util:urlencode(ReqParams),

    URI = mochiweb_util:urlunsplit({"https", "graph.facebook.com/", GraphPath, Query, []}),
    error_logger:info_msg("URI ~p", [URI]),

    {ibrowse_req_id, Id} = ibrowse:send_req(URI, [], HTTPMethod, [],
        [
            {is_ssl, true},
            {ssl_options, []},
            {stream_to, self()}
        ]),

    Request = {Id, From, props:new()}, 
    NewRequests = [Request|State#state.requests], 

    {noreply, State#state { requests = NewRequests }};

handle_call(_Request, _From, State) ->
    {stop, unknown_call, State}.

%% @doc Handle asynchronous requests.
-spec handle_cast(term(), state()) -> {stop, term(), state()}.
handle_cast(_Request, State) ->
    {stop, unknown_cast, State}.

handle_info({ibrowse_async_headers, Id, RetCode, Headers}, State) ->
    NewRequests = case lists:keyfind(Id, 1, State#state.requests) of
        false ->
            error_logger:info_msg("Headers: Couldn't find ~p", [Id]), 
            State#state.requests;
        Request ->
            {Id, From, Props} = Request,
            Props2 = props:set(<<"retcode">>, RetCode, Props),
            Props3 = props:set(<<"headers">>, props:make(Headers), Props2), 
            Request2 = {Id, From, Props3}, 
            lists:keyreplace(Id, 1, State#state.requests, Request2) 
    end, 

    {noreply, State#state { requests = NewRequests }};

handle_info({ibrowse_async_response, Id, Body}, State) ->
    NewRequests = case lists:keyfind(Id, 1, State#state.requests) of
        false ->
            error_logger:info_msg("Body: Couldn't find ~p", [Id]), 
            State#state.requests;
        Request ->
            {Id, From, Props} = Request,
            Props2 = props:set(<<"body">>, Body, Props),
            Request2 = {Id, From, Props2}, 
            lists:keyreplace(Id, 1, State#state.requests, Request2) 
    end, 

    {noreply, State#state { requests = NewRequests }};


handle_info({ibrowse_async_response_end, Id}, State) ->
    NewRequests = case lists:keytake(Id, 1, State#state.requests) of
        false ->
            error_logger:info_msg("Body: Couldn't find ~p", [Id]), 
            State#state.requests;
        {value, Request, Rest} ->
            {Id, From, Props} = Request,
            gen_server:reply(From, {ok, Props}),
            Rest
    end, 

    {noreply, State#state { requests = NewRequests }};

handle_info(_Info, State) ->
    {stop, unknown_info, State}.

%% @doc Code change callback.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Termination callback.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

