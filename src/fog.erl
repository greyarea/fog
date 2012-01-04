-module(fog).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(st, { tid :: integer() }).

-type state() :: #st{}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc gen_server behaviour entry point.
-spec init(frl_types:config()) -> {ok, state()} | {stop, term()}.
init(_Config) ->

    {ok, ok}.

%% @doc Handle synchronous requests.
-spec handle_call(term(), {pid(), term()}, state()) -> {stop, term(), state()}.
handle_call(_Request, _From, State) ->
    {stop, unknown_call, State}.

%% @doc Handle asynchronous requests.
-spec handle_cast(term(), state()) -> {stop, term(), state()}.
handle_cast(_Request, State) ->
    {stop, unknown_cast, State}.

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


