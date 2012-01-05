%% @doc Root supervisor for fog.
-module(fog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor exports
-export([init/1]).

%% @doc Start the root supervisor fog_sup.
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Supervisor behaviour entry point.
-spec init(term()) -> {ok, {{supervisor:strategy(), integer(), integer()}, [supervisor:child_spec()]}}.
init(_Config) ->
    Fog =
        {fog, {fog, start_link, []},
         permanent, 2000, worker, [fog]},

    {ok, {{one_for_one, 5, 10}, [Fog]}}.

