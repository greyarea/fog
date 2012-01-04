%% @doc application behaviour implementation.
-module(fog_app).

-behaviour(application).

%% application exports
-export([start/2,
         stop/1]).

%% @doc Start the fog application.
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    fog_sup:start_link().

%% @doc Stop the fog application.
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

