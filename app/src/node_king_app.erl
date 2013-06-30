-module(node_king_app).
-vsn(0.1).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0]).

-spec start() -> none().
start() ->
  io:format("Start king application...~n", []),
  start(none, none).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(any(), any()) -> any().
start(_StartType, _StartArgs) ->
    io:format("node_king_app:start(~p, ~p)~n", [_StartType, _StartArgs]),
    node_king_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
