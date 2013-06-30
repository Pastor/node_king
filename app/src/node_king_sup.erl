-module(node_king_sup).
-vsn(0.1).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    io:format("node_king_sup:start_link()~n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) -> {ok, { {atom(), integer(), integer()}, [any()] }}.
init([]) ->
    io:format("node_king_sup:init()~n", []),
    node_king_ddb:init_db([]),
    NodeKingConf = ?CHILD(node_king_conf, worker),
    Specs = [
      NodeKingConf
    ],
    {ok, { {one_for_one, 5, 10}, Specs} }.

