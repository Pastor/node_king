-module(node_king_ddb).
-vsn(0.1).


-export([init_db/1]).

-spec init_db(any()) -> none().
init_db(_Settings) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(kingnodes, []),
  mnesia:info().

