-module(node_king_test).
-compile(export_all).



-spec main() -> none().
main() ->
  Items = [
    {"Name1", {0, 1, 2, 3, 4}},  
    {"Name2", 1},
    {"Name3", 2},
    {"Name4", 3},
    {"Name5", 4}
  ],
  Db = ets:new(?MODULE, [bag]),
  ets:insert(Db, Items),
  Result = ets:lookup(Db, "Name1"),
  Result = [{"Name1", {0, 1, 2, 3, 4}}],
  ets:delete_object(Db, hd(Result)),
  [] = ets:lookup(Db, "Name1").



