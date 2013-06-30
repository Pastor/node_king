-module(node_king_util).
-vsn(0.1).

-include("kntable.hrl").

-export([get_nodes_when/2, get_config_from_file/1]).

-record(config_readed, {self = none, list = []}).

%% @doc ...
-spec get_nodes_when([#knode{}], integer()) -> [#knode{}].
get_nodes_when(List, Id) ->
  FilterFunc = fun(#knode{id = NodeId, self = Self}) -> (NodeId >= Id) and (Self == false) end,
  CutList = lists:filter(FilterFunc, List),
  SortFunc = fun(#knode{id = Id1}, #knode{id = Id2}) -> Id1 < Id2 end,
  lists:sort(SortFunc, CutList).

-spec get_config_from_file(string()) -> {ok, #config_readed{}} | {error, string()}.
get_config_from_file(FileName) ->
  ParseTermsFunc = fun
    (#knode{self = true} = Node, #config_readed{list = List}) ->
      #config_readed{self = Node, list = List};
    (#knode{self = false} = Node, #config_readed{self = Self, list = List}) ->
      #config_readed{self = Self, list = List ++ [Node]};
    (_, ConfigReaded) ->
      ConfigReaded      
  end,
  Result = case file:open(FileName, [read]) of
    {ok, Device} ->
      config_each_line(Device, fun
        ([ <<'%'>> | _], ConfigReaded) ->
          ConfigReaded;
        (Line, ConfigReaded) ->
          {ok, Tokens, _} = erl_scan:string(Line),
          case Tokens of
            [] ->
              ConfigReaded;
            _ ->
              {ok, {Uuid, Name, Host, Port, Type, Id, Self}} = erl_parse:parse_term(Tokens),
              ParseTermsFunc(#knode{
                uuid = Uuid,
                name = Name,
                host = Host,
                port = Port,
                type = Type,
                id = Id,
                self = Self
              }, ConfigReaded)
          end
      end, #config_readed{});    
    {error, Reason} ->
      io:format("Error ~p~n", [Reason]),
      #config_readed{}  
  end, 
  {ok, Result}. 
  
-spec config_each_line(any(), any(), #config_readed{}) -> #config_readed{}.
config_each_line(Device, Func, Accum) ->
  case io:get_line(Device, "") of
    eof -> 
      file:close(Device),
      Accum;
    Line ->
      Accum0 = Func(Line, Accum),
      config_each_line(Device, Func, Accum0)
  end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec get_config_from_file_test() -> [any()].
get_config_from_file_test() ->
  {ok, Cwd} = file:get_cwd(),
  ConfigFile = [Cwd ++ "/../../node_king.config"],
  {ok, Result} = get_config_from_file(ConfigFile),
  ?assertEqual(9, length(Result#config_readed.list)),
  ?assertEqual(
    #knode{
      uuid = "D22F742F-6C03-1014-9F8A-EA4D69957277", 
      name = "nodeking006", 
      host = "localhost", 
      port = 9095, 
      type = 1, 
      id   = 7, 
      self = true 
    }, 
    Result#config_readed.self
  ).

-spec get_nodes_then_test_() -> [any()].
get_nodes_then_test_() ->
  TestData = [
    #knode{uuid="", name="node1", host="localhost", port=9090, type=0, id=1, self=false},
    #knode{uuid="", name="node2", host="localhost", port=9090, type=0, id=3, self=false},
    #knode{uuid="", name="node3", host="localhost", port=9090, type=0, id=8, self=true },
    #knode{uuid="", name="node4", host="localhost", port=9090, type=0, id=2, self=false},
    #knode{uuid="", name="node5", host="localhost", port=9090, type=0, id=7, self=false}
  ],
  [
    {"Empty",
     fun() ->
       ?assertEqual([], get_nodes_when([], 0))
     end
    },
    {"Only one",
     fun() ->
       Result = get_nodes_when(TestData, 7),      
       CheckResult = [
         #knode{uuid="", name="node5", host="localhost", port=9090, type=0, id=7, self=false}
       ],
       ?assertEqual(CheckResult, Result)
     end     
    },
    {"Ordered two",
     fun() ->
       Result = get_nodes_when(TestData, 3),       
       CheckResult = [
         #knode{uuid="", name="node2", host="localhost", port=9090, type=0, id=3, self=false},
         #knode{uuid="", name="node5", host="localhost", port=9090, type=0, id=7, self=false}
       ],
       ?assertEqual(CheckResult, Result)
     end     
    }
  ].

-endif.


