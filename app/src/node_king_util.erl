-module(node_king_util).
-vsn(0.1).

-include("kntable.hrl").

-export([get_nodes_when/2, get_config_from_file/1, launch_periodic/4, launch_process/4]).

-define(PERIODIC_CIRCLE, 4).
-record(config_readed, {self = none, list = []}).

%% @doc ...
-spec get_nodes_when([#knode{}], integer()) -> [#knode{}].
get_nodes_when(List, Id) ->
  FilterFunc = fun(#knode{id = NodeId, self = Self}) -> (NodeId >= Id) and (Self == false) end,
  CutList = lists:filter(FilterFunc, List),
  SortFunc = fun(#knode{id = Id1}, #knode{id = Id2}) -> Id1 < Id2 end,
  lists:sort(SortFunc, CutList).

-spec get_config_from_file(string()) -> {ok, #config_readed{}}.
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
  
-spec config_each_line(any(), fun(), #config_readed{}) -> #config_readed{}.
config_each_line(Device, Func, Accum) ->
  case io:get_line(Device, "") of
    eof -> 
      file:close(Device),
      Accum;
    Line ->
      Accum0 = Func(Line, Accum),
      config_each_line(Device, Func, Accum0)
  end.

%% @doc 
%%-spec periodic_func(PeriodicContext :: any()) -> 
%%  {ok, PeriodicContext :: any()} | {error, Reason :: string(), PeriodicContext :: any()}.
-record(launch_context, {periodic_context = [], failure_count = 0, success_count = 0}).

-spec launch_periodic(fun(), fun(), integer(), any()) -> 
  {'ok', pid()}.
launch_periodic(PeriodicFunc, FailureFunc, Timeout, PeriodicContext) ->  
  Pid = spawn(?MODULE, launch_process, [PeriodicFunc, FailureFunc, Timeout, #launch_context{periodic_context = PeriodicContext}]),
  {ok, Pid}.

-spec launch_process(fun(), fun(), integer(), #launch_context{}) -> 
  {'ok', 'aborted', any()}.
launch_process(PeriodicFunc, FailureFunc, Timeout, #launch_context{failure_count = ErrorCount, success_count = SuccessCount} = LaunchContext) ->
  receive
    {terminate, Pid} ->
      Pid ! {ok, terminate, LaunchContext};
    {reboot, _Pid} ->
      launch_process(PeriodicFunc, FailureFunc, Timeout, LaunchContext)
  after Timeout ->
    Result = if
      ErrorCount >= ?PERIODIC_CIRCLE ->
        FailureFunc(LaunchContext#launch_context.periodic_context);
      true ->
        PeriodicFunc(LaunchContext#launch_context.periodic_context)              
    end,
    case Result of
      {ok, PeriodicContext0} ->
        launch_process(
          PeriodicFunc, 
          FailureFunc, 
          Timeout, 
          LaunchContext#launch_context{
            periodic_context = PeriodicContext0, 
            failure_count = 0, 
            success_count = SuccessCount + 1
          }
        );      
      {error, Reason, PeriodicContext0} ->
        io:format("Error: ~p~n", [Reason]),
        launch_process(
          PeriodicFunc, 
          FailureFunc, 
          Timeout, 
          LaunchContext#launch_context{
            periodic_context = PeriodicContext0, 
            failure_count = ErrorCount + 1
          }
        )
    end
  end,
  {ok, aborted, LaunchContext}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec launch_periodic_test_() -> [any()].
launch_periodic_test_() ->
  SuccessPeriodicFunc = fun(PeriodicContext) -> {ok, PeriodicContext} end,
  FailurePeriodicFunc = fun(PeriodicContext) -> {error, "Test failure", PeriodicContext} end,
  SuccessFailureFunc = SuccessPeriodicFunc,
  FailureFailureFunc = FailurePeriodicFunc,
  Timeout = 200,
  SuccessCircle = 10,
  FailureCircle = 1,
  OnFailSuccessCircle = ?PERIODIC_CIRCLE + 1,
  OnFailureCircle = 10,
  [
    {"Success",
     fun() ->
       SuccessTimeout = (SuccessCircle * Timeout) + Timeout,
       {ok, Pid} = launch_periodic(SuccessPeriodicFunc, SuccessFailureFunc, Timeout, undefined),
       receive
       after SuccessTimeout ->
         Pid ! {terminate, self()},
         receive
           {ok, terminate, LaunchContext} ->
             ?assertEqual(LaunchContext#launch_context.success_count, SuccessCircle);
           _ ->
             ?assertEqual(1, 0)
         after 10000 ->
           ?assertEqual(1, 0)
         end
       end,
       ?assertEqual(1, 1)
     end
    },
    {"Failure",
     fun() ->
       %% 1 - success call
       FailureTimeout = (FailureCircle * Timeout) + Timeout,
       {ok, Pid} = launch_periodic(FailurePeriodicFunc, SuccessFailureFunc, Timeout, undefined),
       receive
       after FailureTimeout ->
         Pid ! {terminate, self()},
         receive
           {ok, terminate, LaunchContext} ->
             ?assertEqual(LaunchContext#launch_context.failure_count, FailureCircle);
           _ ->
             ?assertEqual(1, 0)
         after 10000 ->
           ?assertEqual(1, 0)
         end
       end,
       ?assertEqual(1, 1)
     end
    },
    {"OnFailSuccess",
     fun() ->
       OnFailSuccessTimeout = (OnFailSuccessCircle * Timeout) + Timeout,
       {ok, Pid} = launch_periodic(FailurePeriodicFunc, SuccessFailureFunc, Timeout, undefined),
       receive
       after OnFailSuccessTimeout ->
         Pid ! {terminate, self()},
         receive
           {ok, terminate, LaunchContext} ->
             #launch_context{failure_count = FailureCount, success_count = SuccessCount} = LaunchContext,
             ?assertEqual(FailureCount, 0),
             ?assertEqual(SuccessCount, 1);
           _ ->
             ?assertEqual(1, 0)
         after 10000 ->
           ?assertEqual(1, 0)
         end
       end,
       ?assertEqual(1, 1)
     end
    },
    {"OnFailure",
     fun() ->
       OnFailureTimeout = (OnFailureCircle * Timeout) + Timeout,
       {ok, Pid} = launch_periodic(FailurePeriodicFunc, FailureFailureFunc, Timeout, undefined),
       receive
       after OnFailureTimeout ->
         Pid ! {terminate, self()},
         receive
           {ok, terminate, LaunchContext} ->
             #launch_context{failure_count = FailureCount, success_count = SuccessCount} = LaunchContext,
             ?assertEqual(FailureCount, OnFailureCircle),
             ?assertEqual(SuccessCount, 0);
           _ ->
             ?assertEqual(1, 0)
         after 10000 ->
           ?assertEqual(1, 0)
         end
       end,
       ?assertEqual(1, 1)
     end
    }

  ].

-spec get_config_from_file_test() -> [any()].
get_config_from_file_test() ->
  {ok, Cwd} = file:get_cwd(),
  ConfigFile = [Cwd ++ "/../../node_king.config"],
  {ok, Result} = get_config_from_file(ConfigFile),
  ?assertEqual(9, length(Result#config_readed.list)),
  Self0 =  Result#config_readed.self,
  ?assertEqual(
    #knode{
      name = "nodeking006", 
      host = "localhost", 
      port = 9095, 
      type = 1, 
      id   = 7, 
      self = true 
    }, 
    Self0#knode{uuid = undefined}
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


