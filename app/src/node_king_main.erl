-module(node_king_main).
-vsn(0.1).
-compile([export_all]).

-include("kntable.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CONFIG_FILENAME, "node_king.config").
-define(KING_ALIVE_REQUEST,  <<"KING_ALIVE_REQUEST">>).
-define(KING_ALIVE_RESPONSE, <<"KING_ALIVE_RESPONSE">>).
-define(KING_IAM,            <<"KING_IAM">>).
-define(MAX_FAIL_REQ, 3).

-record(knode_packet, {message = undefined, who = #knode{}}).
-record(king_checker_context, {king = #knode{}, failure_count = 0, success_count = 0, context = undefined}).
-record(knode_config, {other = [#knode{}], more = [#knode{}], self = undefined}).
-record(king_loop_context, {context = undefined, self = #knode{}}).
-record(king_find_context, {success = 0, failure = 0, global = 0}).

-ifndef(TEST).
-define(debugFmt, io:format).
-endif.

-spec confval(Key, Default) -> Value when
  Key     :: string(),
  Default :: string(),
  Value   :: string().

-spec start_link() -> {ok, LoopPid, CheckPid} when
  LoopPid :: pid(),
  CheckPid:: pid().


-spec start() -> {ok, LoopPid, CheckPid} when
  LoopPid :: pid(),
  CheckPid:: pid().

-spec start(Config) -> {ok, LoopPid, CheckPid} when
  Config  :: #knode_config{},
  LoopPid :: pid(),
  CheckPid:: pid().

-spec read_packet(Bin) -> {ok, Result} | {error, Reason} when 
  Bin     :: binary(),
  Result  :: #knode_packet{},
  Reason  :: string().

-spec write_packet(Packet) -> Bin when 
  Bin     :: binary(),
  Packet  :: #knode_packet{}.

-spec king_checker_start(Node, Timeout, Context) -> {ok, Pid, Socket} when
  Node    :: #knode{},
  Socket  :: any(),
  Timeout :: integer(),
  Context :: any(),
  Pid     :: pid().
-spec king_checker_start(Node, Timeout, FindFunc, Context) -> {ok, Pid, Socket} when
  Node    :: #knode{},
  Socket  :: any(),
  Timeout :: integer(),
  FindFunc:: fun(),
  Context :: any(),
  Pid     :: pid().
-spec king_checker_start(Node, Timeout, FindFunc, Context, King) -> {ok, Pid, Socket} when
  Node    :: #knode{},
  Socket  :: any(),
  Timeout :: integer(),
  FindFunc:: fun(),
  Context :: any(),
  King    :: #knode{} | 'undefined',
  Pid     :: pid().

-spec king_checker(Socket, Node, Timeout, FindFunc, Context) -> Result when
  Socket  :: any(),
  Node    :: #knode{},
  Timeout :: integer(),
  FindFunc:: fun(),
  Context :: any(),
  Result  :: none().

-spec king_loop_start(Timeout, Node, Context) -> {ok, Pid, Socket} when
  Timeout :: integer(),
  Node    :: #knode{},
  Context :: any(),
  Pid     :: pid(),
  Socket  :: any().

-spec king_loop(Socket, Timeout, Context) -> Result when
  Socket  :: any(),
  Timeout :: integer(),
  Context :: #king_loop_context{},
  Result  :: none().

-spec read_config_file(FileName) -> {ok, Result} when
  FileName:: string(),
  Result  :: #knode_config{}.

-spec read_config_line(Device, Callback, Config) -> Result when
  Device  :: any(),
  Callback:: fun(),
  Config  :: #knode_config{},
  Result  :: #knode_config{}.


-spec king_find(Socket, Timeout, Context) -> Result when
  Socket  :: any(),
  Timeout :: integer(),
  Context :: #knode_config{},
  Result  :: #knode{}.

-spec king_find(Socket, Timeout, Context, State, FindCtx) -> Result when
  Socket  :: any(),
  Timeout :: integer(),
  Context :: #knode_config{},
  State   :: 'request' | 'wait_king' | 'error' | 'wait_alive',
  FindCtx :: #king_find_context{},
  Result  :: #knode{}.


foreach_objects(Objects, Callback) ->
  [ Callback(Object) || Object <- Objects ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
king_find(Socket, Timeout, Context) ->
  king_find(Socket, Timeout, Context, request, #king_find_context{}).

king_find(Socket, Timeout, #knode_config{self = Node, more = MoreObjects} = Context, State, #king_find_context{failure = ErrorCount, success = SuccessCount, global = GlobalCount}) ->
  ?debugFmt("Errors: ~p, Successes: ~p, Global: ~p, State: ~p~n", [ErrorCount, SuccessCount, GlobalCount, State]),
  SendAlive = fun(#knode{host = Host, port = Port}) ->
    Packet = write_packet(#knode_packet{message = ?KING_ALIVE_REQUEST, who = Node}),
    ok = gen_udp:send(Socket, Host, Port, Packet)
  end,
  case State of
    request ->
      spawn_link(?MODULE, foreach_objects, [MoreObjects, SendAlive]),
      king_find(Socket, Timeout, Context, wait_alive, #king_find_context{success = SuccessCount + 1, global = GlobalCount + 1});
    error ->
      ?debugFmt("Call with error~n", []),
      if 
        ErrorCount > 1000 ->
          king_find(Socket, Timeout, Context, request, #king_find_context{failure = 0, global = GlobalCount + 1});
        true ->
          king_find(Socket, Timeout, Context, wait_king, #king_find_context{failure = ErrorCount + 1, global = GlobalCount + 1})
      end;
    _ ->
      inet:setopts(Socket, [{active, once}]),
      receive
        {udp, Socket, _, _, Bin} ->
          case read_packet(Bin) of
            {ok, #knode_packet{who = #knode{id = Id}, message = ?KING_ALIVE_RESPONSE}} 
                when Id >  Node#knode.id ->
              king_find(Socket, Timeout, Context, wait_king, #king_find_context{success = 0, global = GlobalCount + 1});
            {ok, #knode_packet{who = #knode{id = Id} = WhoNode, message = ?KING_IAM}}
                when Id >  Node#knode.id ->
              WhoNode;
            {error, Reason} ->
              io:format("Error: ~p~nTry again~n", [Reason]),
              king_find(Socket, Timeout, Context, error, #king_find_context{failure = ErrorCount + 1, global = GlobalCount + 1})
          end;
        {udp_error, _Socket, Reason} ->
          ?debugFmt("Socket error: ~p~n", [Reason]),
          king_find(Socket, Timeout, Context, error, #king_find_context{failure = ErrorCount + 1, global = GlobalCount + 1});
        Result ->
          ?debugFmt("KingFind receive: ~p~n", [Result]),
          king_find(Socket, Timeout, Context, error, #king_find_context{failure = ErrorCount + 1, global = GlobalCount + 1})
      after Timeout ->
        case State of
          wait_king ->
            if
              SuccessCount >= 10 ->
                king_find(Socket, Timeout, Context, request, #king_find_context{success = SuccessCount + 1, global = GlobalCount + 1});
              true ->
                king_find(Socket, Timeout, Context, wait_king, #king_find_context{success = SuccessCount + 1, global = GlobalCount + 1})
            end;
          error ->
            king_find(Socket, Timeout, Context, error, #king_find_context{failure = ErrorCount + 1, global = GlobalCount + 1});
          wait_alive ->
            Packet = write_packet(#knode_packet{message = ?KING_IAM, who = Node}),
            SendIam = fun(#knode{host = Host, port = Port}) ->
                        ok = gen_udp:send(Socket, Host, Port, Packet)
                      end,
            [SendIam(Node0) || Node0 <- Context#knode_config.other],
            Node;
          _ ->
            ?debugFmt("Illegal state: ~p~n", [State]),
            king_find(Socket, Timeout, Context, error, #king_find_context{failure = ErrorCount + 1, global = GlobalCount + 1})
        end
      end
  end.

confval(Key, Default) ->
  case application:get_env(Key) of
    undefined -> Default;
    Val       -> Val
  end.

start_link() ->
  start().

start() ->
  ConfigFile = confval(configfile, ?CONFIG_FILENAME),
  ?debugFmt("Start reading config: ~p~n", [ConfigFile]),
  {ok, Config} = read_config_file(ConfigFile),  
  start(Config).

start(#knode_config{self = Node} = Config) ->
  ?debugFmt("Call start~n", []),
  case Node of
    undefined ->
      {error, "Illegal config"};
    _ ->
      FindFunc = fun king_find/3,
      KingLoopTimeout = confval(king_loop_timeout, 1000),
      KingCheckerTimeout = confval(king_checker_timeout, 1000),
      {ok, KingLoopPid, _SocketLoop} = king_loop_start(KingLoopTimeout, Node, Config),
      {ok, KingCheckerPid, _SocketChecker} = king_checker_start(Node, KingCheckerTimeout, FindFunc, Config),
      {ok, KingLoopPid, KingCheckerPid}
  end.

-ifdef(TEST).

-spec start_full_test() -> none().
start_full_test() ->
  MyNode  = #knode{
    host  = "localhost", 
    port  = 10101, 
    uuid  = "Unknown_UUID", 
    id    = 100, 
    type  = 0, 
    name  = "My Node", 
    self  = true, 
    alive = true
  },
  NodeConfig = #knode_config{
    self  = MyNode,
    more  = [],
    other = []
  },
  case start(NodeConfig) of
    {ok, LoopPid, CheckerPid} ->
      receive
      after 4000 ->
        LoopPid    ! {internal, terminate, self()},
        CheckerPid ! {internal, terminate, self()}
      end;
    {error, Reason} ->
      ?debugFmt("Error: ~p~n", [Reason]),
      ?assertEqual(1, 0)  
  end,  
  ?assertEqual(1, 1).

-endif.

%% @doc Packet work
read_packet(Bin) ->
  case binary_to_term(Bin, [safe]) of
    #knode_packet{} = Packet ->
      {ok, Packet};
    _ ->
      {error, "Illegal Packet"}
  end.

write_packet(Packet) ->
  term_to_binary(Packet).

-ifdef(TEST).

-spec read_write_packet_test() -> none().
read_write_packet_test() ->
  Packet = #knode_packet{message = "HELLO"},
  Bin = write_packet(Packet),
  {ok, Packet} = read_packet(Bin).

-spec read_packet_failure_test() -> none().
read_packet_failure_test() ->
  {error, "Illegal Packet"} = read_packet(term_to_binary({})).
-endif.

%% @doc King manager
king_checker_start(Node, Timeout, Context) ->
  king_checker_start(Node, Timeout, fun king_find/3, Context).


king_checker_start(Node, Timeout, FindKingFunc, Context) ->
  king_checker_start(Node, Timeout, FindKingFunc, Context, undefined).

king_checker_start(Node, Timeout, FindKingFunc, Context, King) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  Pid = spawn(?MODULE, king_checker, [Socket, Node, Timeout, FindKingFunc, #king_checker_context{king = King, context = Context}]),
  gen_udp:controlling_process(Socket, Pid),
  {ok, Pid, Socket}.

king_checker(Socket, Node, Timeout, FindKingFunc, #king_checker_context{failure_count = FailureCount, success_count = SuccessCount} = Context) ->
  ?debugFmt("Context#king_checker_context.king: ~p~n", [Context#king_checker_context.king]),
  FindKing = case Context#king_checker_context.king of
    undefined ->
      FindKingFunc(Socket, Timeout, Context#king_checker_context.context);
    King ->
      King
  end,
  case FindKing of
    Node ->
      receive
      after Timeout ->
        ?debugFmt("I am a robot. Who next?~n", []),
        king_checker(Socket, Node, Timeout, FindKingFunc, Context#king_checker_context{success_count = SuccessCount + 1, failure_count = 0, king = FindKing})  
      end;
    _ ->
      FindKing
  end,
  Packet = write_packet(#knode_packet{message = ?KING_ALIVE_REQUEST, who = Node}),
  ok = gen_udp:send(Socket, FindKing#knode.host, FindKing#knode.port, Packet),
  receive
    {udp, Socket, _, _, Bin} ->
      case read_packet(Bin) of
        {ok, #knode_packet{message = ?KING_ALIVE_RESPONSE, who = FindKing}} ->
          king_checker(Socket, Node, Timeout, FindKingFunc, Context#king_checker_context{success_count = SuccessCount + 1, failure_count = 0});
        {error, Error} ->
          ?debugFmt("Error: ~p~n", [Error])
      end;
    %% Internal reboot
    {internal, reboot, Pid} ->
      gen_udp:close(Socket),
      {ok, Socket0} = gen_udp:open(0, [binary]),
      Pid ! {ok, rebooted},
      king_checker(Socket0, Node, Timeout, FindKingFunc, Context);
    {internal, terminate, Pid} ->
      Pid ! {ok, terminated}
  after Timeout ->
    King0 = if
      FailureCount >= ?MAX_FAIL_REQ ->
        undefined; 
      true ->
        FindKing
    end,
    king_checker(Socket, Node, Timeout, FindKingFunc, Context#king_checker_context{failure_count = FailureCount + 1, king = King0})
  end,
  gen_udp:close(Socket).

-ifdef(TEST).

-spec king_checker_start_test1({Pid :: pid(), KingPort :: integer(), Timeout :: integer(), Context :: any()}) -> none().
king_checker_start_test1({Pid, _Socket, KingPort, Timeout, _}) ->
  AfterTimeout = Timeout * 50,
  {ok, Socket} = gen_udp:open(KingPort, [binary, {active, false}]),
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, _Socket0, _HostName, _Port, Bin} ->
      {ok, #knode_packet{message = ?KING_ALIVE_REQUEST, who = Who}} = read_packet(Bin),
      Packet = write_packet(#knode_packet{message = ?KING_ALIVE_RESPONSE, who = undefined}),
      gen_udp:send(Socket, Who#knode.host, Who#knode.port, Packet),
      Pid ! {internal, terminate, self()},
      ?assertEqual(1, 1)
  after AfterTimeout ->
    ?assertEqual(1, 0)
  end,
  gen_udp:close(Socket),
  ?_assertEqual(1, 1).

-spec king_checker_start_test2({Pid :: pid(), KingPort :: integer(), Timeout :: integer(), Context :: any()}) -> none().
king_checker_start_test2({_Pid, _Socket, KingPort, Timeout, _Context}) ->
  AfterTimeout = Timeout * 50,
  {ok, Socket} = gen_udp:open(KingPort, [binary, {active, false}]),
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, _Socket0, _HostName, _Port, Bin} ->
      {ok, #knode_packet{message = ?KING_ALIVE_REQUEST}} = read_packet(Bin),
      ?assertEqual(1, 1)
  after AfterTimeout ->
    ?assertEqual(1, 0)
  end,
  gen_udp:close(Socket),
  ?_assertEqual(1, 1).

-spec king_checker_start_test3({Pid :: pid(), KingPort :: integer(), Timeout :: integer(), Context :: any()}) -> none().
king_checker_start_test3({Pid, _, _, _, _Context}) ->
  link(Pid),
  Pid ! {internal, reboot, self()},
  receive
    {ok, rebooted} ->
      ?assertEqual(1, 1)  
  after 2000 ->
    ?assertEqual(1, 0)
  end,
  unlink(Pid),
  ?_assertEqual(1, 1).



-spec king_checker_start_test_() -> none().
king_checker_start_test_() ->
 {foreach,
  fun() ->
    KingPort = 9090,
    Timeout = 1000,
    Node = #knode{host = "localhost", port = 9090},
    Func = fun(_Socket, _Timeout, _Context) ->
      #knode{host = "localhost", port = KingPort, id = 0}  
    end,
    {ok, Pid, Socket} = king_checker_start(Node, Timeout, Func, []),
    {Pid, Socket, KingPort, Timeout, []}
  end,
  fun({Pid, Socket, _KingPort, _Timeout, _Context}) ->
    Pid ! {internal, terminate, self()},
    receive
      {ok, terminated} ->
        ?assertEqual(1, 1)
    after 2000 ->
      ?assertEqual(1, 0)
    end,
    gen_udp:close(Socket)       
  end,
  [
    fun king_checker_start_test1/1,    
    fun king_checker_start_test2/1,
    fun king_checker_start_test3/1
  ]
 }.

-endif.

%% @doc response_loop


king_loop_start(Timeout, Node, Context) ->
  {ok, Socket} = gen_udp:open(Node#knode.port, [binary, {active, false}]),
  Pid = spawn(?MODULE, king_loop, [Socket, Timeout, #king_loop_context{context = Context, self = Node}]),
  gen_udp:controlling_process(Socket, Pid),
  {ok, Pid, Socket}.

king_loop(Socket, Timeout, Context) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, Socket, _HostName, _Port, Bin} ->
      case read_packet(Bin) of
        {ok, #knode_packet{message = Message, who = #knode{host = WhoHost, port = WhoPort}} = WhoNode} ->
          case Message of
            ?KING_ALIVE_REQUEST -> 
              ResponseBin = write_packet(#knode_packet{message = ?KING_ALIVE_RESPONSE, who = Context#king_loop_context.self}),
              gen_udp:send(Socket, WhoHost, WhoPort, ResponseBin);
            ?KING_ALIVE_RESPONSE ->
              if 
                WhoNode =:= Context#king_loop_context.self ->
                  ?debugFmt("Self messaging~n", []);
                true ->
                  ?debugFmt("Response from: ~p~n", [WhoNode])
              end
          end;
        {error, Message} ->
          ?debugFmt("Exception: ~p~n", [Message]);
        Result ->
          ?debugFmt("Result: ~p~n", [Result])
      end,
      king_loop(Socket, Timeout, Context);
    %%
    {internal, terminate, Pid} ->
      Pid ! {ok, terminated};
    %%
    Other ->
      ?debugFmt("Other receive: ~p~n", [Other]),
      king_loop(Socket, Timeout, Context)
  after Timeout ->
    ?debugFmt("Timeout~n", []),
    king_loop(Socket, Timeout, Context)
  end.

%% @doc config reader
read_config_file(FileName) ->
  ?debugFmt("read_config_file~n", []),
  NodeConfig = #knode_config{
    more  = [],
    other = [],
    self  = undefined  
  },
  ParseTermsFunc = fun
    (#knode{self = true} = Node, NodeConfig0) ->
      NodeConfig0#knode_config{self = Node};
    (Node, NodeConfig0) ->
      Other = NodeConfig0#knode_config.other ++ [Node],
      NodeConfig0#knode_config{other = Other}      
  end,
  ?debugFmt("Open file~n", []),
  Result = case file:open(FileName, [read]) of
    {ok, Device} ->
      ?debugFmt("Read lines~n", []),
      read_config_line(Device, fun
        (Line, NodeConfig0) ->
          {ok, Tokens, _} = erl_scan:string(Line),
          case Tokens of
            [] ->
              NodeConfig0;
            _ ->
              {ok, {Uuid, Name, Host, Port, Type, Id, Self}} = erl_parse:parse_term(Tokens),
              ParseTermsFunc(#knode{
                uuid = Uuid,
                name = Name,
                host = Host,
                port = Port,
                type = Type,
                id = Id,
                self = Self,
                alive = true
              }, NodeConfig0)
          end
      end, NodeConfig);    
    {error, Reason} ->
      ?debugFmt("Error ~p~n", [Reason]),
      NodeConfig  
  end, 
  ?debugFmt("Check result~n", []),
  case  Result#knode_config.self of
    undefined ->
      {ok, Result};
    #knode{id = Id} ->
      {More, Other} = lists:foldl(fun(#knode{id = ElementId} = Element, {More, Other}) ->
        if
          ElementId > Id ->
            {More ++ [Element], Other};
          ElementId =:= Id ->
            {More, Other};
          true ->
            {More, Other ++ [Element]}
        end    
      end, {[], []}, Result#knode_config.other),
      {ok, Result#knode_config{more = More, other = Other}}
  end.
  
read_config_line(Device, Func, Accum) ->
  case io:get_line(Device, "") of
    eof -> 
      file:close(Device),
      Accum;
    Line ->
      Accum0 = Func(Line, Accum),
      read_config_line(Device, Func, Accum0)
  end.

-ifdef(TEST).

-spec node_config_self_test() -> none().
node_config_self_test() ->
  {ok, CurrentDirectory} = file:get_cwd(),
  ConfigFileName = CurrentDirectory ++ "/../../node_king.config",
  {ok, NodeConfig} = read_config_file(ConfigFileName),
  ?assertNotEqual(NodeConfig#knode_config.self, undefined).

-spec node_config_count_test() -> none().
node_config_count_test() ->
  {ok, CurrentDirectory} = file:get_cwd(),
  ConfigFileName = CurrentDirectory ++ "/../../node_king.config",
  {ok, NodeConfig} = read_config_file(ConfigFileName),
  OtherCount = length(NodeConfig#knode_config.other),
  MoreCount = length(NodeConfig#knode_config.more),
  ?assert(OtherCount >= 1),
  ?assert(MoreCount >= 1).


-endif.


