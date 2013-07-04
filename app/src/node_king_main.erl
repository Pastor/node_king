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
-record(knode_config, {other, more, self = undefined}).
-record(king_loop_context, {context = undefined, self = #knode{}}).

-spec confval(Key, Default) -> Value when
  Key     :: string(),
  Default :: string(),
  Value   :: string().

-spec start() -> {ok, LoopPid, CheckPid} when
  LoopPid :: pid(),
  CheckPid:: pid().

-spec read_packet(Bin) -> {ok, Result} | {error, Reason} when 
  Bin     :: binary(),
  Result  :: #knode_packet{},
  Reason  :: string().

-spec write_packet(Packet) -> Bin when 
  Bin     :: binary(),
  Packet  :: #knode_packet{}.

-spec king_checker_start(Node, Socket, Timeout, Context) -> {ok, Pid, Socket0} when
  Node    :: #knode{},
  Socket  :: any(),
  Socket0 :: any(),
  Timeout :: integer(),
  Context :: any(),
  Pid     :: pid().
-spec king_checker_start(Node, Socket, Timeout, FindFunc, Context) -> {ok, Pid, Socket0} when
  Node    :: #knode{},
  Socket  :: any(),
  Socket0 :: any(),
  Timeout :: integer(),
  FindFunc:: fun(),
  Context :: any(),
  Pid     :: pid().
-spec king_checker_start(Node, Socket, Timeout, FindFunc, Context, King) -> {ok, Pid, Socket0} when
  Node    :: #knode{},
  Socket  :: any(),
  Socket0 :: any(),
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

-spec king_find(Socket, Timeout, Context, State) -> Result when
  Socket  :: any(),
  Timeout :: integer(),
  Context :: #knode_config{},
  State   :: 'request' | 'wait_king' | 'error' | 'wait_alive',
  Result  :: #knode{}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
king_find(Socket, Timeout, Context) ->
  king_find(Socket, Timeout, Context, request).

king_find(Socket, Timeout, #knode_config{self = Node} = Context, State) ->
  SendAlive = fun({_, #knode{host = Host, port = Port}}) ->
    Packet = write_packet(#knode_packet{message = ?KING_ALIVE_REQUEST, who = Node}),
    ok = gen_udp:send(Socket, Host, Port, Packet)
  end,
  SendAliveProcess = fun() ->
    [ SendAlive(Node0) || Node0 <- ets:tab2list(Context#knode_config.more) ]
  end,
  Wait = fun(WaitFunc, State0, Circle) ->
           inet:setopts(Socket, [{active, once}]),
           receive
             {udp, Socket, _, _, Bin} ->
               case read_packet(Bin) of
                 {ok, #knode_packet{who = #knode{id = Id}, message = ?KING_ALIVE_RESPONSE}} 
                     when Id >=  Node#knode.id ->
                   WaitFunc(WaitFunc, wait_king, 0);
                 {ok, #knode_packet{who = #knode{id = Id} = WhoNode, message = ?KING_IAM}}
                     when Id >=  Node#knode.id ->
                   WhoNode;
                 {error, Reason} ->
                   io:format("Error: ~p~nTry again~n", [Reason]),
                   WaitFunc(WaitFunc, error, Circle + 1)
               end      
           after Timeout ->
             case State0 of
               wait_king ->
                 if
                   Circle >= 100 ->
                     king_find(Socket, Timeout, Context, request);
                   true ->
                     WaitFunc(WaitFunc, wait, Circle + 1)
                 end;
               error ->
                 WaitFunc(WaitFunc, error, Circle + 1);
               wait_alive ->
                 Packet = write_packet(#knode_packet{message = ?KING_IAM, who = Node}),
                 SendIam = fun({_, #knode{host = Host, port = Port}}) ->
                             ok = gen_udp:send(Socket, Host, Port, Packet)
                           end,
                 [SendIam(Node0) || Node0 <- ets:tab2list(Context#knode_config.other)],
                 Node  
             end
           end
  end,
  case State of
    request ->
      spawn_link(SendAliveProcess),
      Wait(Wait, wait_alive, 0);
    error ->
      io:fromat("Call with error~n", []),
      Wait(Wait, error, 0)
  end.

confval(Key, Default) ->
  case application:get_env(Key) of
    undefined -> Default;
    Val       -> Val
  end.

start() ->
  FindFunc = fun king_find/3,
  KingLoopTimeout = confval(king_loop_timeout, 10000),
  KingCheckerTimeout = confval(king_checker_timeout, 1000),
  ConfigFile = confval(configfile, ?CONFIG_FILENAME),
  #knode_config{self = Node} = Config = read_config_file(ConfigFile),
  {ok, KingLoopPid, Socket} = king_loop_start(KingLoopTimeout, Node, Config),
  {ok, KingCheckerPid} = king_checker_start(Node, Socket, KingCheckerTimeout, FindFunc, Config),
  {ok, KingLoopPid, KingCheckerPid}.                                                     


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
king_checker_start(Node, Socket, Timeout, Context) ->
  king_checker_start(Node, Socket, Timeout, fun king_find/3, Context).


king_checker_start(Node, Socket, Timeout, FindKingFunc, Context) ->
  king_checker_start(Node, Socket, Timeout, FindKingFunc, Context, undefined).

king_checker_start(Node, Socket, Timeout, FindKingFunc, Context, King) ->
  ?debugFmt("Socket: ~p~n", [Socket]),
  Pid = spawn(?MODULE, king_checker, [Socket, Node, Timeout, FindKingFunc, #king_checker_context{king = King, context = Context}]),
  gen_udp:controlling_process(Socket, Pid),
  {ok, Pid, Socket}.

king_checker(Socket, Node, Timeout, FindKingFunc, #king_checker_context{failure_count = FailureCount, success_count = SuccessCount} = Context) ->
  FindKing = case Context#king_checker_context.king of
    undefined ->
      FindKingFunc(Socket, Timeout, Context#king_checker_context.context);
    King ->
      King
  end,
  Packet = write_packet(#knode_packet{message = ?KING_ALIVE_REQUEST, who = Node}),
  ok = gen_udp:send(Socket, FindKing#knode.host, FindKing#knode.port, Packet),
  receive
    {udp, Socket, _, _, Bin} ->
      case read_packet(Bin) of
        {ok, #knode_packet{message = ?KING_ALIVE_RESPONSE, who = FindKing}} ->
          king_checker(Socket, Node, Timeout, FindKingFunc, Context#king_checker_context{success_count = SuccessCount + 1, failure_count = 0});
        {error, Error} ->
          io:format("Error: ~p~n", [Error])
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
        Context#king_checker_context.king
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
    {udp, _Socket, _HostName, _Port, Bin} ->
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
    {udp, _Socket, _HostName, _Port, Bin} ->
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
    Timeout = 100,
    Node = #knode{host = "localhost", port = 9090},
    Socket = gen_udp:open(KingPort, [binary, {active, false}]),
    Func = fun(_Socket, _Timeout, _Context) ->
      #knode{host = "localhost", port = KingPort, id = 0}  
    end,
    Result = king_checker_start(Node, Socket, Timeout, Func, []),
    {ok, Pid, _} = Result,
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
    {udp, Socket, HostName, Port, Bin} ->
      case read_packet(Bin) of
        {ok, #knode_packet{message = ?KING_ALIVE_REQUEST}} ->
          Bin = write_packet(#knode_packet{message = ?KING_ALIVE_RESPONSE, who = Context#king_loop_context.self}),
          gen_udp:send(Socket, HostName, Port, Bin);
        {error, Message} ->
          io:format("Exception: ~p~n", [Message])
      end,
      king_loop(Socket, Timeout, Context)
  after Timeout ->
    io:format("Timeout~n", []),
    king_loop(Socket, Timeout, Context)
  end.

%% @doc config reader
read_config_file(FileName) ->
  ParseTermsFunc = fun
    (#knode{self = true} = Node, NodeConfig) ->
      NodeConfig#knode_config{self = Node};
    (Node, NodeConfig) ->
      ets:insert(NodeConfig#knode_config.other, {Node#knode.uuid, Node}),
      NodeConfig      
  end,
  Result = case file:open(FileName, [read]) of
    {ok, Device} ->
      read_config_line(Device, fun
        (Line, NodeConfig) ->
          {ok, Tokens, _} = erl_scan:string(Line),
          case Tokens of
            [] ->
              NodeConfig;
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
              }, NodeConfig)
          end
      end, #knode_config{
        more  = ets:new(?MODULE, [bag, {keypos, 2}]),
        other = ets:new(?MODULE, [bag, {keypos, 2}]),
        self  = undefined  
      });    
    {error, Reason} ->
      io:format("Error ~p~n", [Reason]),
      #knode_config{}  
  end, 
  case  Result#knode_config.self of
    undefined ->
      {ok, Result};
    #knode{id = Id} ->
      More = [{Uuid, Node} || {Uuid, #knode{id = ElemId} = Node} <- ets:tab2list(Result#knode_config.other), ElemId >= Id],
      [ets:delete(Result#knode_config.other, Uuid) || {Uuid, _} <- More],
      ets:insert(Result#knode_config.more, More),
      {ok, Result}
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
  io:format("~p~n", [ConfigFileName]),
  {ok, NodeConfig} = read_config_file(ConfigFileName),
  OtherList = ets:tab2list(NodeConfig#knode_config.other),
  OtherCount = length(OtherList),
  MoreList = ets:tab2list(NodeConfig#knode_config.more),
  MoreCount = length(MoreList),
  ?assert(OtherCount >= 1),
  ?assert(MoreCount >= 1).


-endif.


