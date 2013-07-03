-module(node_king_main).
-vsn(0.1).
-compile([export_all]).

-include("kntable.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(KING_ALIVE_REQUEST,  <<"KING_ALIVE_REQUEST">>).
-define(KING_ALIVE_RESPONSE, <<"KING_ALIVE_RESPONSE">>).
-define(MAX_FAIL_REQ, 3).

-record(knode_packet, {message = undefined, who = #knode{}}).
-record(king_checker_context, {king = #knode{}, failure_count = 0, success_count = 0, context = undefined}).
-record(knode_config, {other, more, self = undefined}).

%% @doc Packet work
-spec read_packet(Bin) -> {ok, any()} | {error, string()} 
  when Bin :: binary().
read_packet(Bin) ->
  case binary_to_term(Bin, [safe]) of
    #knode_packet{} = Packet ->
      {ok, Packet};
    _ ->
      {error, "Illegal Packet"}
  end.

-spec write_packet(Packet) -> Bin when 
  Bin :: binary(),
  Packet :: #knode_packet{}.
write_packet(Packet) ->
  term_to_binary(Packet).

-ifdef(TEST).

-spec read_write_packet_test() -> none().
read_write_packet_test() ->
  Packet = #knode_packet{message = "HELLO"},
  Bin = write_packet(Packet),
  {ok, Packet} = read_packet(Bin).
-endif.

%% @doc King manager
-spec king_find_king_default(Socket :: any(), Context :: any()) -> #knode{}.
king_find_king_default(_Socket, _Context) ->
  #knode{}.

-spec king_checker_start(Node:: #knode{}, Timeout :: integer(), Context :: any()) -> {ok, pid()}.
king_checker_start(Node, Timeout, Context) ->
  king_checker_start(Node, Timeout, fun king_find_king_default/2, Context).


-spec king_checker_start(Node:: #knode{}, Timeout :: integer(), FindKingFunc :: fun(), Context :: any()) -> {ok, pid()}.
king_checker_start(Node, Timeout, FindKingFunc, Context) ->
  king_checker_start(Node, Timeout, FindKingFunc, Context, undefined).

-spec king_checker_start(Node:: #knode{}, Timeout :: integer(), FindKingFunc :: fun(), Context :: any(), King :: #knode{} | 'undefined') -> {ok, pid()}.
king_checker_start(Node, Timeout, FindKingFunc, Context, King) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  Pid = spawn(?MODULE, king_checker, [Socket, Node, Timeout, FindKingFunc, #king_checker_context{king = King, context = Context}]),
  gen_udp:controlling_process(Socket, Pid),
  {ok, Pid}.

-spec king_checker(Socket :: any(), Node:: #knode{}, Timeout :: integer(), FindKingFunc :: fun(), Context :: any()) -> none().
king_checker(Socket, Node, Timeout, FindKingFunc, #king_checker_context{failure_count = FailureCount, success_count = SuccessCount} = Context) ->
  FindKing = case Context#king_checker_context.king of
    undefined ->
      FindKingFunc(Socket, Context#king_checker_context.context);
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
      {ok, Socket} = gen_udp:open(0, [binary]),
      Pid ! {ok, rebooted},
      king_checker(Socket, Node, Timeout, FindKingFunc, Context);
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
king_checker_start_test1({Pid, KingPort, Timeout, _}) ->
  AfterTimeout = Timeout * 50,
  {ok, Socket} = gen_udp:open(KingPort, [binary, {active, false}]),
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, _Socket, _HostName, _Port, Bin} ->
      {ok, #knode_packet{message = ?KING_ALIVE_REQUEST}} = read_packet(Bin),
      Pid ! {internal, terminate, self()},
      ?assertEqual(1, 1);
    More ->
      ?debugFmt("More: ~p~n", [More])
  after AfterTimeout ->
    ?assertEqual(1, 0)
  end,
  gen_udp:close(Socket),
  ?_assertEqual(1, 1).

-spec king_checker_start_test2({Pid :: pid(), KingPort :: integer(), Timeout :: integer(), Context :: any()}) -> none().
king_checker_start_test2({_Pid, KingPort, Timeout, _Context}) ->
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


-spec king_checker_start_test_() -> none().
king_checker_start_test_() ->
 {foreach,
  fun() ->
    KingPort = 9090,
    Timeout = 100,
    Node = #knode{host = "localhost", port = 9090},
    {ok, Pid} = king_checker_start(Node, Timeout, fun(_Socket, _Context) ->
      #knode{host = "localhost", port = KingPort, id = 0}  
    end, []),
    {Pid, KingPort, Timeout, []}
  end,
  fun({Pid, _KingPort, _Timeout, _Context}) ->
    Pid ! {internal, terminate, self()},
    receive
      {ok, terminated} ->
        ?assertEqual(1, 1)
    after 2000 ->
      ?assertEqual(1, 0)
    end       
  end,
  [
    fun king_checker_start_test1/1,    
    fun king_checker_start_test2/1
  ]
 }.

-endif.

%% @doc response_loop
-record(king_loop_context, {context = undefined, self = #knode{}}).

-spec king_loop_start(Port :: port_type(), integer(), #knode{}, any()) -> {ok, pid()}.
king_loop_start(Port, Timeout, Node, Context) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
  Pid = spawn(?MODULE, king_loop, [Socket, Timeout, #king_loop_context{context = Context, self = Node}]),
  gen_udp:controlling_process(Socket, Pid),
  {ok, Pid}.

-spec king_loop(Socket :: any(), integer(), #king_loop_context{}) -> none().
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
-spec read_config_file(string()) -> {ok, #knode_config{}}.
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
      ets:foldr(fun
                 ({Uuid, #knode{id = ElemId} = Node}, NodeConfig) when ElemId >= Id  ->
                  ets:insert(NodeConfig#knode_config.more, {Uuid, Node}),
                  %% Remove node
                  %%Result = ets:lookup(NodeConfig#knode_config.other, Uuid),
                  %%?debugFmt("Lookup: ~p~n", [Result]),
                  ets:delete(NodeConfig#knode_config.other, Uuid),
                  NodeConfig;
                 (_, NodeConfig) ->
                  NodeConfig
                end, Result, Result#knode_config.other
      ),
      {ok, Result}
  end.
  
-spec read_config_line(Device :: any(), fun(), #knode_config{}) -> #knode_config{}.
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


