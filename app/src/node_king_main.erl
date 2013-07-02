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

-record(king_checker_context, {king = #knode{}, failure_count = 0, success_count = 0, context = undefined}).

-spec king_find_king_default(Context :: any()) -> #knode{}.
king_find_king_default(_Context) ->
  #knode{}.

-spec king_checker_start(Timeout :: integer(), Context :: any()) -> {ok, pid()}.
king_checker_start(Timeout, Context) ->
  king_checker_start(Timeout, fun king_find_king_default/1, Context).


-spec king_checker_start(Timeout :: integer(), FindKingFunc :: fun(), Context :: any()) -> {ok, pid()}.
king_checker_start(Timeout, FindKingFunc, Context) ->
  king_checker_start(Timeout, FindKingFunc, Context, undefined).

-spec king_checker_start(Timeout :: integer(), FindKingFunc :: fun(), Context :: any(), King :: #knode{} | 'undefined') -> {ok, pid()}.
king_checker_start(Timeout, FindKingFunc, Context, King) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  Pid = spawn(?MODULE, king_checker, [Socket, Timeout, FindKingFunc, #king_checker_context{king = King, context = Context}]),
  gen_udp:controlling_process(Socket, Pid),
  {ok, Pid}.

-spec king_checker(Socket :: any(), Timeout :: integer(), FindKingFunc :: fun(), Context :: any()) -> none().
king_checker(Socket, Timeout, FindKingFunc, #king_checker_context{failure_count = FailureCount, success_count = SuccessCount} = Context) ->
  FindKing = case Context#king_checker_context.king of
    undefined ->
      FindKingFunc(Context#king_checker_context.context);
    King ->
      King
  end,
  ok = gen_udp:send(Socket, FindKing#knode.host, FindKing#knode.port, ?KING_ALIVE_REQUEST),
  receive
    {udp, Socket, _, _, ?KING_ALIVE_RESPONSE} ->
      king_checker(Socket, Timeout, FindKingFunc, Context#king_checker_context{success_count = SuccessCount + 1, failure_count = 0});
    {udp, Socket, _, _, Bin} ->
      %% TODO: Exception?
      io:format("Receive from king: ~p~n", [Bin]),
      king_checker(Socket, Timeout, FindKingFunc, Context#king_checker_context{success_count = SuccessCount + 1, failure_count = 0});
    %% Internal reboot
    {internal, reboot, Pid} ->
      gen_udp:close(Socket),
      {ok, Socket} = gen_udp:open(0, [binary]),
      Pid ! {ok, rebooted},
      king_checker(Socket, Timeout, FindKingFunc, Context);
    {internal, terminate, Pid} ->
      Pid ! {ok, terminated}
  after Timeout ->
    King0 = if
      FailureCount >= ?MAX_FAIL_REQ ->
        undefined; 
      true ->
        Context#king_checker_context.king
    end,
    king_checker(Socket, Timeout, FindKingFunc, Context#king_checker_context{failure_count = FailureCount + 1, king = King0})
  end,
  gen_udp:close(Socket).

-ifdef(TEST).

-spec king_checker_start_test1({Pid :: pid(), KingPort :: integer(), Timeout :: integer(), Context :: any()}) -> none().
king_checker_start_test1({Pid, KingPort, Timeout, _}) ->
  AfterTimeout = Timeout * 50,
  {ok, Socket} = gen_udp:open(KingPort, [binary, {active, false}]),
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, _Socket, _HostName, _Port, ?KING_ALIVE_REQUEST} ->
      Pid ! {internal, terminate, self()},
      ?assertEqual(1, 1)
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
    {udp, _Socket, _HostName, _Port, ?KING_ALIVE_REQUEST} ->
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
    {ok, Pid} = king_checker_start(Timeout, fun(_Context) ->
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


-record(king_loop_context, {context = undefined}).

-spec king_loop_start(Port :: port_type(), integer(), any()) -> {ok, pid()}.
king_loop_start(Port, Timeout, Context) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
  Pid = spawn(?MODULE, king_loop, [Socket, Timeout, #king_loop_context{context = Context}]),
  gen_udp:controlling_process(Socket, Pid),
  {ok, Pid}.

-spec king_loop(Socket :: any(), integer(), #king_loop_context{}) -> none().
king_loop(Socket, Timeout, Context) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, Socket, HostName, Port, ?KING_ALIVE_REQUEST} ->
      io:format("KingAlive request. HostName: ~p~n", [HostName]),
      gen_udp:send(Socket, HostName, Port, ?KING_ALIVE_RESPONSE),
      king_loop(Socket, Timeout, Context);
    {udp, Socket, HostName, _Port, Bin} ->
      io:format("Receive bin. HostName: ~p, Bin: ~p~n", [HostName, Bin]),
      king_loop(Socket, Timeout, Context)
  after Timeout ->
    io:format("Timeout~n", []),
    king_loop(Socket, Timeout, Context)
  end.


