-module(exp_server).
-author("Pastor").
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



-export([init/1, handle_call/3, handle_cast/2]). 
-export([handle_info/2, terminate/2, code_change/3]).


-export([stop/0]).

-export([start/0, ping/0]).


-spec stop() -> any().

-spec ping() -> {ok, pong}.

-spec start() -> {ok, Pid} when
  Pid        :: pid().

-spec init(Args) -> {ok, State} when
  Args       :: [],
  State      :: any().

-spec terminate(Args, State) -> ok when
  Args       :: any(),
  State      :: any().

-spec handle_call(ping, From, State) -> {reply, Response, State} when
  From       :: pid(),
  Response   :: any() | 'error',
  State      :: any().

-spec handle_cast(Message, State) -> any() when
  Message    :: any(),
  State      :: any().

-spec handle_info(Message, State) -> {noreplay, State} when
  Message    :: any(),
  State      :: any().

-spec code_change(OldVersion, State, Extra) -> {ok, State} when
  OldVersion :: any(),
  Extra      :: any(),
  State      :: any().


stop() ->
  gen_server:cast(?MODULE, stop).

ping() ->
  gen_server:call(?MODULE, ping).

-ifdef(TEST).

-spec ping_test() -> none().
ping_test() ->
  start(),
  Result = ping(),
  stop(),
  ?assertEqual(Result, {ok, pong}).

-endif.

start() ->             
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
  State = [],
  {ok, State}.

handle_call(ping, _From, State) ->
  Response = {ok, pong},
  {reply, Response, State};

handle_call(_Message, _From, State) -> 
  {reply, error, State}.


%- Stubs

terminate(Reason, _State) ->                 
  ok.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Message, State) -> 
  {noreply, State}.

handle_info(_Message, State) -> 
  {noreply, State}.

code_change(_OldVersion, State, _Extra) -> 
  {ok, State}.
