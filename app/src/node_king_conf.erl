-module(node_king_conf).
-vsn(0.1).
-behaviour(gen_server).
-include("kntable.hrl").


-export([start_link/0]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CONFIG_FILENAME, "node_king.config").
-record(state, {config=[#knode{}], self=#knode{}}).


-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec read_from_file(string()) -> [any()].
read_from_file(FileName) ->
  case file:consult(FileName) of
    {ok, TermList} ->
      TermList;
    {error, Reason} ->
      io:format("Error: ~p~n", [Reason]),
      []
  end.

-spec confval(any(), any()) -> any().
confval(Key, Default) ->
  case application:get_env(Key) of
    undefined -> Default;
    Val       -> Val
  end.

-spec init([]) -> {ok, #state{}}.
init([]) ->
  ConfigFile = confval(configfile, ?CONFIG_FILENAME),
  TermList = read_from_file(ConfigFile),
  {ok, #state{config=TermList}}.


-spec handle_call(any(), any(), any()) -> {atom(), atom(), any()}.
handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

-spec handle_cast(any(), any()) -> {atom(), any()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(any(), any()) -> {atom(), any()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

