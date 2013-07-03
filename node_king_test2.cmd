rem @echo off

set OPTS=-noshell

rebar clean compile && erl +W w -mnesia dir '"./ddb"' -name erlang_node_king@localhost %OPTS% -pa app/ebin -s node_king_test main -s init stop

