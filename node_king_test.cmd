rem @echo off

set OPTS=-noshell

rem erl +W w -mnesia dir '"./ddb"' -name erlang_node_king@localhost %OPTS% -pa app/ebin -s node_king_util test -s init stop

rebar clean eunit skip_deps=true suites=node_king_util

