rem @echo off

set HOME=.

rem dialyzer --build_plt --apps kernel stdlib erts mnesia eunit
rem dialyzer --add_to_plt --apps inets

dialyzer -Wunderspecs --src -I ./app/include ./app/src/node_king_main.erl
rem dialyzer -Wunderspecs --src -I ./app/include ./app/src/node_king_app.erl
rem dialyzer -Wunderspecs --src -I ./app/include ./app/src/node_king_sup.erl
rem dialyzer -Wunderspecs --src -I ./app/include ./app/src/node_king_ddb.erl
rem dialyzer -Wunderspecs --src -I ./app/include ./app/src/node_king_conf.erl

