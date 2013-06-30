rem @echo off

set HOME=.

rem dialyzer --build_plt --apps kernel stdlib erts mnesia eunit
rem dialyzer --add_to_plt --apps inets

dialyzer -Wunderspecs --src -I app/include *.erl

