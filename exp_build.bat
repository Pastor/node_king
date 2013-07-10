@echo off

set OPTS=-noshell


rebar clean eunit skip_deps=true suites=exp_server
rem rebar clean compile && erl +W w -name exp@localhost %OPTS% -pa exp/ebin -s exp_server start -s exp_server ping -s init stop
