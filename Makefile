REBAR=rebar3
ERL=erl
APP=courier

init: get-deps compile dialyzer

compile:
	${REBAR} compile

ct:
	${REBAR} ct

dev:
	ERL_FLAGS=" -args_file config/vm.args " ${REBAR} shell

dialyzer:
	${REBAR} dialyzer

get-deps:
	${REBAR} get-deps
