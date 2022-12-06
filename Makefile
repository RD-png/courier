REBAR=rebar3
ERL=erl
APP=courier

init: get-deps compile

# Verbose
compile-v: dialyzer xref compile

compile:
	${REBAR} compile

ct:
	${REBAR} ct

dev:
	ERL_FLAGS=" -args_file config/vm.args " ${REBAR} shell

dialyzer:
	${REBAR} dialyzer

xref:
	${REBAR} xref

get-deps:
	${REBAR} get-deps
