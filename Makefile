REBAR=rebar3
ERL=erl

compile:
	${REBAR} compile

dev:
	ERL_FLAGS=" -args_file config/vm.args " ${REBAR} shell
