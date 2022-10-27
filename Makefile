REBAR=rebar3
ERL=erl
APP=courier

init: get-deps compile init-dialyzer

compile:
	${REBAR} compile

ct:
	${REBAR} ct

dev:
	ERL_FLAGS=" -args_file config/vm.args " ${REBAR} shell

dialyzer:
	${REBAR} dialyzer

init-dialyzer:
	@dialyzer --build_plt --apps erts kernel stdlib mnesia --output_plt .${APP}.plt

get-deps:
	${REBAR} get-deps
