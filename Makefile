all: compile eunit dialyze edoc

compile:
	@./rebar3 compile

clean:
	@./rebar3 clean

eunit:
	@./rebar3 do eunit,cover

edoc:
	@./rebar3 edoc

start: compile
	@./rebar3 shell

dialyze: compile
	@./rebar3 dialyzer
