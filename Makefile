.PHONY: compile check xref

all: check test

check: compile xref

compile: deps
	@rebar compile

test: eunit ct

eunit:
	@rebar skip_deps=true eunit

ct:
	@rebar skip_deps=true ct

erl:
	erl -pa ebin deps/*/ebin

clean:
	@rebar clean

deps:
	@rebar get-deps

xref: compile
	@rebar skip_deps=true xref
