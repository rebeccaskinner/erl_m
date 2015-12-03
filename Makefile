.PHONY: clean all start

all: compile

compile:
	rebar compile

clean:
	rebar clean

start: compile
	erl -pa ebin deps/*/ebin -eval "application:ensure_all_started(erl_m, permanent)."
