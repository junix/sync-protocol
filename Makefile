.PHONY : compile test clean

compile:
	rm -f maxwell_protocol
	./rebar get-deps
	./rebar compile

test: compile
	./rebar eu

clean:
	rm -f ./ebin/* .eunit/* maxwell_protocol
