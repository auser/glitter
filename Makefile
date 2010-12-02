all: compile

compile:
	@./rebar compile

boot:
	(cd ebin; $(ERL) -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot write_scripts $(APP) $(VERSION);)

test: compile
	@./rebar eunit

clean:
	@./rebar clean
