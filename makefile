compile:
	erlc src/*.erl tests/*.erl

test: compile
	erl -noshell -s test_it test -s init stop

analyze: compile dialyze_src dialyze_tests



dialyze_src:
	dialyzer --add_to_plt -r src
	dialyzer src/*.erl -Wno_unknown --no_check_plt --quiet

dialyze_tests:
	dialyzer --add_to_plt -r tests
	dialyzer tests/*.erl -Wno_unknown --no_check_plt --quiet