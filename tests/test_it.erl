-module(test_it).

-include_lib("eunit/include/eunit.hrl").

client_starts_test() ->
    Pid = client:start(),
    client:stop(Pid).

client_status_test() ->
    Pid = client:start(),
    Status1 = client:get_status(Pid),
    ?assertEqual([], Status1),
    client:stop(Pid).

client_add_timer_test() ->
    Pid = client:start(),
    client:add_timer(Pid, 1000, "timer1"),
    Status1 = client:get_status(Pid),
    ?assertEqual( [{"timer1", running}], Status1),
    client:stop(Pid).

client_timer_finish_test() ->
    Pid = client:start(),
    client:add_timer(Pid, 10, "timer1"),
    timer:sleep(200),
    Status1 = client:get_status(Pid),
    ?assertEqual([{"timer1", done}], Status1),
    client:stop(Pid).

client_timer_multiple_finish_test() ->
    Pid = client:start(),
    client:add_timer(Pid, 10, "timer1"),
    client:add_timer(Pid, 50, "timer2"),
    timer:sleep(20),
    Status1 = client:get_status(Pid),
    ?assertEqual([{"timer2", running}, {"timer1", done}], Status1),
    client:stop(Pid).

client_timer_finish_then_add_more_test() ->
    Pid = client:start(),
    client:add_timer(Pid, 10, "timer1"),
    timer:sleep(20),
    client:add_timer(Pid, 50, "timer2"),
    Status1 = client:get_status(Pid),
    ?assertEqual([{"timer2", running}, {"timer1", done}], Status1),
    client:stop(Pid).

client_await_1_timer_test() ->
    Pid = client:start(),
    client:add_timer(Pid, 10, "timer1"),
    Status1 = client:await(Pid, "timer1"),
    ?assertEqual(timer_finished, Status1),
    client:stop(Pid).


client_await_2_timers_test() ->
    Pid = client:start(),
    client:add_timer(Pid, 50, "timer1"),
    client:add_timer(Pid, 10, "timer2"),
    Status1 = client:await(Pid, "timer1"),
    Status2 = client:await(Pid, "timer2"),
    ?assertEqual(timer_finished, Status1),
    ?assertEqual(timer_finished, Status2),
    client:stop(Pid).

client_await_nonexistent_test() ->
    Pid = client:start(),
    client:add_timer(Pid, 50, "timer1"),
    Status1 = client:await(Pid, "doesn't exist"),
    ?assertEqual(not_found, Status1),
    client:stop(Pid).
