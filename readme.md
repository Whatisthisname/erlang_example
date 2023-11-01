Erlang exam preparation, using Dialyzer for type-checking and eunit for testing.

Implements a small server running different timers that the client can subscribe to or get the status of.

```erlang
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

client_await_timer_test() ->
    Pid = client:start(),
    client:add_timer(Pid, 10, "timer1"),
    Status1 = client:await(Pid, "timer1"),
    ?assertEqual(timer_finished, Status1),
    client:stop(Pid).
```
