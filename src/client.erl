-module(client).


%% public API
-export([start/0, stop/1, add_timer/3, get_status/1, await/2]).

-spec start() -> pid().
start() -> 
    case gen_server:start(timers, {}, []) of
        {ok, Pid} -> Pid;
        {error, Reason} -> throw(Reason)
    end.

-spec stop(pid()) -> ok.
stop(Pid) -> gen_server:stop(Pid).

-spec add_timer(pid(), number(), string()) -> ok.
add_timer(Pid, Time, Message) -> gen_server:call(Pid, {add_timer, Time, Message}).


-type server_return_status() :: [{string(), running | done}].
-spec get_status(pid()) -> server_return_status().
get_status(Pid) -> gen_server:call(Pid, get_status).

-spec await(pid(), string()) -> timer_finished | not_found.
await(Pid, Name) ->
    gen_server:call(Pid, {subscribe, Name}).
