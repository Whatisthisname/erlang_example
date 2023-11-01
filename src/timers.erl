-module(timers).


-record(timer, {ref :: reference(), name::string(), status :: running | done, subscribers = [] :: [gen_statem:from()]}).

-record(state, {timers = [] :: [#timer{}]}).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-behaviour(gen_server).

init(_) ->
    {ok, #state{}}.


-spec state_gather_status_for_client(#state{}) -> client:server_return_status().
state_gather_status_for_client(State) ->
    Answer = 
        lists:map(
            fun(Timer) -> 
                {Timer#timer.name, Timer#timer.status} 
            end, 
            State#state.timers
        ),
    Answer.

-spec state_add_subscriber_to_timers(gen_statem:from(), string(), #state{}) -> #state{}.
state_add_subscriber_to_timers(From, Name, State) ->
    TimerOption = 
        case lists:keyfind(Name, #timer.name, State#state.timers) of
            false -> none;
            Result -> Result
        end,
    case TimerOption of
        none -> 
            gen_statem:reply(From, not_found),
            State;
        TimerRecord ->
            case TimerRecord#timer.status of
                done -> 
                    gen_statem:reply(From, timer_finished),
                    State;
                running ->
                    Add_subscriber_to_timer = 
                        fun (Timer) ->
                            case Timer#timer.name of
                                Name -> Timer#timer{subscribers = [From | Timer#timer.subscribers]};
                                _ -> Timer
                            end
                        end,
                    NewTimers = lists:map(Add_subscriber_to_timer, State#state.timers),
                    State#state{timers = NewTimers}
            end
    end.

-spec state_handle_timer_finished(reference(), #state{}) -> #state{}.
state_handle_timer_finished(Ref, State) ->
    io:fwrite("timer finished: ~p~n", [Ref]),
    Update_timer_status = 
        fun(Timer) -> 
            % find the reference that matches the one that exited
            case Timer#timer.ref of
                % if the reference matches, set the status to done
                Ref -> 
                    lists:foreach(
                        fun(Subscriber) -> 
                            gen_statem:reply(Subscriber, timer_finished)
                        end, 
                        Timer#timer.subscribers
                    ),
                    Timer#timer{status = done};
                % otherwise, do nothing
                _ -> Timer
            end
        end, 
    NewTimers = lists:map(Update_timer_status, State#state.timers),
    State#state{timers = NewTimers}.


handle_call({add_timer, Time, Name}, _From, State) ->
    process_flag(trap_exit, true),
    TimerRef = make_ref(),
    spawn_link(fun() -> timer:sleep(Time), exit(TimerRef) end),
    {reply, ok, State#state{timers = [#timer{ref = TimerRef, name = Name, status = running} | State#state.timers]}};
handle_call(get_status, _From, State) ->
    {reply, state_gather_status_for_client(State), State};
handle_call({subscribe, Name}, From, State) ->
    NewState = state_add_subscriber_to_timers(From, Name, State),
    {noreply, NewState}.

handle_info({'EXIT', _Pid, Ref}, State) ->
    NewState = state_handle_timer_finished(Ref, State),
    {noreply, NewState}.

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.