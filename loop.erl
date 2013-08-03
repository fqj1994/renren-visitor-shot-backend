-module(loop).
-export([user_keeper/5, user_keeper_mon/5, user_keeper_mon_core/6]).


user_keeper(Token, Uid, Target, LastVal, _LastSleepTime) ->
    inets:start(httpc, [{profile, list_to_atom(Uid)}]),
    {SleepTime, VisitorCount} = try
        NewVisitorCount = renren:renren_get_visitor_count(Token, Uid),
        list_to_atom("m" ++ Uid) ! {update, NewVisitorCount},
        NewSleepTime = case NewVisitorCount of
            LastVal ->
                60;
            _ ->
                30
        end,
        if
            NewVisitorCount >= Target ->
                List = renren:renren_get_latest_visitor(Token, Uid, 7 + NewVisitorCount - Target),
                if 
                    length(List) >= 7 + NewVisitorCount - Target ->
                        VisitorList = common:skip_first_serveral(List, length(List) - 7),
                        publish:draw_graph_and_publish(Token, Uid, Target, VisitorList);
                    true ->
                        publish:publish_status_not_captured(Token, Uid, Target)
                end,
                self() ! stop;
            true ->
                ok
        end,
        {NewSleepTime, NewVisitorCount}
    catch
        _E -> {30, LastVal}
    end,
    receive
        stop ->
            inets:stop(httpc, list_to_atom(Uid)),
            ok
    after SleepTime * 1000 ->
            loop:user_keeper(Token, Uid, Target, VisitorCount, SleepTime)
    end.


user_keeper_mon_core(A, B, C, D, E, {Value, Time}) ->
    receive
        {'EXIT', _Pid, normal} ->
            ok;
        {'EXIT', _Pid, _} ->
            loop:user_keeper_mon(A, B, C, D, E);
        {check, From} ->
            From ! {Value, Time},
            loop:user_keeper_mon_core(A, B, C, D, E, {Value, Time});
        {update, NewValue} ->
            {NewTimeMega, NewTimeSec, _} = erlang:now(),
            NewTime = NewTimeMega * 1000000 + NewTimeSec,
            loop:user_keeper_mon_core(A, B, C, D, E, {NewValue, NewTime})
    end.

user_keeper_mon(A, B, C, D, E) ->
    process_flag(trap_exit, true),
    spawn_link(fun() ->
        register(list_to_atom("p" ++ B), self()),
        loop:user_keeper(A, B, C, D, E) end
    ),
    loop:user_keeper_mon_core(A, B, C, D, E, {0, 0}).


