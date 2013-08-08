-module(main).

-export([start/0, start/1, show_list/0]).

start() ->
    start(9991).

start(Port) ->
    inets:start(),
    ssl:start(),
    spawn(fun() -> loop:user_keeper_global_mon() end),
    nets:tcp_server(Port).

process_and_output_users_list([]) ->
    ok;
process_and_output_users_list([Head | Tail]) ->
    case Head of
        {Uid, Target, Current, Time, Access, Refresh} ->
            io:format("|~s\t|~p\t|~p\t\t|~p\t|~p|~p|\n", [Uid, Target, Current, Time, Access, Refresh]);
        {Uid} ->
            io:format("|~s\t|\n", [Uid])
    end,
    process_and_output_users_list(Tail).

show_list() ->
    global_mon ! {list, self()},
    receive
        List ->
            io:format("|Uid\t\t|Target\t|CurrentVal\t|Timestamp\t|\n"),
            process_and_output_users_list(List)
    end.
