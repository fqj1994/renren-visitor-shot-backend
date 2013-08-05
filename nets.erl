-module(nets).
-export([
        accept_worker/1, accept_loop/1, tcp_server/1
    ]).

accept_worker(Socket) ->
    case gen_tcp:recv(Socket, 3) of
        {ok, <<1:1/big-unsigned-integer-unit:8,LENID:1/big-unsigned-integer-unit:8, LENTOKEN:1/big-unsigned-integer-unit:8>>} ->
            {ok, UIDB} = gen_tcp:recv(Socket, LENID),
            {ok, TOKENB} = gen_tcp:recv(Socket, LENTOKEN),
            {ok, <<TARGET:4/big-unsigned-integer-unit:8>>} = gen_tcp:recv(Socket, 4),
            UID = binary_to_list(UIDB),
            TOKEN = binary_to_list(TOKENB),
            try
                list_to_atom("m" ++ UID) ! {check, self()},
                gen_tcp:send(Socket, "e")
            catch error:badarg ->
                spawn(fun() -> register(list_to_atom("m" ++ UID), self()), loop:user_keeper_mon(TOKEN, UID, TARGET, 0, 30) end),
                gen_tcp:send(Socket, "o")
            end;
        {ok, <<2:1/big-unsigned-integer-unit:8, LENID:1/big-unsigned-integer-unit:8, _>>} ->
            {ok, UIDB} = gen_tcp:recv(Socket, LENID),
            UID = binary_to_list(UIDB),
            list_to_atom("p" ++ UID) ! stop;
        {ok, <<3:1/big-unsigned-integer-unit:8, LENID:1/big-unsigned-integer-unit:8, _>>} ->
            {ok, UIDB} = gen_tcp:recv(Socket, LENID),
            UID = binary_to_list(UIDB),
            try
                list_to_atom("m" ++ UID) ! {check, self()},
                receive
                    {Value, Time} -> gen_tcp:send(Socket, <<1:1/big-unsigned-integer-unit:8, Value:4/big-unsigned-integer-unit:8, Time:8/big-unsigned-integer-unit:8>>)
                after 1000 ->
                        gen_tcp:send(Socket, <<0:1/big-unsigned-integer-unit:8, 0:12/big-unsigned-integer-unit:8>>)
                end
            catch
                error:_ -> gen_tcp:send(Socket, <<0:1/big-unsigned-integer-unit:8, 0:12/big-unsigned-integer-unit:8>>)
            end;
        {ok, <<4:1/big-unsigned-integer-unit:8, LENID:1/big-unsigned-integer-unit:8, _>>} ->
            {ok, UIDB} = gen_tcp:recv(Socket, LENID),
            UID = binary_to_list(UIDB),
            {ok, <<TARGET:4/big-unsigned-integer-unit:8>>} = gen_tcp:recv(Socket, 4),
            try
                list_to_atom("p" ++ UID) ! {updatetarget, TARGET},
                gen_tcp:send(Socket, "o")
            catch
                error:_ -> gen_tcp:send(Socket, "e")
            end;
        _ ->
            ok
    end,
    gen_tcp:close(Socket).


accept_loop(S) ->
    case gen_tcp:accept(S) of
        {ok, Socket} ->
            spawn(fun () -> nets:accept_worker(Socket) end);
        {error, _} ->
            ok
    end,
    nets:accept_loop(S).

tcp_server(Port) ->
    Opts = [
        {active, false},
        binary,
        {backlog, 256},
        {packet, 0},
        {nodelay, true},
        {reuseaddr, true},
        inet6
    ],
    case gen_tcp:listen(Port, Opts) of
        {ok, S} ->
            nets:accept_loop(S);
        {error, E} ->
            io:format("~p\n", [E])
    end.
