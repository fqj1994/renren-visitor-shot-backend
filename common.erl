-module(common).
-export([
        get_value_from_key/2, http_post/3, urlencode/1, urlencode_utf8/1,
        skip_first_serveral/2, run/1, run/2, gen_list/1, gen_list/2
    ]).


% URL编码
urlencode(S) when is_binary(S) ->
        urlencode(S, <<>>).
urlencode_utf8(S) when is_list(S) ->
        unicode:characters_to_list(urlencode(unicode:characters_to_binary(S, utf8), <<>>), utf8).

urlencode(<<>>, R) ->
        R;
urlencode(<<H:8,T/binary>>, R) ->
        if
                %字母
                (H >= $a) and ($z >= H) ->
                        urlencode(T, <<R/binary, H:8>>);
                (H >= $A) and ($Z >= H) ->
                        urlencode(T, <<R/binary, H:8>>);
                (H >= $0) and ($9 >= H) ->
                        urlencode(T, <<R/binary, H:8>>);
                %空格
                H =:= 32 ->
                        urlencode(T, <<R/binary, 43:8>>);
                %其他
                true ->
                        Specials = encode_specials(H),
                        urlencode(T, <<R/binary, Specials/binary>>)
        end.

%特殊字符编码
encode_specials(T) ->
        ASC = unicode:characters_to_binary(integer_to_list(T, 16)),
        <<37:8, ASC/binary>>.



http_post(Url, Data, Profile) ->
    httpc:request(post, 
        {
            Url,
            [],
            "application/x-www-form-urlencoded", 
            Data
        },
        [], [], Profile).



get_value_from_key([{Key, Value} | _T], Key) ->
    Value;
get_value_from_key([], _Key) ->
    error;
get_value_from_key([_H | T], Key) ->
    get_value_from_key(T, Key).



skip_first_serveral(List, 0) ->
    List;
skip_first_serveral([_H | T], N) ->
    skip_first_serveral(T, N - 1);
skip_first_serveral([], _N) ->
    [].




run(Cmd) ->
    run(Cmd, 30000).

run(Cmd, Timeout) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status]),
    loop(Port, [], Timeout).

loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Port, Data++NewData, Timeout);
        {Port, {exit_status, 0}} -> Data;
        {Port, {exit_status, _}} -> error
    after Timeout ->
            error
    end.

gen_list([], ANS) ->
    lists:concat(lists:reverse(ANS));
gen_list([{Head} | T], ANS) ->
    U = integer_to_list(get_value_from_key(Head, <<"uid">>)),
    N = urlencode_utf8(unicode:characters_to_list(get_value_from_key(Head, <<"name">>), utf8)),
    R = unicode:characters_to_list(get_value_from_key(Head, <<"headurl">>), utf8),
    H = lists:concat([U, " ", N, " ",R, [10]]),
    gen_list(T, [H | ANS]).

gen_list(T) ->
    gen_list(T, []).


