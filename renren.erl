-module(renren).
-export([renren_get_visitor_count/2, renren_get_latest_visitor/2, renren_get_latest_visitor/3]).
-include("renrenapp.hrl").

renren_get_visitor_count(Token, Uid) ->
    case common:http_post(
            "https://api.renren.com/restserver.do",
            lists:concat(["access_token=", common:urlencode_utf8(Token), "&method=users.getProfileInfo&format=json&v=1.0&fields=visitors_count&uid=", Uid]),
            list_to_atom(Uid)
        ) of
        {ok, {_, _, Body}} -> 
            {J} = jiffy:decode(list_to_binary(Body)),
            is_token_expired(J, Uid),
            case common:get_value_from_key(J, <<"visitors_count">>) of
                error ->
                    error;
                Number ->
                    {ok, Number}
            end;
        _E -> error
    end.

renren_get_latest_visitor(Token, Uid) ->
    renren_get_latest_visitor(Token, Uid, 10).
renren_get_latest_visitor(Token, Uid, Count) ->
    case common:http_post(
            "https://api.renren.com/restserver.do",
            lists:concat(["access_token=", common:urlencode_utf8(Token), "&method=users.getVisitors&format=json&v=1.0&count=", integer_to_list(Count)]),
            list_to_atom(Uid)
        ) of
        {ok, {_, _, Body}} ->
            {J} = jiffy:decode(list_to_binary(Body)),
            is_token_expired(J, Uid),
            case common:get_value_from_key(J, <<"visitors">>) of
                error ->
                    error;
                List ->
                    {ok, List}
            end;
        _E -> error
    end.

do_refresh(Uid, RefreshToken) ->
    case httpc:request(get, {
                "https://graph.renren.com/oauth/token?grant_type=refresh_token&refresh_token=" ++ common:urlencode_utf8(RefreshToken) ++ "&client_id=" ++ ?CLIENT_KEY ++ "&client_secret=" ++ ?CLIENT_SECRET,
                []
            },[], []) of
        {ok, {_, _, Body}} ->
            {J} = jiffy:decode(list_to_binary(Body)),
            case common:get_value_from_key(J, <<"refresh_token">>) of
                error -> error;
                NewRefreshToken ->
                    case common:get_value_from_key(J, <<"access_token">>) of
                        error -> error;
                        NewAccessToken ->
                            list_to_atom("p" ++ Uid) ! {updatetoken, NewRefreshToken, NewAccessToken},
                            ok
                    end
            end;
        _E -> error
    end.


refresh_token(Uid) ->
    list_to_atom("m" ++ Uid) ! {self(), requestrefreshtoken},
    receive
        {refreshtoken, null} ->
            error;
        {refreshtoken, RefreshToken} ->
            do_refresh(Uid, RefreshToken)
    end.

is_token_expired(Response, Uid) ->
    case common:get_value_from_key(Response, <<"error_code">>) of
        error ->
            ok;
        2002 ->
            refresh_token(Uid);
        _ ->
            error
    end.
