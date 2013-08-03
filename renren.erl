-module(renren).
-export([renren_get_visitor_count/2, renren_get_latest_visitor/2, renren_get_latest_visitor/3]).

renren_get_visitor_count(Token, Uid) ->
    case common:http_post(
            "https://api.renren.com/restserver.do",
            lists:concat(["access_token=", Token, "&method=users.getProfileInfo&format=json&v=1.0&fields=visitors_count&uid=", Uid]),
            list_to_atom(Uid)
        ) of
        {ok, {_, _, Body}} -> 
            {J} = jiffy:decode(list_to_binary(Body)),
            common:get_value_from_key(J, <<"visitors_count">>);
        _E -> error
    end.

renren_get_latest_visitor(Token, Uid) ->
    renren_get_latest_visitor(Token, Uid, 10).
renren_get_latest_visitor(Token, Uid, Count) ->
    case common:http_post(
            "https://api.renren.com/restserver.do",
            lists:concat(["access_token=", Token, "&method=users.getVisitors&format=json&v=1.0&count=", integer_to_list(Count)]),
            list_to_atom(Uid)
        ) of
        {ok, {_, _, Body}} ->
            {J} = jiffy:decode(list_to_binary(Body)),
            common:get_value_from_key(J, <<"visitors">>);
        _E -> error
    end.
