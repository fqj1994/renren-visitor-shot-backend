-module(publish).
-export([
        draw_graph_and_publish/4,
        draw_graph_and_publish_text/4,
        publish_status_not_captured/3
    ]).


draw_graph_and_publish_text(Token, Uid, Target, VisitorList) ->
    PREFIX = common:urlencode_utf8("恭喜"),
    MIDDLE = common:urlencode_utf8("成为第"),
    SUFFIX = common:urlencode_utf8("个来访。"),
    {P} = lists:nth(1, VisitorList),
    CIRCLE_NAME = common:urlencode_utf8(unicode:characters_to_list(common:get_value_from_key(P, <<"name">>), utf8)),
    CIRCLE_ID = common:urlencode_utf8(integer_to_list(common:get_value_from_key(P, <<"uid">>))),
    case common:http_post(
            "https://api.renren.com/restserver.do",
            lists:concat(["access_token=", Token, "&method=status.set&format=json&v=1.0&status=", PREFIX, "@", CIRCLE_NAME, "(", CIRCLE_ID, ") ", MIDDLE, integer_to_list(Target), SUFFIX]),
            list_to_atom(Uid)
        ) of
        {ok, {_, _, Body}} ->
            {J} = jiffy:decode(list_to_binary(Body)),
            common:get_value_from_key(J, <<"visitors_count">>);
        _E -> error
    end.


draw_graph_and_publish(Token, Uid, Target, VisitorList) ->
    case file:open(Uid ++ ".txt", [write, raw, binary]) of
        {ok, Fd} ->
            file:write(Fd, lists:concat([Uid, "\n", Token, "\n", integer_to_list(Target), "\n", common:gen_list(VisitorList)])),
            common:run("./drawgraph.py " ++ Uid ++ ".txt");
        _E -> error
    end.


publish_status_not_captured(Token, Uid, Target) ->
    %PREFIX = "%E5%8F%88%E6%B2%A1%E6%88%AA%E5%88%B0%E7%AC%AC",
    %SUFFIX = "%E4%B8%AA%E6%9D%A5%E8%AE%BF%E3%80%82%28yl%29",
    %case http_post(
    %        "https://api.renren.com/restserver.do",
    %        lists:concat(["access_token=", Token, "&method=status.set&format=json&v=1.0&status=", PREFIX, integer_to_list(Target), SUFFIX]),
    %        list_to_atom(Uid)
    %    ) of
    %    {ok, {_, _, _Body}} -> 
    %        ok;
    %    _E -> error
    %end.
    ok.


