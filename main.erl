-module(main).

-export([start/0, start/1]).

start() ->
    start(9991).

start(Port) ->
    inets:start(),
    ssl:start(),
    nets:tcp_server(Port).

