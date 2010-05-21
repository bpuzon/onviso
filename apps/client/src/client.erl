-module(client).
-compile(export_all).

init() ->
    init(server_node()).
init(Node) ->
    application:start(runtime_tools),
    net_kernel:connect_node(Node).

restart() ->
    init:restart().

server_node() ->
    {ok,HostName} = inet:gethostname(),
    list_to_atom("server@" ++ HostName).


get() ->
    erlang:send({server,server_node()}, {get,self()}),
    receive Data -> Data
    after 1000   -> no_reply
    end.

put(Ting) ->
    erlang:send({server,server_node()}, {put,self(),Ting}),
    receive ok -> ok
    after 1000 -> no_reply
    end.
