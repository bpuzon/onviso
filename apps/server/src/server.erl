-module(server).
-compile(export_all).

start() ->
    application:start(runtime_tools),
    Pid = spawn(?MODULE,loop,[[]]),
    register(server,Pid).

stop() ->
    server ! stop.

loop(Data) ->
    receive
	{put,From,Ting} -> From ! ok,
			   loop([Ting|Data]);
	{get,From}      -> From ! Data,
			   loop(Data);
	stop            -> stopped;
	clear           -> loop([])
    end.
