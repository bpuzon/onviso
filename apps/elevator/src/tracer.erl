%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
%%%----------------------------------------------------------------------
%%% File    : tracer.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Text-based event tracer that reports system events.
%%% Created :  4 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(tracer).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init([])
%%  Initializes the event handler.
%%----------------------------------------------------------------------
init([]) ->
    {ok,Name}=application:get_env(elevator,filename),
    {ok, File}=file:open(Name, write),
    {ok, File}.

%%----------------------------------------------------------------------
%% handle_event(Event, [])
%%  Prints info on the event that has occured.
%%  To be implemented...
%%----------------------------------------------------------------------
handle_event(Event, File) ->
    {{Year,Month,Day}, {Hour,Min,Seconds}} = erlang:localtime(),
    io:format(File, "~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hour, Min, Seconds]),
    lists:map(fun(X) ->
                  io:format(File, ",~p", [X])
              end, tuple_to_list(Event)),
    io:format(File, "~n",[]),
    %io:format("~p\n", [Event]),
    {ok, File}.

%%----------------------------------------------------------------------
%% handle_call not used
%%----------------------------------------------------------------------
handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% handle_info not used
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% terminate has nothing to clean up.
%%----------------------------------------------------------------------
terminate(Reason, File) ->
    file:close(File),
    ok.

%%----------------------------------------------------------------------
%% code_change has no state to convert.
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
