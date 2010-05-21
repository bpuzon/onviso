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
%%% File    : util.erl
%%% Author  : H�kan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Start functions for the elevator system.
%%% Created :  1 Sep 1999 by H�kan Huss <hakan@erlang.ericsson.se>

-module(util).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-define(display,display).  %% TA addition to allow different outputs
%-define(display,screen). 
%-define(display,eqc). 

-export([start/3, start_eqc/3, start_trace/3, stop/0, start_sup/3,
	 top_proc/4, top_proc_sup/3]).

%%----------------------------------------------------------------------
%% start(IFloor, NFloors, NElevs)
%%  Starts the system without supervision (except for the graphics
%%  portion).
%%----------------------------------------------------------------------
start(IFloor, NFloors, NElevs) ->
    spawn(util, top_proc, [IFloor, NFloors, NElevs, []]).

start_eqc(IFloor, NFloors, NElevs) ->
    spawn(util, top_proc, [IFloor, NFloors, NElevs, [{handler_eqc, [IFloor,NFloors,NElevs]}]]).

%%----------------------------------------------------------------------
%% start_trace(IFloor, NFloors, NElevs)
%%  Starts the system without supervision (except for the graphics
%%  portion). Installs the tracer event handler.
%%----------------------------------------------------------------------
start_trace(IFloor, NFloors, NElevs) ->
    spawn(util, top_proc, [IFloor, NFloors, NElevs, [{tracer, []}]]).

%%----------------------------------------------------------------------
%% start_sup(IFloor, NFloors, NElevs)
%%  Starts the system with supervision.
%%----------------------------------------------------------------------
start_sup(IFloor, NFloors, NElevs) ->
    spawn(util, top_proc_sup, [IFloor, NFloors, NElevs]).

%%----------------------------------------------------------------------
%% stop()
%%  Attempts to stop the system
%%----------------------------------------------------------------------
stop() ->
    case whereis (top_proc) of
	undefined ->
	    ok;
	_ ->
	    top_proc ! stop
    end,
    case whereis (top_proc_sup) of
	undefined ->
	    ok;
	_ ->
	    top_proc_sup ! stop
    end,
    stopped.

%%----------------------------------------------------------------------
%% top_proc(IFloor, NFloors, NElevs, Handlers)
%%  Starts and links to the important processes, then blocks until it
%%  gets a stop messsage.
%%----------------------------------------------------------------------
top_proc(IFloor, NFloors, NElevs, Handlers) ->
    register(top_proc, self()),
    os:putenv("eqctimeout","200"),
    {ok, SPid} = scheduler:start_link(),
    sys_event:start_link([{?display, [IFloor, NFloors, NElevs]}] ++ Handlers),
    EPids = lists:foreach(fun (ENo) ->
				  {ok, EPid} = elevator:start_link(ENo)
			  end,
			  lists:seq(1, NElevs)),
    block().

%%----------------------------------------------------------------------
%% top_proc(IFloor, NFloors, NElevs, Handlers)
%%  As above, but with supervision.
%%----------------------------------------------------------------------
top_proc_sup(IFloor, NFloors, NElevs) ->
    register(top_proc_sup, self()),
    sim_sup:start_link(IFloor, NFloors, NElevs),
    block().

block() ->
    receive
	stop ->
	    exit(shutdown)
    end.
