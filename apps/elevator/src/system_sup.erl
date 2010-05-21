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
%%% File    : system_sup.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Supervisor for the elevator system.
%%% Created : 30 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(system_sup).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(supervisor).

%% External exports
-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(NElevs, EventHandlers) ->
    supervisor:start_link({local, system_sup}, system_sup,
			  [NElevs, EventHandlers]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init([NElevs, EventHandlers])
%%  Starts up the elevator control system with NElevs elevators.
%%  EventHandlers is a list of system event handlers that
%%  should be installed when sys_event starts up.
%%----------------------------------------------------------------------
init([NElevs, EventHandlers]) ->
    {ok,{{type, maxR, maxT},
	 [{scheduler, "..."},
	  {sys_event, "..."},
	  {elev_sup,  "..."}]}}.
