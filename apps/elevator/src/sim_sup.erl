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
%%% File    : sim_sup.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Top level supervisor for the elevator simulator system.
%%% Created : 30 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(sim_sup).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-define(display,screen).

-behaviour(supervisor).

%% External exports
-export([start_link/3]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(InitFloor, NFloors, NElevs) ->
    supervisor:start_link({local, sim_sup}, sim_sup,
			  [NElevs, [{?display, [InitFloor, NFloors, NElevs]}]]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init([NElevs, EventHandlers])
%%  Starts up the graphics supervisor and the system supervisor.
%%----------------------------------------------------------------------
init([NElevs, EventHandlers]) ->
    {ok,{{one_for_all, 0, 1},
	 [{g_sup, {g_sup, start_link, []},
	   permanent, infinity, supervisor, [g_sup]},
	  {system_sup, {system_sup, start_link, [NElevs, EventHandlers]},
	   permanent, infinity, supervisor, [system_sup]}]}}.
