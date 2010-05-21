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
%%% File    : elevators.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Elevator application module.
%%% Created : 30 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(elevators).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Start the top-level supervisor with the appropriate values.
%% Not complete.
%%----------------------------------------------------------------------
start(normal, []) ->
    % should obtain InitFloor, NFloors and NElevs from configuration
    % parameters
    InitFloor = 2,
    NFloors = 6,
    NElevs = 3,
    sim_sup:start_link(InitFloor, NFloors, NElevs).

%%----------------------------------------------------------------------
%% Stop has nothing special to do.
%%----------------------------------------------------------------------
stop(_State) ->
    ok.
