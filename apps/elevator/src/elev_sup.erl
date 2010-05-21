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
%%% File    : elev_sup.erl
%%% Author  : Håkan Huss <haken@erlang.ericsson.se>
%%% Purpose : Elevator process supervisor.
%%% Created : 28 Aug 1999 by Håkan Huss <haken@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(elev_sup).
-author('haken@erlang.ericsson.se').
-vsn("1.0").

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(NElevs) ->
    supervisor:start_link({local, elev_sup}, elev_sup, NElevs).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init
%%  To be completed.
%%----------------------------------------------------------------------
init(NElevs) ->
    ChildList = make_child_specs(NElevs),
    SupFlags = "...",
    {ok,{SupFlags, ChildList}}.

%%----------------------------------------------------------------------
%% make_child_specs(N)
%%  Returns a list of child specifications for N elevator control
%%  processes. Use the number (N) as the child name.
%%  To be implemented.
%%----------------------------------------------------------------------
make_child_specs(N) ->
    "...".
