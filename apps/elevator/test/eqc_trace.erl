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

-module(eqc_trace).

-export([behaviour_info/1,
         validate/2, validate/3]).

behaviour_info(callbacks) ->
  [{initial_state,0}, {next_state,2}, {postcondition,2}].

validate(CallBack,Events) ->
  State = CallBack:initial_state(),
  fsm(CallBack,Events,State).

validate(CallBack,Events,InitState) ->
  fsm(CallBack,Events,InitState).

fsm(CallBack,[],State) ->
  true;
fsm(CallBack,[Event|Events],State) ->
  CallBack:postcondition(State,Event) andalso
  fsm(CallBack,Events,CallBack:next_state(State,Event)).


