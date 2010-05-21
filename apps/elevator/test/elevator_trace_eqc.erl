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

-module(elevator_trace_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

-behaviour(eqc_trace).

-export([initial_state/1, next_state/2, postcondition/2]).

-record(state,{fbs=[], elevators=[]}).

initial_state(Elevators) ->
    #state{fbs = [], elevators = [{I,[]} || I<-lists:seq(1,Elevators)]}.

next_state(State, {floor,F}) ->
    add_fb(F,State);
next_state(State, {elevator,E,F}) ->
    add_elev(E,F,State);
next_state(State,{open,E,F}) ->
    rm_open(E,F,State).

postcondition(State,{open,E,F}) ->
    check_open(E,F,State);
postcondition(_State,_Event) ->
    true.


%%%%%%%%%%%% Boolean properties %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% elevator E is opened at floor F, check whether
%% there is a corresponding goal
check_open(E,F,State) ->
   {_,StopList} = lists:nth(E,State#state.elevators),
   lists:member(F,State#state.fbs) orelse
   lists:member(F,StopList).

add_fb(F,State) ->
    State#state{fbs = State#state.fbs++[F]}.

add_elev(E,F,State) ->
    State#state{elevators =
		lists:map(fun({I,SL}) ->
				  case E==I of
				      true -> 
					  {I,SL++[F]};
				      false -> 
					  {I,SL}
				  end 
			  end,State#state.elevators)
              }.


rm_open(Elev,Floor,State) ->
    State#state{fbs = rm_floor(Floor,State#state.fbs),
		elevators = 
		lists:map(fun({I,SL}) ->
				  case I==Elev of
				      true ->
					  {I,rm_floor(Floor,SL)};
                                   false ->
					  {I,SL}
				  end
			  end,State#state.elevators)}.

rm_floor(Floor,Floors) ->
    lists:filter(fun(F) -> F=/=Floor end,Floors).




