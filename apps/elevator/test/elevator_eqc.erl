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

-module(elevator_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(eqc_statem).

-export([command/1, initial_state/0, next_state/3, 
         precondition/2, postcondition/3]).

-record(state,{floors, elevators}).

%% starting elevator:
%%  util:start(InitialFloor, NrFloors, NrElevators)
%% stop elevator system:
%% util:stop()

prop_stop_where_requested() ->
   ?FORALL({Floors,Elevators},{choose(1,6),choose(1,4)},
          prop_stop_where_requested(Floors,Elevators)).

prop_stop_where_requested(Floors,Elevators) ->
    ?FORALL(Cmds,commands(?MODULE,initialstate(Floors,Elevators)),
	    begin 
		start(Floors,Elevators),
		{H,S,Res} = run_commands(?MODULE,Cmds),
                Events = collectevents(),
		util:stop(),
                Res==ok andalso 
                eqc_trace:validate(elevator_trace_eqc,Events,
                                   elevator_trace_eqc:initial_state(Elevators))
	    end).



%%%%%%%%%%%%% Generators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

command(S) ->
   frequency([
	      {3,{call,scheduler,f_button_pressed,
                       [choose(1,S#state.floors)]}},
	      {3,{call,scheduler,e_button_pressed,
                        [choose(1,S#state.elevators),
		         choose(1,S#state.floors)]}},
	      {3,{call,?MODULE,wait,[50]}}
             ]).


initial_state() ->
    #state{floors=3,  
           elevators = 2}.

%% I want to be able to initialize the state with a parameter
initialstate(Floors,Elevators) ->
    #state{floors=Floors,  
           elevators = Elevators}.

% Preconditions

precondition(_,_) -> 
	true.

next_state(S,_,_) ->
	S.

postcondition(_,_,_) ->
	true.



%%%%%%%%%%%%%%%%%%%%% commands %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


wait(MSec) ->
    receive
	after MSec ->
		{wait,MSec}
    end.

collectevents() ->
    wait(90),
    gen_event:call(sys_event,handler_eqc,opened).

start(Floors,Elevators) ->
    wait(10),
    util:start(1,Floors,Elevators),
    wait(10),
    gen_event:add_handler(sys_event,handler_eqc,
                                         [1,Floors,Elevators]),
    wait(50).

