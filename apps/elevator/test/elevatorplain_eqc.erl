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

-module(elevatorplain_eqc).

-compile(export_all).

-include("q:/Development/eqc/eqc-project/include/eqc.hrl").
-include("q:/Development/eqc/eqc-project/include/eqc_statem.hrl").

-record(trace,{fbs=[], elevators=[]}).

%% starting elevator:
%%  util:start(InitialFloor, NrFloors, NrElevators)
%% stop elevator system:
%% util:stop()

prop_stop_where_requested(Floors,Elevators) ->
    ?FORALL(Cmds,list(command(Floors,Elevators)),
	    begin 
		wait(10),
		util:start(1,Floors,Elevators),
		wait(10),
		gen_event:add_handler(sys_event,handler_eqc,
                                          [1,Floors,Elevators]),
		wait(50),
                Result = eval(Cmds),
		wait(7*Floors*Elevators),
                Trace = collectevents(),                
		util:stop(),
		?WHENFAIL(io:format("~p~n",[Trace]), 
                          valid_opened(Trace,inittrace(Elevators)))
	    end).

prop_stop_where_requested() ->
   ?FORALL({Floors,Elevators},{choose(1,10),choose(1,4)},
           prop_stop_where_requested(Floors,Elevators)).


%%%%%%%%%%%%% Generators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% hard to ensure that I always add something to begin or end or 
%% sequence. Only possibility is to have state side effect

command(Floors,Elevators) ->
   	 frequency([
	            {1,{call,?MODULE,wait,[50]}},
	            {1,{call,scheduler,f_button_pressed,
                        [choose(1,Floors)]}},
	            {1,{call,scheduler,e_button_pressed,
                        [choose(1,Elevators), choose(1,Floors)]}}
                   ]).


%%%%%%%%%%%% Boolean properties %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

valid_opened([],State) ->
    true;
valid_opened([{floor,F}|EFs],State) ->
    valid_opened(EFs,add_fb(F,State));
valid_opened([{elevator,E,F}|EFs],State) ->
    valid_opened(EFs,add_elev(E,F,State));
valid_opened([{open,E,F}|EFs],State) ->
    check_open(E,F,State) andalso valid_opened(EFs,rm_open(E,F,State)).

%% elevator E is opened at floor F, check whether
%% there is a corresponding goal
check_open(E,F,State) ->
   {_,SL} = lists:nth(E,State#trace.elevators),
   lists:member(F,State#trace.fbs) orelse
   lists:member(F,SL).



inittrace(Elevators) ->
    #trace{fbs = [], elevators = [{I,[]} || I<-lists:seq(1,Elevators)]}.

add_fb(F,State) ->
    State#trace{fbs = State#trace.fbs++[F]}.

add_elev(E,F,State) ->
    State#trace{elevators =
		lists:map(fun({I,SL}) ->
				  case E==I of
				      true -> 
					  {I,SL++[F]};
				      false -> 
					  {I,SL}
				  end 
			  end,State#trace.elevators)
              }.


rm_open(Elev,Floor,State) ->
    State#trace{fbs = rm_floor(Floor,State#trace.fbs),
		elevators = 
		lists:map(fun({I,SL}) ->
				  case I==Elev of
				      true ->
					  {I,rm_floor(Floor,SL)};
                                   false ->
					  {I,SL}
				  end
			  end,State#trace.elevators)}.

rm_floor(Floor,Floors) ->
    lists:filter(fun(F) -> F=/=Floor end,Floors).



%%%%%%%%%%%%%%%%%%%%% commands %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


wait(MSec) ->
    receive
	after MSec ->
		{wait,MSec}
    end.

collectevents() ->
    gen_event:call(sys_event,handler_eqc,opened).


