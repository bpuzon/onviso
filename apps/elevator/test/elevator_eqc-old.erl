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

-include("../include/eqc.hrl").
-include("../include/eqc_lazy_lists.hrl").
-include("../include/eqc_commands.hrl").

-record(state,{fbs=[], elevators = []}).

%% starting elevator:
%%  util:start(InitialFloor, NrFloors, NrElevators)
%% stop elevator system:
%% util:stop()

prop_stop_where_requested(IFloor,Floors,Elevators) ->
    ?FORALL(Cmds,commands(Floors,Elevators),
	?IMPLIES(valid_seq(Cmds),
	    begin 
		wait(10),
		util:start(IFloor,Floors,Elevators),
		wait(10),
		gen_event:add_handler(sys_event,handler_eqc,
                                          [IFloor,Floors,Elevators]),
		wait(50),
		case run_commands(Cmds) of
		    {ok,Vs} ->
			util:stop(),
			valid_opened(lists:last(Vs),initialstate(Elevators));
		    {error,Results,Reason} ->
			util:stop(),
			?FORALL(Errors,return(Results),
				?WHENFAIL(io:format("~p~n",[Reason]), false))
		end
	    end)).



%% all sequences should end with checking the open sequence
%%
valid_seq([]) ->
   false;
valid_seq(Cmds) -> 
   case lists:last(Cmds) of
        {set,_,gen_event,call,_} -> true;
	_ -> false
   end.


pick_cmds(Floors,Elevators) ->
    pick(commands(Floors,Elevators)).


%%%%%%%%%%%%% Generators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commands(Floors,Elevators) ->
    make_commands(sequence(Floors,Elevators)).


sequence(Floors,Elevators) ->
    frequency([{1,collectevents()},
	       {3,?LAZY(wait(Floors,Elevators))},
	       {3,?LAZY(floorbutton(Floors,Elevators))},
	       {3,?LAZY(elevbutton(Floors,Elevators))}
              ]).


floorbutton(Floors,Elevators) ->
    ?LET(F,choose(1,Floors),
	 ?SET(OK,scheduler,f_button_pressed,[F],
	 sequence(Floors,Elevators))).

elevbutton(Floors,Elevators) ->
    ?LET(F,choose(1,Floors),
    ?LET(E,choose(1,Elevators),
         ?SET(OK,scheduler,e_button_pressed,[E,F],
	 sequence(Floors,Elevators)))).

wait(Floors,Elevators) ->
    ?SET(OK,?MODULE,wait,[50],
    sequence(Floors,Elevators)).

collectevents() ->
    ?SET(OK,?MODULE,wait,[50],
    ?SET(Opened,gen_event,call,[sys_event,handler_eqc,opened], [])).

%%%%%%%%%%%% Boolean properties %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


valid_opened([],State) ->
    true;
valid_opened([{floor,F}|EFs],State) ->
    valid_opened(EFs,add_fb(F,State));
valid_opened([{elevator,E,F}|EFs],State) ->
    valid_opened(EFs,add_elev(E,F,State));
valid_opened([{open,E,F}|EFs],State) ->
    case check_open(E,F,State) of
         true -> 
	    valid_opened(EFs,rm_open(E,F,State));
	 false ->
            false
    end.

%% elevator E is opened at floor F, check whether
%% there is a corresponding goal
check_open(E,F,State) ->
   {_,SL} = lists:nth(E,State#state.elevators),
   lists:member(F,State#state.fbs) orelse
   lists:member(F,SL).



%%%%%% update the state %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initialstate(Elevators) ->
    #state{fbs = [], elevators = [{I,[]} || I<-lists:seq(1,Elevators)]}.

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

wait(MSec) ->
    receive
	after MSec ->
		{wait,MSec}
    end.








%%%%%%%%%%%%%%%%%%%%%% more properties %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




prop_configurations() ->
   IFloor = 1,
       ?FORALL({Floors,Elevators,Cmds},
               {choose(2,6),choose(1,3),commands(6,3)},
          ?IMPLIES(valid_seq(Cmds,IFloor,Floors,Elevators),
	    begin 
		wait(10),
		util:start(IFloor,Floors,Elevators),
		wait(10),
		gen_event:add_handler(sys_event,handler_eqc,
                                          [IFloor,Floors,Elevators]),
		wait(50),
		case run_commands(Cmds) of
		    {ok,Vs} ->
			util:stop(),
			valid_opened(lists:last(Vs),initialstate(Elevators));
		    {error,Results,Reason} ->
			util:stop(),
			?FORALL(Errors,return(Results),
				?WHENFAIL(io:format("~p~n",[Reason]), false))
		end
	    end)).

           
valid_seq([],IFloor,Floors,Elevators) ->
   false;
valid_seq(Cmds,IFloor,Floors,Elevators) -> 
   case lists:last(Cmds) of
        {set,_,gen_event,call,_} -> 
	   lists:all(fun(Cmd) -> valid(Cmd,IFloor,Floors,Elevators) end,Cmds);
	_ -> false
   end.


valid({set,_,_,e_button_pressed,[E,F]},IFloor,Floors,Elevators) ->
    (E=<Elevators) and (F=<Floors);
valid({set,_,_,f_button_pressed,[F]},IFloor,Floors,Elevators) ->
    (F=<Floors);
valid(_,IFloor,Floors,Elevators) ->
    true.





%%%%%%%% junk %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








myseq1(Floors,Elevators) ->
    ?SET(OK,scheduler,e_button_pressed,[2,4],
    ?SET(OK,?MODULE,wait,[200],
    ?SET(OK,scheduler,e_button_pressed,[2,1],
    ?SET(OK,scheduler,e_button_pressed,[2,2],
    ?SET(OK,?MODULE,wait,[choose(20,520)],
         opened(Floors,Elevators)))))).

myseq(Floors,Elevators) ->
    ?SET(OK,scheduler,e_button_pressed,[2,2],
    ?SET(OK,?MODULE,wait,[20],
    ?SET(OK,scheduler,e_button_pressed,[2,1],
    ?SET(OK,scheduler,e_button_pressed,[2,3],
    ?SET(OK,scheduler,f_button_pressed,[1],
    ?SET(OK,?MODULE,wait,[choose(20,520)],
    ?SET(OK,?MODULE,wait,[520],
         opened(Floors,Elevators)))))))).


opened(Floors,Elevators) ->
    ?SET(Opened,?MODULE,events,[],
    ?ASSERT(?MODULE,valid_opened,[Opened,initialstate(Elevators)],[])).
