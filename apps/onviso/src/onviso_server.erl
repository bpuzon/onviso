%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%% @doc <p>This module together with {@module config_generator} is the foundation 
%% of a configuration back-end layer for higher-level tools which want to make 
%% use of the Onviso. The {@module onviso_server} acts as a middle man between any 
%% user-interface and the wrapper, and offers a possibility to store and load
%% configurations to and from files. </p>
%% <p>For a full example of how these two modules can be used, please refer to 
%% the {@module cli} module.
%% </p>
%%% Version: 1.0

%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met: 
%%% * Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution. 
%%% * Neither the name of the Erlang Training & Consulting nor the names of its
%%% contributors may be used to endorse or promote products
%%% derived from this software without specific prior written permission.
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-module(onviso_server).
-behaviour(gen_server).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

%% Api towards onviso (all take own ref as argument)
-export([run_trace/1, run_trace/2,
         stop_trace/1,
         merge_trace/2, merge_trace/3]).

%% Api for managing trace cases
-export([add_trace_case/1,
	 add_pattern/1,
	 add_flag/1,
	 add_merge_conf/1,
         get_trace_cases/0,
         get_patterns/0,
         get_flags/0, 
         get_merge_configurations/0,
         get_trace_case/1,
         get_pattern/1, 
         get_flag/1,
         get_merge_conf/1,
         delete_trace_case/1,
         delete_pattern/1,
         delete_flag/1, 
         delete_merge_conf/1,         
         save_to_file/1,
         read_from_file/1]).

%% Gen server API
-export([start_link/0,
         stop/0,
         init/1,
         handle_call/3,
         handle_cast/2, 
         handle_info/2, 
         code_change/3,
         terminate/2]).

-include("onviso.hrl").

-define(SERVER, ?MODULE).

-record(state, {trace_cases,
                patterns, 
                flags, 
                merge_confs}).

-type merge_fail() :: {error,
		       unknown_trace_execution |
		       unknown_merge |
		       no_trace_running}.

%%------------------------------------------------------------------------------
%% user control api
%%------------------------------------------------------------------------------

%% @doc Run one of the stored trace cases.
%% @end
-spec run_trace(integer()) -> ok.    
run_trace(Ref) ->
    run_trace(Ref, "").

%% @doc Run one of the stored trace cases, providing a comment.
%% @end
-spec run_trace(integer(), string()) -> ok.    
run_trace(Ref, Comment) ->
    case get_trace_case(Ref) of
        #trace_case{} = Case ->
            gen_server:call(?SERVER, {run_trace, Case, Comment});
        false ->
            {error, no_trace}
    end.

%% @doc Stop a trace
%% @end
-spec stop_trace(integer()) -> ok.
stop_trace(Ref) ->                          
    case get_trace_case(Ref) of
	#trace_case{trace_ref = []} ->
	    ok;
        #trace_case{trace_ref = OnvisoRef} ->
            gen_server:call(?SERVER, {stop_trace, OnvisoRef});
        _Else ->
            ok
    end.    

%% @doc Merge a stopped or running trace case. This function will merge the most
%% recent trace execution. If a trace is still running, then when 
%% this function is called, the trace will be stopped automatically. 
%% <p>The function requires the id of a merge to run</p>
%% @end
-spec merge_trace(integer(), integer()) -> ok | merge_fail().
merge_trace(Ref, MergeId) ->
    merge_trace(Ref, -1, MergeId).

%% @doc Merge a stopped or running trace case.
%% If it is still running, then when 
%% this function is called, the trace will be stopped automatically. 
%% <p>The function requires the id of a merge to run and an id of
%% a trace execution</p>
%% @end

-spec merge_trace(integer(), integer(), integer()) -> ok | merge_fail().
merge_trace(Ref, RunRef, MergeId) ->
    stop_trace(Ref),
    case get_trace_case(Ref) of
	#trace_case{} = Case ->
	    Merges = Case#trace_case.merge_confs,
	    Runs = Case#trace_case.all_traces,
	    %%Check if the selected run reference and the selected merge reference
	    %%are valid and run the merge
	    RunRef2 = case RunRef of
			  -1 ->
			      element(1, hd(Runs));
			  RunRef ->
			      RunRef
		      end,
	    case lists:keymember(RunRef2, 1, Runs) of
		true ->
		    case lists:keyfind(MergeId, 2, Merges) of
			false ->
			    {error, unknown_merge};
			MergeConf ->
			    gen_server:call(?SERVER, {merge_trace, RunRef2, MergeConf})
		    end;
		false ->
		    {error, unknown_trace_execution}
	    end;
	_ ->
	    {error, no_trace_running}
    end.
    

%%------------------------------------------------------------------------------
%% user api
%%------------------------------------------------------------------------------
-spec add_trace_case(#trace_case{}) -> ok.
add_trace_case(TraceCase) ->
    NewPatterns = [add_pattern(Pattern) || Pattern <- TraceCase#trace_case.patterns],
    NewFlag = add_flag(TraceCase#trace_case.trace_flag),
    NewMergeConfs = [add_merge_conf(Conf) || Conf <- TraceCase#trace_case.merge_confs],
    NewTraceCase = TraceCase#trace_case{trace_flag = NewFlag,
				        patterns = NewPatterns,
					merge_confs = NewMergeConfs},
    gen_server:call(?SERVER, {add_trace_case, NewTraceCase}).

-spec add_pattern(#pattern{}) -> ok.
add_pattern(#pattern{id = undefined} = Pattern) ->
    gen_server:call(?SERVER, {add_pattern, Pattern});
add_pattern(Pattern) ->
    Pattern.

-spec add_flag(#flags{}) -> ok.
add_flag(#flags{id = undefined} = Flag) ->
    gen_server:call(?SERVER, {add_flag, Flag});
add_flag(Flag) ->
    Flag.
		 
-spec add_merge_conf(#merge_conf{}) -> ok.
add_merge_conf(#merge_conf{id = undefined} = MergeConf) ->
    gen_server:call(?SERVER, {add_merge_conf, MergeConf});
add_merge_conf(MergeConf) ->
    MergeConf.


%% ---- Getters
-spec get_trace_cases() -> list(#trace_case{}).
get_trace_cases() ->
    gen_server:call(?SERVER, get_trace_cases).

-spec get_patterns() -> list(#pattern{}).
get_patterns() ->
    gen_server:call(?SERVER, get_patterns).

-spec get_flags() -> list(#flags{}).
get_flags() ->
    gen_server:call(?SERVER, get_flags).

-spec get_merge_configurations() -> list(#merge_conf{}).
get_merge_configurations() ->
    gen_server:call(?SERVER, get_merge_configurations).

-spec get_trace_case(integer()) -> #trace_case{}.
get_trace_case(Reference) ->    
    gen_server:call(?SERVER, {get_trace_case, Reference}).

-spec get_pattern(integer()) -> #pattern{}.
get_pattern(Reference) ->    
    gen_server:call(?SERVER, {get_pattern, Reference}).

-spec get_flag(integer()) -> #flags{}.
get_flag(Reference) ->    
    gen_server:call(?SERVER, {get_flag, Reference}).

-spec get_merge_conf(integer()) -> #merge_conf{}.
get_merge_conf(Reference) ->    
    gen_server:call(?SERVER, {get_merge_conf, Reference}). 

%% ---- Delete
-spec delete_trace_case(integer()) -> ok.    
delete_trace_case(Reference) ->
    gen_server:call(?SERVER, {delete_trace_case, Reference}).

-spec delete_pattern(integer()) -> ok.    
delete_pattern(Reference) ->
    gen_server:call(?SERVER, {delete_pattern, Reference}).

-spec delete_flag(integer()) -> ok.    
delete_flag(Reference) ->
    gen_server:call(?SERVER, {delete_flag, Reference}).

-spec delete_merge_conf(integer()) -> ok.    
delete_merge_conf(Reference) ->
    gen_server:call(?SERVER, {delete_merge_conf, Reference}).


%% ---- File management
%% @doc Save current configuration to a file. The file will be stored in a 
%% binary format. 
%% @end
-spec save_to_file(string()) -> {ok, integer()}.   
save_to_file(FileName) ->
    gen_server:call(?SERVER, {save_conf_to_file, FileName}).

%% @doc Read a configuration file as previously saved by 
%% {@link save_to_file/1. 'save_to_file/1'}.
%% @end
-spec read_from_file(string()) -> {ok, list()}.   
read_from_file(FileName) ->
    gen_server:call(?SERVER, {read_conf_from_file, FileName}).


%%------------------------------------------------------------------------------
%% gen_server exports
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

-spec init(term()) -> {ok, #state{}}.
init(_Args) ->
    State = #state{trace_cases = [],
                   patterns = [], 
                   flags = [],
                   merge_confs = []},
    {ok, State}.

%% ---- Gen server calls
-spec handle_call(term(), pid(), #state{}) -> {reply, term(), #state{}}.
%% control handlers
handle_call({run_trace, Case, Comment}, _From, State) ->
    Flags = Case#trace_case.trace_flag,
    
    Patterns = [{Mod, Fun, Ari, MS} || 
                   #pattern{module = Mod,
                            function = Fun,
                            arity = Ari, 
                            matchspec = MS} <- Case#trace_case.patterns],

    Reply = case onviso:trace(Patterns,
                              Case#trace_case.nodes,
                              {Flags#flags.scope, Flags#flags.flags},
                              Case#trace_case.overload,
			      Case#trace_case.trace_opts) of
                {ok, Ref} ->
		    Traces = Case#trace_case.all_traces,
                    NewCase = Case#trace_case{
				trace_ref = Ref,
				all_traces = [{Ref,
					       calendar:now_to_local_time(erlang:now()),
					       Comment} | Traces]
					     },
                    spawn(?MODULE, add_trace_case, [NewCase]), % unsafe?
                    %add_trace_case(NewCase), 
                    ok;
                Error ->
                    Error
            end,
    
    {reply, Reply, State};

handle_call({stop_trace, OnvisoRef}, _From, State) ->
    onviso:stop(OnvisoRef),
    {reply, ok, State};

handle_call({merge_trace, CaseRef, MergeConf}, _From, State) ->
    %%The API ensures that Case and MergeConf are linked
    Reply = onviso:merge(CaseRef,
                         MergeConf#merge_conf.beginfun,
                         MergeConf#merge_conf.workfun, 
                         MergeConf#merge_conf.endfun),
    {reply, Reply, State};    

%% file handlers
handle_call({read_conf_from_file, FileName}, _From, State) ->
    {Reply, NewState} = case file:read_file(FileName) of
                            {ok, BinaryData} ->
                                case sanity_check(binary_to_term(BinaryData)) of
                                    {Reason, []} ->
                                        {Reason, State}; % original state if error
                                    {ok, Data} ->
					%%Remove trace runs without data
					TraceCases = Data#state.trace_cases,
					Filtered = lists:map(fun validate_runs/1,
							     TraceCases),
                                        {ok, Data#state{trace_cases = Filtered}}
                                end;                        
                            Reason ->
                                {Reason, State}
                        end,
    
    {reply, build_readable_reply({Reply, NewState}), NewState};

handle_call({save_conf_to_file, FileName}, _From, State) ->
    Reply = case file:open(FileName, [write]) of
		{ok, FD} ->
		    SerialisedData = term_to_binary(State),
		    
		    file:write(FD, SerialisedData),
		    file:close(FD),
		    
		    {ok, size(SerialisedData)};            
		Reason ->
		    Reason
	    end,
    {reply, Reply, State};

%% get handlers
handle_call(get_trace_cases, _From, State) ->
    {reply, State#state.trace_cases, State};

handle_call(get_patterns, _From, State) ->
    {reply, State#state.patterns, State};

handle_call(get_flags, _From, State) ->
    {reply, State#state.flags, State};

handle_call(get_merge_configurations, _From, State) ->
    {reply, State#state.merge_confs, State};

handle_call({get_trace_case, Ref}, _From, State) ->
    {reply, lists:keyfind(Ref, 2, State#state.trace_cases), State};

handle_call({get_pattern, Ref}, _From, State) ->
    {reply, lists:keyfind(Ref, 2, State#state.patterns), State};

handle_call({get_flag, Ref}, _From, State) ->
    {reply, lists:keyfind(Ref, 2, State#state.flags), State};

handle_call({get_merge_conf, Ref}, _From, State) ->
    {reply, lists:keyfind(Ref, 2, State#state.merge_confs), State};

%% set handlers
handle_call({add_trace_case, TraceCase}, _From, State) ->
    NewCase = case TraceCase#trace_case.case_id of
		  undefined ->
		      NextId = length(State#state.trace_cases) + 1,
		      TraceCase#trace_case{case_id = NextId};
		  _ ->
		      TraceCase
	      end,

    NewCases = lists:keystore(TraceCase#trace_case.case_id, 2,
			      State#state.trace_cases, NewCase),
    NewState = State#state{trace_cases=NewCases},

    {reply, NewCase, NewState};

handle_call({add_pattern, #pattern{} = Pattern}, _From, State) ->
    NextId = length(State#state.patterns) + 1,
    Pattern2 = Pattern#pattern{id = NextId},
    NewPatterns = [Pattern2 | State#state.patterns],
    {reply, Pattern2, State#state{patterns = NewPatterns}};

handle_call({add_flag, #flags{} = Flag}, _From, State) ->
    NextId = length(State#state.flags) + 1,
    Flag2 = Flag#flags{id = NextId},
    NewFlags = [Flag2 | State#state.flags],
    {reply, Flag2, State#state{flags=NewFlags}};

handle_call({add_merge_conf, #merge_conf{} = MergeConf}, _From, State) ->
    NextId = length(State#state.merge_confs) + 1,
    MergeConf2 = MergeConf#merge_conf{id = NextId},
    NewMergeConfs = [ MergeConf2 | State#state.merge_confs],
    {reply, MergeConf2, State#state{merge_confs = NewMergeConfs}};

%% delete handlers
handle_call({delete_trace_case, Ref}, _From, State) ->
    NewList = lists:keydelete(Ref, 2, State#state.trace_cases),
    NewState = State#state{trace_cases = NewList},
    {reply, ok, NewState};

handle_call({delete_pattern, Ref}, _From, State) ->
    NewList = lists:keydelete(Ref, 2, State#state.patterns),
    NewState = State#state{patterns = NewList},
    {reply, ok, NewState};

handle_call({delete_flag, Ref}, _From, State) ->
    NewList = lists:keydelete(Ref, 2, State#state.flags),
    NewState = State#state{flags = NewList},
    {reply, ok, NewState};

handle_call({delete_merge_conf, Ref}, _From, State) ->
    NewList = lists:keydelete(Ref, 2, State#state.merge_confs),
    NewState = State#state{merge_confs = NewList},
    {reply, ok, NewState};


%% normal stop
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% ---- Gen server casts
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% ---- Gen server info
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% ---- Gen server other
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% helper functions
%%------------------------------------------------------------------------------
-spec validate_runs(#trace_case{}) -> #trace_case{}.
validate_runs(TraceCase) ->
    Runs = TraceCase#trace_case.all_traces,
    Runs2 = lists:filter(fun({RunRef, _, _}) ->
				 case onviso:status(RunRef) of
				     {ok, _} ->
					 true;
					{error, no_trace_running} ->
					 false
				 end
			 end, Runs),
    TraceCase#trace_case{all_traces = Runs2}.

sanity_check(#state{trace_cases = TraceCases,
                    patterns = Patterns,
                    flags = Flags,
                    merge_confs = MergeConfs} = State) ->
    case {sanity_trace_cases(TraceCases),
          sanity_patterns(Patterns), 
          sanity_flags(Flags),
          sanity_merge_confs(MergeConfs)} of
        {ok, ok, ok, ok} ->
            {ok, State};
        _ ->
            {{error, file_corrupted}, []}
    end;
sanity_check(_Data) ->
    {{error, file_corrupted}, []}.

%%
sanity_trace_cases([]) ->
    ok;
sanity_trace_cases([#trace_case{}|Rest]) ->
    sanity_trace_cases(Rest).

%%
sanity_patterns([]) ->
    ok;
sanity_patterns([#pattern{}|Rest]) ->
    sanity_patterns(Rest).

%%
sanity_flags([]) ->
    ok;
sanity_flags([#flags{}|Rest]) ->
    sanity_flags(Rest).

%%
sanity_merge_confs([]) ->
    ok;
sanity_merge_confs([#merge_conf{}|Rest]) ->
    sanity_merge_confs(Rest).

%%
build_readable_reply({ok, #state{trace_cases = TraceCases,
                                 patterns = Patterns,
                                 flags = Flags,
                                 merge_confs = MergeConfs}}) ->
    {ok, [{trace_cases, length(TraceCases)},
          {patterns, length(Patterns)},
          {flags, length(Flags)},
          {merge_configurations, length(MergeConfs)}]};
build_readable_reply({Reason, _}) ->
    Reason.
