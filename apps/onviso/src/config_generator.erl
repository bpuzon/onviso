%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%%% @author  Bartlomiej Puzon <bartlomiej.puzon@erlang-solutions.com>
%%% @doc  The module provides an API to generate configurations.
%%%
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
-module(config_generator).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').
-include("onviso.hrl").

-export([create_trace_case/0,
         create_trace_case/1,
         create_pattern/4, 
         create_flags/2,
         create_merge_conf/0,
	 create_merge_conf/3,
         create_merge_conf/4,
         add_pattern/2,
         add_flags/2,
         add_merge_conf/2,
         add_nodes/2,
         create_fun/1,
	 validate/6,
         validate/5]).

%% @doc Creates an empty trace case with a generated ID. 
%% @end
-spec create_trace_case() -> #trace_case{}.     
create_trace_case() ->
    create_trace_case(none).

%% @doc Creates an empty trace case with a generated ID and a string name. 
%% Note that the string name is only for readability. 
%% @end
-spec create_trace_case(string()) -> #trace_case{}.
create_trace_case(Name) ->
    #trace_case{case_id = undefined,
                name = Name,
                patterns = [],
                nodes = [],
		all_traces = [],
                output = "output.txt",
                timestamp = erlang:now(),
		merge_confs = [],
		trace_opts = []
               }.

%% @doc Create a pattern record. Will perform some basic error validation
%% on the pattern. 
%% @end.
-spec create_pattern(atom(), atom(), list(), term()) -> #pattern{}.
create_pattern(Module, Function, Arity, MatchSpec) ->
    case MatchSpec of
        caller ->
            do_create_pattern(Module, Function, Arity, MatchSpec);
        return ->
            do_create_pattern(Module, Function, Arity, MatchSpec);
	{fun2ms, FunStr} when is_list(FunStr) ->
	    do_create_pattern(Module, Function, Arity, MatchSpec);
        Spec when is_list(Spec) ->
            do_create_pattern(Module, Function, Arity, MatchSpec);
        _Else ->
            {error, invalid_matchspec}
    end.

do_create_pattern(Module, Function, Arity, MatchSpec) ->
    PatternId = undefined, %%onviso_server:get_next_pattern_id(),
    #pattern{id = PatternId,
             module = Module, 
             function = Function, 
             arity = Arity,
             matchspec = MatchSpec}.

%% @doc Create flag record.
%% @end
-spec create_flags(atom(), list()) -> #flags{}.
create_flags(Scope, Flags) ->
    #flags{id = undefined, %%onviso_server:get_next_flag_id(),
           scope = Scope, 
           flags = Flags}.

%% @doc <p>Create merge configuration record with the default settings. 
%% This means collecting all traces and writing them, as they arrive to the 
%% collector node, to file. No manipulation of the trace is conducted. </p>
%% @end
-spec create_merge_conf() -> #merge_conf{}.
create_merge_conf() ->
    create_merge_conf(void, void, file, "").

%% @doc <p>Create merge configuration record without comment. </p>
%% @end
-spec create_merge_conf(function(), function(), function()) -> #merge_conf{}.
create_merge_conf(BeginFun, WorkFun, EndFun) ->
    create_merge_conf(BeginFun, WorkFun, EndFun, "").

%% @doc <p>Create merge configuration record. This record contains three funs which
%% defines the way merging is done. It is important that the <code>workfun</code>'s head
%% matches the trace output. </p>
%% <p>Example <code>workfun</code> heads</p>
%% <pre>
%% fun(Node, {trace_ts, Pid, call, {Module, Function, Arity}, Time}, PidMapping, Data) ->
%%        ...
%%    (Node, {trace_ts, Pid, call, {M, F, A}, CallerFunction, Time}, PidMapping, Data) ->
%%        ...
%%    (Node, {trace_ts, Pid, return_from, From, {M, F, A}, Time}, PidMapping, Data) ->
%%        ... 
%% end
%% </pre>
%% <p>The workfun must always return <code>{ok, NewData}</code>.</p>
%% <p><strong>Important:</strong> If a trace pattern is generated and the 
%% workfun cannot match on any of the heads a runtime error will be generated 
%% when trying to merge. </p>
%% <p><code>Data</code> is the accumulator for the trace case evaluation. By default
%% it is set to <code>[]</code> but can be overridden in the <code>beginfun</code>.</p>
%% <pre>
%% BeginFun = fun([]) ->
%%                {ok, #handlerdata{}}
%%            end.
%% </pre>
%% <p>In the <code>endfun</code> it is possible to do any kind of post-processing 
%% of the accumulated data. The fun must return <code>ok</code> when successful.</p>
%% @end
-spec create_merge_conf(function(), function(), function(), string()) -> #merge_conf{}.
create_merge_conf(BeginFun, WorkFun, EndFun, Comment) ->
    #merge_conf{id=undefined,
		beginfun=BeginFun,
		workfun=WorkFun,
		endfun=EndFun,
		comment=Comment
	       }.

%% @doc Add a pattern to a trace case record. 
%% @end    
-spec add_pattern(#trace_case{}, #pattern{}) -> #pattern{}.
add_pattern(TraceCase, Pattern) ->
    NewPatterns = [Pattern|TraceCase#trace_case.patterns],
    TraceCase#trace_case{patterns=NewPatterns}.

%% @doc Add a flag specification to a trace case record. 
%% @end
-spec add_flags(#trace_case{}, #flags{}) -> #trace_case{}.
add_flags(TraceCase, Flags) ->    
    TraceCase#trace_case{trace_flag = Flags}.

%% @doc Add a merge configuration to a trace case record. 
%% @end
-spec add_merge_conf(#trace_case{}, #merge_conf{}) -> #trace_case{}.   
add_merge_conf(TraceCase, MergeConf) ->
    NewMergeConfs = [MergeConf|TraceCase#trace_case.merge_confs],
    TraceCase#trace_case{merge_confs = NewMergeConfs}.

%% @doc Append nodes to a trace case. 
%% @end
-spec add_nodes(#trace_case{}, list(atom())) -> #trace_case{}.
add_nodes(TraceCase, Nodes) ->
    AddNodes = lists:append(Nodes, TraceCase#trace_case.nodes),
    TraceCase#trace_case{nodes = AddNodes}.

%% @doc Create a fun from a string. 
%% @end
-spec create_fun(string()) -> {ok, function()}.    
create_fun(StringFun) ->
    if length(StringFun) > 1 ->
	   {ok, WithEnd} = ensure_period_ending(StringFun),
	   FunToEval = string:join(["Fun = ", WithEnd], ""),
	   evaluate_fun(FunToEval);
       true ->
	   {ok, void}
    end.

%% @doc <p>Validate a configuration as inputted in a user interface. It is assumed
%% that all data is passed as strings in the following format:</p>
%% <pre>
%% Patterns = [{"module", "function", "arguments", "match specification"}]
%% Nodes = ["node@no-name-host"]
%% Flags = ["scope, [flags]"]
%% Merge = [{beginfun, BeginFun}, {workfun, WorkFun}, {endfun, EndFun}, {comment, Comment}]
%% Options = [{overload, "[]"}, {name, "Easy to read name"}, 
%%            {output_file, "traceoutput.txt"}]
%% TraceOpts = "list()" (see onviso for the possible trace options list
%% </pre>
%% <p>Call validate like this:</p>
%% <pre> validate(Patterns, Nodes, Flags, Merge, Options) </pre>
%% <p>The function will try to validate the input as is and in some cases try 
%% to correct minor mistakes. </p>
%% <p>When an error is present the return could look like this:</p>
%% <pre>
%%     {error, [{ok, [{ok, Pattern}, {ok, Pattern2}]},
%%              {ok, [{ok, Node}]},
%%              {ok, [{ok, Flag}]},
%%              {error, [{ok, {beginfun, BeginFun}},
%%                       {error, {workfun, syntax}},
%%                       {ok, {endfun, EndFun}}]},
%%              {ok, [{ok, Option}]}]}.
%% </pre>
-spec validate(list(), list(), list(), list(), list(), list()) -> {ok, #trace_case{}}.
validate(Patterns, Nodes, Flags, Merge, Options, TraceOpts) ->
    MatchValidators = pair_validators(Patterns, Nodes, Flags, 
				      Merge, Options, TraceOpts),
    CheckedIndividual = lists:foldl(fun ({Validator, Validate}, Validated) ->
					    [Validator(Validate)| Validated]
				    end, [], MatchValidators),
    GeneralSummary = {summary(CheckedIndividual), CheckedIndividual},
    case GeneralSummary of
        {ok, Summary} ->
            {ok, create_complete_trace_case(Summary)};
        Error ->
            Error
    end.

-spec validate(list(), list(), list(), list(), list()) -> {ok, #trace_case{}}.
validate(Patterns, Nodes, Flags, Merge, Options) ->
    validate(Patterns, Nodes, Flags, Merge, Options, "[]").


%% Validators
pair_validators(Patterns, Nodes, Flags, Merge, Options, TraceOpts) ->
    [{fun validate_patterns/1, Patterns},
     {fun validate_nodes/1, Nodes},
     {fun validate_flags/1, Flags},
     {fun validate_merge/1, Merge},
     {fun validate_options/1, Options},
     {fun validate_trace_options/1, TraceOpts}].

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------    

create_complete_trace_case(Summary) ->
    % folds over each element in the list and passes them 
    % to a fun, the input accumulator is a trace case with generated ID.
    lists:foldl(fun add_to_trace_case_record/2,
                create_trace_case(), Summary).

-spec retrieve_merge_confs(list(list())) -> list(#merge_conf{}).
retrieve_merge_confs(Data) ->
    lists:map(fun({merge_conf, ok, List}) ->
		      {beginfun, ok, {beginfun, BeginFun}} = lists:keyfind(beginfun, 1, List),
		      {workfun, ok, {workfun, WorkFun}} = lists:keyfind(workfun, 1, List),
		      {endfun, ok, {endfun, EndFun}} = lists:keyfind(endfun, 1, List),
		      {comment, ok, {comment, Comment}} = lists:keyfind(comment, 1, List),
		      create_merge_conf(BeginFun, WorkFun, EndFun, Comment)
	      end, Data).

add_to_trace_case_record({Type, ok, Data}, Record) ->
    case Type of 
        patterns ->
            Patterns = [create_pattern(M, F, A, MS) || 
                           {pattern, ok, {M, F, A, MS}} <- Data],
            Record#trace_case{patterns = Patterns};
        nodes ->
            Nodes = [Node || {node, ok, Node} <- Data],
            Record#trace_case{nodes = Nodes};        
        flags ->
            [{flag, ok, {Scope, FlagList}}] = Data,
            Record#trace_case{trace_flag = create_flags(Scope, FlagList)};
        merge_confs ->
	    Record#trace_case{merge_confs = retrieve_merge_confs(Data)};
        options ->
            lists:foldl(fun(Option, OptionsRecord) ->
                                  case Option of  
                                      {overload, ok, {overload, Overload}} ->
                                          OptionsRecord#trace_case{
                                            overload = Overload};
                                      {name, ok, {name, Name}} ->
                                          OptionsRecord#trace_case{
                                            name = Name};
                                      {output_file, ok, {output_file, OutFile}} ->
                                          OptionsRecord#trace_case{
                                            output = OutFile}
                                  end
                        end, Record, Data);
	trace_opts ->
	    {trace_opts, Opts} = Data,
	    Record#trace_case{trace_opts = Opts}
    end.

%%

validate_patterns([]) ->
    {patterns, error, no_data};

validate_patterns(Patterns) ->
    do_validate_patterns(Patterns, []).

do_validate_patterns([], Acc) ->
    {patterns, summary(Acc), Acc};
do_validate_patterns([{Module, Function, Arity, MatchSpec}
                      = OrgPattern|Rest], Acc) ->
    NewAcc = case validate_mfa(Module, Function, Arity) of
                 {Mod, Fun, Ari} ->
                     case evaluate_expression(MatchSpec) of
                         {ok, MatchExprs} ->
                             [{pattern, ok, {Mod, Fun, Ari, MatchExprs}}|Acc];
                         _ErrorExpr ->
                             [{pattern, error, {OrgPattern}}|Acc]
                     end;
                 _ ->
                     [{pattern, error, OrgPattern}|Acc]
             end,
    do_validate_patterns(Rest, NewAcc);
do_validate_patterns([Error|Rest], Acc) ->
    do_validate_patterns(Rest, [{pattern, error, Error}|Acc]).

%%
validate_mfa(M, F, "_") ->
    {list_to_atom(M), list_to_atom(F), '_'};
    
validate_mfa(M,F,A) ->
    Ari = case catch list_to_integer(A) of
	      Int when is_integer(Int),
		       Int >= 0 ->
		  Int;
	      _ ->
		  error
	  end,
    case Ari of
	error ->
	    error;
	Ari ->
	    {list_to_atom(M), list_to_atom(F), Ari}
    end.
		  
%% 
validate_nodes([]) ->
    {nodes, error, no_data};

validate_nodes(Nodes) ->
    AtomNodes = lists:foldl(fun(Node, Acc) ->
                                [{node, ok, list_to_atom(Node)}|Acc]
                        end, [], Nodes),

    {nodes, summary(AtomNodes), AtomNodes}.

%%
validate_flags([]) ->
    {flags, error, no_data};

validate_flags(Flags) ->
    F = lists:foldl(fun(Flag, Acc) ->
                            case evaluate_expression(Flag) of
                                {ok, Exprs} ->
                                    [{flag, ok, Exprs}|Acc];
                                {error, _Reason} ->
                                    [{flag, error, Flag}|Acc]
                            end
                    end, [], Flags),

    {flags, summary(F), F}.   

%% 

validate_merge([]) ->
    {merge_confs, error, no_data};

validate_merge(MergeConfs) ->
    do_validate_merges(MergeConfs, []).

do_validate_merges([], Acc) ->
    {merge_confs, summary(Acc), Acc};

do_validate_merges([MergeConf|Rest], Acc) ->
    M = lists:foldl(fun({comment, String}, Acc2) ->
			    [{comment, ok, {comment, String}} | Acc2];
		       ({Type, FunString}, Acc2) ->
                            case create_fun(FunString) of
                                {error, Reason} ->
                                    case FunString of
                                        "void" ->
                                            [{Type, ok, {Type, void}}|Acc2];
                                        "file" ->
                                            [{Type, ok, {Type, file}}|Acc2];
                                        "shell" -> 
                                            [{Type, ok, {Type, shell}}|Acc2];
                                        _ ->
                                            [{Type, error, {Type, Reason}}|Acc2]
                                    end;
                                {ok, Fun} ->
                                    [{Type, ok, {Type, Fun}}|Acc2]
                            end
                    end, [], MergeConf),
    Result = {merge_conf, summary(M), M},
    do_validate_merges(Rest, [Result|Acc]).

%%
validate_options(Options) ->
    do_validate_options(Options, []).

do_validate_options([], Acc) ->
    {options, summary(Acc), Acc};
do_validate_options([{Type, Data}|Rest], Acc) ->
    case Type of
        overload ->
            New = case evaluate_expression(Data) of
                      {ok, Exprs} ->
                          {Type, ok, {Type, Exprs}};
                      {error, Reason} ->
                          {Type, error, {Type, Reason}}
                  end,
            do_validate_options(Rest, [New|Acc]);
        name ->
            do_validate_options(Rest, [{Type, ok, {Type, Data}}|Acc]);
        output_file ->
            do_validate_options(Rest, [{Type, ok, {Type, Data}}|Acc]);
        _ ->
            do_validate_options(Rest, Acc)
    end.

validate_trace_options(TraceOpts) ->
    case evaluate_expression(TraceOpts) of
	{error, Error} ->
	    {trace_opts, error, {trace_opts, Error}};
	{ok, Other} when is_list(Other) ->
	    case validate_trace_options2(Other) of
		ok ->
		    {trace_opts, ok, {trace_opts, Other}};
		{error, Reason} ->
		    {trace_opts, error, {trace_opts, Reason}}
	    end;
	{ok, _} ->
	    {trace_opts, error, {trace_opts, syntax}}
    end.

validate_trace_options2(Options) ->
    Validation = lists:map(fun({autoresume, AOpts}) ->
				   onviso_monitor:validate_options(AOpts);
			      (Other) ->
				   {error, {unknown_options_group, Other}}
			   end, Options),
    Errors = lists:filter(fun(ok) -> false;
			     (_) -> true
			  end, Validation),
    case Errors of 
	[] ->
	    ok;
	Errors ->
	    {error, {traceoptions__errors, Errors}}
    end.
				   

%% 
%% CheckList = [{type, validity, Data}]
summary(CheckList) -> 
    case lists:all(fun({_, Validity, _}) ->
                           Validity == ok
                   end, CheckList) of
        true ->
            ok;
        false ->
            error
    end.


%% 
ensure_period_ending(Fun) ->
    case string:right(Fun, 1) of
        "." ->
            {ok, Fun};
        _ ->
            {ok, string:join([Fun, "."], "")}
    end.

%%
evaluate_fun(StringFun) ->
    {ok, Tokenized, _} = erl_scan:string(StringFun),
    case erl_parse:parse_exprs(Tokenized) of
      {ok, Parsed} ->
	  case erl_eval:exprs(Parsed, []) of
	    {value, Fun, _} ->
		{ok, Fun};
	    _ ->
		{error, evaluation}
	  end;
      _ ->
	  {error, syntax}
    end.

%% 
evaluate_expression(StringData) ->
    VariableName = string:join(["Variable = ", StringData, "."], ""),
    evaluate_fun(VariableName).
    
