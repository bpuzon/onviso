%%% @author  Bartlomiej Puzon <bartlomiej@erlang-solutions.com>
%%% This module helps in logfiles manipulation (fetching, merging).
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
-module(onviso_file_helper). 
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

-export([get_files_to_fetch/3,
	 get_files_to_merge/2,
	 get_trace_setup/3,
	 output_dir/1,
	 purge_logs/0,
	 copy_tag/0]).

-define(COPY_TAG, "copy_").
-define(LOGDIR_TAG, "tracedata_").   %%Don't leave blank

%Returns the list of files to fetch for each node
%Dict is a dictionary of the form {Node, Max_log_seq_number
-spec get_files_to_fetch(list(), integer(), dict()) -> list().
get_files_to_fetch(Nodes, Ref, Dict) ->
    [{Node, [{trace_log, ".", get_logs(trace,
				       Ref,
				       Node,
				       dict:fetch(Node, Dict))},
	     {ti_log, ".", get_logs(ti,
				    Ref,
				    Node,
				    dict:fetch(Node, Dict))}]}
     || Node <- Nodes].


%Returns the list of files to be merged.
-spec get_files_to_merge(integer(), dict()) -> list().
get_files_to_merge(Ref, Counters) ->    
    lists:foldl(fun(Node, Acc) -> % only take the files we can merge
			{Trace, Ti} = extract_per_node(Node,
						       Ref,
						       dict:fetch(Node, Counters)),
			case {Trace, Ti} of
			    {[], []} ->
				Acc;
			    {Trace, Ti} ->
				[ {Node, [{trace_log, Trace},
					  {ti_log, Ti}]} | Acc ]
			end
                end, [], dict:fetch_keys(Counters)).

%Returns a trace specification with proper file data for a given node
%This needs to be formatted: [{Node, [Details]}]
%Details: {TraceType, {file, [FileList]}}
-spec get_trace_setup(atom(), integer(), integer()) -> list().
get_trace_setup(Node, Ref, RunNumber) ->
    [{Node, [{trace, {file, get_filename(trace,
					 Ref,
					 Node,
					 RunNumber)}},
	     {ti, {file, get_filename(ti,
				      Ref,
				      Node,
				      RunNumber)}}]}].

%%Returns the list of files for a single node
-spec extract_per_node(atom(), integer(), integer()) -> {list(), list()}.
extract_per_node(Node, Ref, LogLimit) ->
    TraceFiles = get_logs(trace, Ref, Node, LogLimit),
    TiFiles = get_logs(ti, Ref, Node, LogLimit),
    ThisNode = node(),
    {TraceFiles2, TiFiles2} = case Node of
				  ThisNode ->
				      {TraceFiles, TiFiles};
				  _ ->
				      {add_prefix(TraceFiles, Ref),
				       add_prefix(TiFiles, Ref)}
			      end,
    find_existing(TraceFiles2, TiFiles2, [], []).

-spec find_existing(list(), list(), list(), list()) -> {list(), list()}.                        
find_existing([], [], Trace, Ti) ->
    {lists:reverse(Trace), lists:reverse(Ti)};
find_existing([H1|T1], [H2|T2], Trace, Ti) ->
    case {filelib:is_file(H1), 
	  filelib:is_file(H2)} of
	{true, true} ->
	    find_existing(T1, T2, [H1 | Trace], [H2 | Ti]);
	{false, _} ->
	    find_existing(T1, T2, Trace, Ti)
    end.

%Returns the list of logfies for a given node and reference number
-spec get_logs(atom(), integer(), atom(), integer()) -> list().
get_logs(Type, Ref, Node, LogLimit) ->
    [ get_filename(Type, Ref, Node, Num) || Num <- lists:seq(1, LogLimit) ].

%Creates a logfile name from its description
-spec get_filename(atom(), integer(), atom(), integer()) -> string().
get_filename(Type, Ref, Node, RunNumber) ->    
    atom_to_list(Node) ++ "_" ++ integer_to_list(Ref)
	++ "_" ++ integer_to_list(RunNumber) ++ atom_to_list(Type) ++ ".log".

-spec add_prefix(list(), integer()) -> list().
add_prefix(List, Ref) ->
    lists:map(fun(E) ->
		      filename:join([output_dir(Ref), ?COPY_TAG ++ E])
	      end, List).

-spec output_dir(integer()) -> string().
output_dir(Ref) ->
    ?LOGDIR_TAG ++ integer_to_list(Ref).

-spec copy_tag() -> list().
copy_tag() ->
    ?COPY_TAG.

-spec purge_logs() -> ok.
purge_logs() ->
    os:cmd("rm -rf " ++ ?LOGDIR_TAG ++ "*").
