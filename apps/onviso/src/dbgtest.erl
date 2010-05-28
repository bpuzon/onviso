%%%-------------------------------------------------------------------
%%% @author  Bartłomiej Puzoń <bartlomiej.puzon@erlang-solutions.com>
%%%
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
%%This file contains routines to test how to implement dbg:start_trace
%%using other dbg calls
-module(dbgtest).
-compile(export_all).


%%INFO
%%The existing dbg features will be used as shown below to create the inviso-like
%%behaviour.
%%dbg:run_trace(Patterns, Nodes, Flags, Options) -> {ok, TraceHandler}
%%Patterns: [{M, F, A, MS}], the usual wildcards will be supported
%%MS: can be a proper MS, predefined atoms (return, caller) or a string with
%%a fun literal that will be transformed to a MS (as in dbg:fun2ms)
%%Nodes is a list of nodes on which to run the trace
%%Flags is a list of flags to use (note: timestamp flags is always added)
%%Options: With Options, additional flags can be passed. As for #3666,
%%this includes setting how the trace data should be processed. One can:
%% -use the file port (default), which will dump all trace data to files
%%  on the remote nodes and fetch them when the trace will stop.
%% -use the ip port - the data will be stored to files on the local node 
%%   log wrapping may be turned on (dumping to a unwrapped log is default)
%%   additionaly, a special handler can be specified to deal with trace data streams
%%   [I think the only advantage here is that we can save up some disk. OR we want
%%   to fire an action during trace, e.g. if a threshold has been reached?]
%% -use custom handler (this will add overhead, as trace ports are not used)
%%Each trace creates a new folder where all trace data is kept. In the folder,
%%trace setup data is stored for further reference. Folder names are used as TraceHandlers,
%%but this is not a concern of the user.
%% 
%%dbg:stop_trace(Handler) -> ok
%%Stops tracing. All trace_clients are stopped and the data is fetched.
%%
%%dbg:info_trace(Handler) -> {ok, List}
%%This will retrieve the details on a given trace

%%Note the timestamp flag, will be essential during the merge
t() ->
    trace_opts().

trace_opts() ->
    [
     [{client, get, 0, [{'_',[], [{return_trace}]}]},
      {server, loop, '_', [{'_',[],[{return_trace}]}]}
],
     ['client@ariel', 'server@ariel'],
     {all, [call, timestamp]},
     [{data_handler, ip}, {trace_dir, "trace"}]
    ].
    

%%If we want to use "process" handler, we still can use 
run_trace_process() ->
    [Patterns, Nodes, Flags, _] = trace_opts(),
    dbg:tracer(node(), process, get_handler_spec()),
    [dbg:n(Node) || Node <- Nodes ],
    erlang:apply(dbg, p, tuple_to_list(Flags)),
    [erlang:apply(dbg, tpl, tuple_to_list(Pattern)) || Pattern <- Patterns].

%%This uses file port. The data goes directly to the port
run_trace_file() ->
    [Patterns, Nodes, Flags, _] = trace_opts(),
    dbg:tracer(),
    [dbg:tracer(Node, port, tport(Node)) || Node <- Nodes ],
    erlang:apply(dbg, p, tuple_to_list(Flags)),
    [erlang:apply(dbg, tpl, tuple_to_list(Pattern)) || Pattern <- Patterns].


%%This uses ip port
run_trace() ->
    [Patterns, Nodes, Flags, _] = trace_opts(),
    dbg:tracer(),
    dbg:tracer(hd(Nodes), port, dbg:trace_port(ip, 28882)),
    dbg:tracer(hd(tl(Nodes)), port, dbg:trace_port(ip, 28883)),
    dbg:trace_client(ip, 28882, get_handler_spec()),
    dbg:trace_client(ip, 28883, get_handler_spec()),
    erlang:apply(dbg, p, tuple_to_list(Flags)),
    [erlang:apply(dbg, tpl, tuple_to_list(Pattern)) || Pattern <- Patterns].

stop_trace() ->
    dbg:stop_clear().

tport(Node) ->
    dbg:trace_port(file, {"a_trace_file__"++atom_to_list(Node), wrap, ".trc"}).

tiport(P) ->
    dbg:trace_port(ip, P).

get_handler_spec() ->
    F = fun(Msg, Last) ->
		io:format("Got: ~p~n", [Msg]),
		
		Last+1
	end,
    {F, 0}.

setup() ->
    dbg:tracer(process, get_handler_spec()),
    dbg:p(all, call),
    dbg:tp(dbgtest, []).

test() ->
    3.


merge_with_inviso() ->
    Nodes = [client@ariel, server@ariel, debug@ariel],
    gen_nodes_wrp(Nodes, 8).
    
			      
gen_nodes_flat(Nodes) ->
    lists:map(fun(Node) ->
		      {Node, filename:join("trace", atom_to_list(Node) ++ "_trace.trc")}
	      end, Nodes).
    
gen_nodes_wrp(Nodes, Wrp) ->
    lists:map(fun(Node) ->
		      AllNames = [filename:join("trace", atom_to_list(Node) ++ "_trace"
						++ integer_to_list(N) ++".trc") ||
				     N <- lists:seq(0, Wrp)],
		      
		      Existing = sort_files(AllNames),
		      {Node, [{trace_log, Existing}]}
	      end, Nodes).
			  
sort_files(AllNames) ->
    {_, BeforeGap, AfterGap} =
	lists:foldl(fun(Name, {bef, Before, []}) ->
			    case filelib:is_regular(Name) of
				true ->
				    {bef, [Name | Before], []};
				false ->
				    {aft, Before, []}
			    end;
		       (Name, {aft, Before, After}) ->
			    case filelib:is_regular(Name) of
				true ->
				    {aft, Before, [Name | After]};
				false ->
				    {fin, Before, After}
			    end;
		       (_, {fin, Before, After}) ->
			    {fin, Before, After}
		    end, {bef, [], []}, AllNames),
    lists:append(lists:reverse(AfterGap), lists:reverse(BeforeGap)).
