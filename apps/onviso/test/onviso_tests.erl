%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%%% @doc  The module generate traces for the tests
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
-module(onviso_tests).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

-include_lib("eunit/include/eunit.hrl").

-export([generate_traces/1]).

-record(trace_data, {patterns,
		     flags,
		     overload,
		     collector,
		     nodes,
		     options}).

%%--------------------------------------------------------------------
%% Set-up and teardown 
%%--------------------------------------------------------------------
setup() ->
    onviso:clean(), % always start from a clean sheet
    timer:sleep(20).

teardown(_) ->
    ok.

trace_test_() ->
    {inorder,
     {foreach, 
      fun setup/0,
      fun teardown/1,
      [
       ?_test(trace()),
       ?_test(trace_with_arities()),
       ?_test(standard_merge()),
       ?_test(merge_when_node_restarts()),
       ?_test(reconnect_running_node()),
       ?_test(auto_reconnect()),
       ?_test(node_status_test()),
       ?_test(profiling_count_function_calls()),
       ?_test(profiling_count_function_calls_shell()),
       ?_test(profiling_time_function_calls()),
       ?_test(multiple_trace_starts()),
       ?_test(relay_to_shell()),
       ?_test(trace_message()),
       ?_test(trace_message_with_arity()),
       ?_test(trace_gc()),
       ?_test(stop_inviso_not_running()),
       ?_test(stop_with_no_trace()),
       ?_test(add_trace_no_nodes()),
       ?_test(add_trace_wrong_patterns()),
       ?_test(merge_without_trace()),
       ?_test(clean()),
       ?_test(clean_exists()),
       ?_test(status_all()),
       ?_test(status_all_multiple()),
       ?_test(status_one()),
       ?_test(status_no_trace()),
       ?_test(status_one_no_trace()),
       ?_test(last_trace_no_trace()),
       ?_test(trace_only_one_node()),
       ?_test(node_counter_trace()),
       string2ms_checks()
      ]
     }
    }.

%%--------------------------------------------------------------------
%% Function call traces
%%--------------------------------------------------------------------
trace() ->
    {ok, Ref} = onviso:trace(get_standard_patterns(),
                             get_nodes(),
                             get_traceflags()),
    ?assert(Ref > 0),
    ?assertMatch({error, {already_started, _}}, inviso:start()),
    ?assert(ok =:= generate_traces(10)),
    ?assertEqual(shutdown, onviso:stop(Ref)),
    ?assertEqual(ok, onviso:clean()).

trace_with_arities() ->
    {ok, Ref} = onviso:trace(get_standard_patterns_with_arity(),
                             get_nodes(),
                             get_traceflags()),
    ?assert(Ref > 0),
    ?assertMatch({error, {already_started, _}}, inviso:start()),
    ?assert(ok =:= generate_traces(10)),
    ?assertEqual(shutdown, onviso:stop(Ref)),
    ?assertEqual(ok, onviso:clean()).


standard_merge() ->
    {ok, Ref} = onviso:trace(get_standard_patterns(),
                             get_nodes(),
                             get_traceflags()),
    ?assert(Ref > 0),
    ?assert(ok =:= generate_traces(10)),

    {ok, Count} = onviso:merge(Ref, void, void, file),
    ?assert(Count > 0),
    onviso:clean().

node_status_test() ->
    {ok, Ref} = onviso:trace(get_message_patterns(),
			     get_nodes(),
			     get_traceflags()),
    ?assert(Ref > 0),
    List = onviso:node_status(),
    ?assert(lists:member({client_node(), active}, List)),
    ?assert(lists:member({server_node(), active}, List)),
    ?assertEqual(length(List), 2),
    onviso:clean().

reconnect_running_node() ->
    {ok, Ref} = onviso:trace(get_message_patterns(),
			     get_nodes(),
			     get_traceflags()),
    ?assert(Ref > 0),
    ?assert(ok =:= generate_traces(2)),
    onviso:reconnect(client_node()),
    ?assert(ok =:= generate_traces(2)),

    {ok, Count} = onviso:merge(Ref, void, void, file),
    ?assertEqual(12, Count),
    onviso:clean().

merge_when_node_restarts() ->
    %%resume_disabled should have been checked in different
    %%unit test, but each node restart require time
    {ok, Ref} = onviso:trace(get_message_patterns(),
			     get_nodes(),
			     get_traceflags(),
			     [],
			     [{autoresume, [{disabled, true},
					    {default_interval, 100}]}]),
    ?assert(Ref > 0),
    ?assert(ok =:= generate_traces(2)),
    rpc:call(client_node(), init, restart, []),
    timer:sleep(2000),
    rpc:call(client_node(), client, init, [server_node()]),
    List = onviso:node_status(),
    ?assert(lists:member({client_node(), inactive}, List)),

    onviso:reconnect(client_node()),

    ?assert(ok =:= generate_traces(2)),
    {ok, Count} = onviso:merge(Ref, void, void, file),
    ?assertEqual(10, Count),
    onviso:clean().
	
auto_reconnect() ->    
    {ok, Ref} = onviso:trace(get_message_patterns(),
			     get_nodes(),
			     get_traceflags(),
			     [],
			     [{autoresume, [{default_interval, 100},
					    {nodes, [{client_node(), 100},
						     {server_node(), 200}]}]}]),
    ?assert(Ref > 0),
    ?assert(ok =:= generate_traces(2)),
    rpc:call(client_node(), init, restart, []),
    timer:sleep(2000),
    rpc:call(client_node(), client, init, [server_node()]),

    ?assert(ok =:= generate_traces(2)),
    {ok, Count} = onviso:merge(Ref, void, void, file),
    ?assertEqual(10, Count),
    onviso:clean().
    


profiling_count_function_calls() ->
    {ok, Ref} = onviso:trace(get_standard_patterns(),
                             get_nodes(),
                             get_traceflags()),
    ?assert(ok =:= generate_traces(10)),
    {ok, Count} = onviso_example:profiling_count_function_calls(Ref),
    ?assert(Count > 0),
    onviso:clean().


profiling_count_function_calls_shell() ->
    {ok, Ref} = onviso:trace(get_standard_patterns(),
                             get_nodes(),
                             get_traceflags()),
    ?assert(ok =:= generate_traces(10)),
    ?assertMatch({ok, _}, onviso_example:profiling_count_function_calls_shell(Ref)),
    onviso:clean().

profiling_time_function_calls() ->
    {ok, Ref} = onviso:trace(get_standard_patterns(),
                             get_nodes(),
                             get_traceflags()),
    ?assert(ok =:= generate_traces(10)),
    ?assertMatch({ok, _}, onviso_example:profiling_time_function_calls(Ref)),
    onviso:clean().


multiple_trace_starts() ->
    {ok, Ref1} = onviso:trace(get_standard_patterns(),
                              get_nodes(),
                              get_traceflags()),

    ?assert(Ref1 =:= 1),

    {ok, Ref2} = onviso:trace(get_standard_patterns(),
                              get_nodes(),
                              get_traceflags()),

    ?assert(Ref2 =:= 2),

    onviso:clean().

relay_to_shell() ->
    ?assertEqual({ok, 1}, onviso:trace(get_standard_patterns(),
					[{relayer, server_node()},
					 {relayer, client_node()},
					 {collector, node()}],
                                       get_traceflags())),       
    
    onviso:clean().

trace_only_one_node() ->
    ?assertEqual({ok, 1}, onviso:trace([{io, format, 2, []}],
                                       {all, [call]})),

    io:format("test message1~n", []),
    io:format("test message2~n", []),
    io:format("test message3~n", []),
    timer:sleep(1000),

    ?assertEqual({ok, 3}, onviso:merge(1, void, void, shell)),

    onviso:clean().

node_counter_trace() ->
    onviso:trace(get_standard_patterns(),
                 get_nodes(),
                 get_traceflags()),

    generate_traces(10),
    
    ?assertMatch({ok, _}, onviso_example:node_trace_counter(1)).
                               

%%--------------------------------------------------------------------
%% Message passing traces
%%--------------------------------------------------------------------
trace_message() ->
    {ok, Ref} = onviso:trace(get_message_patterns(),
                             get_nodes(),
                             get_message_tf()),

    ?assert(ok =:= generate_traces(10)),

    WorkFun = 
        fun(Node, Trace, _PidMappings, HandlerData) ->
                NewData = [{Node, Trace}|HandlerData],
                {ok, NewData}
        end,
    
    {ok, Count} = onviso:merge(Ref, void, WorkFun, file),
    ?assert(Count > 0),                
    onviso:clean().

trace_message_with_arity() ->
{ok, Ref} = onviso:trace(get_message_patterns_with_arity(),
                             get_nodes(),
                             get_message_tf()),

    ?assert(ok =:= generate_traces(10)),

    WorkFun = 
        fun(Node, Trace, _PidMappings, HandlerData) ->
                NewData = [{Node, Trace}|HandlerData],
                {ok, NewData}
        end,
    
    {ok, Count} = onviso:merge(Ref, void, WorkFun, file),
    ?assert(Count > 0),                
    onviso:clean().
    


%%--------------------------------------------------------------------
%% Garbage collection and process information traces
%%--------------------------------------------------------------------
trace_gc() ->
    {ok, Ref} = onviso:trace(get_standard_patterns(),
                             get_nodes(),
                             get_procs_tf()),
    
    generate_multiple_traces(20), % used to make the gc happy :) 
    rpc:cast(server_node(), erlang, garbage_collect, []), % pure force
    rpc:cast(client_node(), erlang, garbage_collect, []), % pure force
 
   WorkFun = 
        fun(Node, Trace, _PidMappings, HandlerData) ->
                NewData = [{Node, Trace}|HandlerData],
                {ok, NewData}
        end,
   
    timer:sleep(1000),
    {ok, Count} = onviso:merge(Ref, void, WorkFun, file),
    ?assert(Count > 0),
    onviso:clean().


%%--------------------------------------------------------------------
%% Error messages tests
%%--------------------------------------------------------------------
stop_inviso_not_running() ->
    ?assertEqual({error, no_trace_running}, onviso:stop(0)).

stop_with_no_trace() ->
    inviso:start(),
    ?assertEqual({error, no_trace_running}, onviso:stop(0)).

add_trace_no_nodes() ->
    ?assertMatch({ok, _}, onviso:trace(get_standard_patterns(),
                                       [],
                                       get_traceflags())),
    ?assertEqual(ok, onviso:clean()).

add_trace_wrong_patterns() ->
    Patterns = [{io, format, '_'}],
    ?assertEqual({error, pattern_format}, onviso:trace(Patterns,
                                                       get_nodes(),
                                                       get_traceflags())),
    ?assertEqual(ok, onviso:clean()).

merge_without_trace() ->
    ?assertEqual({error, no_trace_running}, onviso:merge(0, void, void, file)).

%%--------------------------------------------------------------------
%% Clean up tests
%%--------------------------------------------------------------------
clean() ->
    ?assertEqual(ok, onviso:clean()).


clean_exists() ->
    ?assertMatch({ok, _}, onviso:trace(get_standard_patterns(),
                                       get_nodes(),
                                       get_traceflags())),
    ?assertEqual(ok, onviso:clean()).

%%--------------------------------------------------------------------
%% Status tests
%%--------------------------------------------------------------------
status_all() ->
    ?assertMatch({ok, 1}, onviso:trace(get_standard_patterns(),
                                       get_nodes(),
                                       get_traceflags())),
    Row = get_tdata([]).
%    ?assertMatch({ok, [{1, running, Row, _}]}, onviso:status()).


status_all_multiple() ->
    ?assertMatch({ok, 1}, onviso:trace(get_standard_patterns(),
                                       get_nodes(),
                                       get_traceflags())),

    ?assertMatch({ok, 2}, onviso:trace(get_standard_patterns(),
                                       get_nodes(),
                                       get_traceflags())),    

    Row = get_tdata([]),
    ?assertMatch({ok, [{1, stopped, Row, _},
                       {2, running, Row, _}]},
                 onviso:status()).


status_one() ->
    {ok, Ref} = onviso:trace(get_standard_patterns(),
                             get_nodes(),
                             get_traceflags()),
    ?assert(Ref =:= 1),

    Row = get_tdata([]),
    ?assertMatch({ok, [{1,
                        running,
			Row,
			_}]},
                 onviso:status()).

status_no_trace() ->
    ?assertEqual({error, no_trace_running}, onviso:status()).

status_one_no_trace() ->
    ?assertEqual({error, no_trace_running}, onviso:status(15165)).

last_trace_no_trace() ->
    ?assertEqual({error, no_trace_running}, onviso:last_trace()).


%%--------------------------------------------------------------------
%% string2ms tests
%%--------------------------------------------------------------------

string2ms_checks() ->
    [?_test(assert_tranform(ok, "fun(_) -> return_trace() end.")),
     ?_test(assert_tranform(ok, "fun(_) -> return_trace() end")),
     ?_test(assert_tranform(error, "asas")),
     ?_test(assert_tranform(error, "A = 3 + 2. 3."))].
	    
    
assert_tranform(Type, String) ->
    ?assertMatch({Type, _}, onviso:string2ms(String)).

%%--------------------------------------------------------------------
%% Trace info
%%--------------------------------------------------------------------

server_node() ->
    {ok, HostName} = inet:gethostname(),
    list_to_atom("server@" ++ HostName).

client_node() ->
    {ok, HostName} = inet:gethostname(),
    list_to_atom("client@" ++ HostName).

get_nodes() ->

    Nodes = [client_node(), server_node()],    
    [?assert(net_kernel:connect(Node)) || Node <- Nodes],

    Nodes.

get_tdata(Options) ->
    #trace_data{patterns = get_standard_patterns(),
		nodes = [client_node(),
			 server_node()],
		overload = [],
		options = Options,
		flags = {all, [call]}}.
				
get_message_patterns() ->
    [
     {server, loop, '_', []},
     {client, get, '_', []},
     {client, put, '_', []}
    ].

get_message_patterns_with_arity() ->
    [
     {server, loop, 1, []},
     {client, get, 0, []},
     {client, put, 1, []}
    ].
    
get_message_tf() ->
    {all, [send, 'receive']}.

get_procs_tf() ->
    {all, [procs, garbage_collection]}.
        
get_standard_patterns() ->
    [
     {server, loop, '_', caller},
     {client, get, '_', {fun2ms, "fun(_) -> return_trace() end."}},
     {client, put, '_', []}
    ].


get_standard_patterns_with_arity() ->
    [
     {server, loop, 1, caller},
     {client, get, 0, return},
     {client, put, 1, []}
    ].

get_traceflags() ->
    {all, [call]}.

%%--------------------------------------------------------------------
%% Generate traces
%%--------------------------------------------------------------------

generate_traces(0) ->
    timer:sleep(10),
    {server, server_node()} ! clear,
    ok;
generate_traces(N) when N rem 3 =:= 0 ->
    rpc:cast(client_node(), client, get, []),
    generate_traces(N-1);
generate_traces(N) ->
    rpc:cast(client_node(), client, put, [N]),
    generate_traces(N-1).

generate_multiple_traces(0) ->    
    ok;
generate_multiple_traces(N) when N rem 5 =:= 0 ->
    {server, server_node()} ! clear,
    generate_multiple_traces(N-1);
generate_multiple_traces(N) ->
    spawn(?MODULE, generate_traces, [10]),
    generate_multiple_traces(N-1).
