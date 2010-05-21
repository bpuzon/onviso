%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%%% @doc  The module provides tests for onviso_server
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
-module(onviso_server_tests).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

-include_lib("eunit/include/eunit.hrl").
-include("onviso.hrl").

setup() ->
    onviso_server:start_link().

cleanup(_) ->
    onviso_server:stop().

file_cleanup(_) ->
    cleanup(ok),
    file:delete("test.conf").

setup_run() ->
    onviso_server:start_link(),
    TraceCase = test_tracecase(),
    onviso_server:add_trace_case(TraceCase),
    ok.

control_test_() ->
    {inorder, 
     {setup, 
      fun setup_run/0,
      fun cleanup/1, 
      [
       ?_test(merge_different_runs()), %%order does matter here
       ?_test(run_trace()),
       ?_test(run_trace_no_exist()),
       ?_test(stop_trace()),
       ?_test(stop_trace_no_exist()),
       ?_test(merge_trace()),
       ?_test(merge_trace_no_exist()),
       ?_test(merge_run_no_exist()),
       ?_test(merge_trace_unlinked())
      ]
     }
    }.

id_test_() ->
    {inorder, 
     {setup,
      fun setup/0,
      fun cleanup/1,
      [
       ?_test(multiple_test_id_gen())       
      ]
     }
    }.

get_add_create_delete_test_() ->
    {inorder, 
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [?_test(add_trace_case()),
       ?_test(add_pattern()),
       ?_test(add_flags()),
       ?_test(add_mergespec()),
       ?_test(get_trace_cases()),
       ?_test(get_patterns()),
       ?_test(get_flags()),
       ?_test(get_merge_confs()),
       ?_test(get_trace_case()),
       ?_test(get_pattern()),
       ?_test(get_flag()),
       ?_test(get_merge_conf()),
       ?_test(delete_trace_case()),
       ?_test(delete_pattern()),
       ?_test(delete_flag()),
       ?_test(delete_merge_conf())
      ]
     }
    }.

file_test_() ->
    {inorder, 
     {foreach,
      fun setup/0,
      fun file_cleanup/1,
      [?_test(write_conf_to_file()),
       %%The test is commented out as this functionality has been moved
       %%to the UI layer
       %%?_test(write_conf_file_exists()),
       ?_test(read_from_file()),
       ?_test(read_from_non_existing_file()),
       ?_test(read_corrupted_file()),
       ?_test(read_and_update_trace_runs()),
       ?_test(usable_fun_after_read())
      ]
     }
    }.


multiple_test_id_gen() ->
    Tc1 = config_generator:create_trace_case("1"),
    onviso_server:add_trace_case(Tc1),

    Tc2 = config_generator:create_trace_case("2"),
    onviso_server:add_trace_case(Tc2),

    Tc3 = config_generator:create_trace_case("3"),
    onviso_server:add_trace_case(Tc3),
    
    ?assertMatch([_, _, _], onviso_server:get_trace_cases()).


%% ---- Setters 
add_trace_case() ->
    TraceCase = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TraceCase)),    
    TC2 = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TC2)).    
 
add_pattern() ->
    Pattern = config_generator:create_pattern(io, format, '_', []),
    ?assertMatch(#pattern{}, onviso_server:add_pattern(Pattern)).

add_flags() ->
    Flag = config_generator:create_flags(all, [call]),
    ?assertMatch(#flags{}, onviso_server:add_flag(Flag)).

add_mergespec() ->
    MergeConf = test_mergeconf(),
    ?assertMatch(#merge_conf{}, onviso_server:add_merge_conf(MergeConf)).


%% ---- Getters
get_trace_cases() ->
    TraceCase = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TraceCase)),
    ?assertMatch([#trace_case{case_id = 1, name = "Simple identifier"}],
                 onviso_server:get_trace_cases()).

get_patterns() ->
    TraceCase = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TraceCase)),
    ?assertMatch([#pattern{id = 1}], onviso_server:get_patterns()).

get_flags() ->
    TraceCase = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TraceCase)),
    ?assertMatch([#flags{id = 1}], onviso_server:get_flags()).

get_merge_confs() ->
    TraceCase = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TraceCase)),
    ?assertMatch([#merge_conf{id = 1}], onviso_server:get_merge_configurations()).


get_trace_case() ->
    TraceCase = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TraceCase)),
    ?assertMatch(#trace_case{case_id = 1}, 
                 onviso_server:get_trace_case(1)).

get_pattern() ->
    Pattern = config_generator:create_pattern(io, format, '_', []),
    ?assertMatch(#pattern{}, onviso_server:add_pattern(Pattern)),
    ?assertMatch(#pattern{id = 1}, 
                 onviso_server:get_pattern(1)).

get_flag() ->
    Flag = config_generator:create_flags(all, [call]),
    ?assertMatch(#flags{}, onviso_server:add_flag(Flag)),
    ?assertMatch(#flags{id = 1}, 
                 onviso_server:get_flag(1)).

get_merge_conf() ->
    MergeConf = test_mergeconf(),
    ?assertMatch(#merge_conf{}, onviso_server:add_merge_conf(MergeConf)),
    ?assertMatch(#merge_conf{id = 1}, 
                 onviso_server:get_merge_conf(1)).

%% ---- Removal
delete_trace_case() ->
    TraceCase = test_tracecase(),
    TraceCase2 = onviso_server:add_trace_case(TraceCase),
    ?assertEqual(ok, onviso_server:delete_trace_case(TraceCase2#trace_case.case_id)),
    ?assertEqual(false, onviso_server:get_trace_case(1)).   

delete_pattern() ->
    Pattern = config_generator:create_pattern(io, format, '_', []),
    Pattern2 = onviso_server:add_pattern(Pattern),
    ?assertEqual(ok, onviso_server:delete_pattern(Pattern2#pattern.id)),
    ?assertEqual(false, onviso_server:get_pattern(1)).   

delete_flag() ->
    Flag = config_generator:create_flags(all, [call]),
    Flag2 = onviso_server:add_flag(Flag),
    ?assertEqual(ok, onviso_server:delete_flag(Flag2#flags.id)),
    ?assertEqual(false, onviso_server:get_flag(1)).   

delete_merge_conf() ->
    MergeConf = test_mergeconf(),
    MConf2 =  onviso_server:add_merge_conf(MergeConf),
    ?assertEqual(ok, onviso_server:delete_merge_conf(MConf2#merge_conf.id)),
    ?assertEqual(false, onviso_server:get_merge_conf(1)).   

%% ---- Read write files
write_conf_to_file() ->
    TraceCase = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TraceCase)),
    {ok, Size} = onviso_server:save_to_file("test.conf"),
    ?assert(Size == filelib:file_size("test.conf")),
    ?assertEqual(ok, file:delete("test.conf")).

write_conf_file_exists() ->
    TraceCase = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TraceCase)),
    {ok, Size} = onviso_server:save_to_file("test.conf"),
    ?assert(Size == filelib:file_size("test.conf")),
    ?assertEqual({error, file_exists}, onviso_server:save_to_file("test.conf")),
    ?assertEqual(ok, file:delete("test.conf")).

read_from_file() ->
    TraceCase = test_tracecase(),
    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(TraceCase)),
    {ok, Size} = onviso_server:save_to_file("test.conf"),
    ?assertEqual(Size, filelib:file_size("test.conf")),
    ?assertEqual({ok, [{trace_cases, 1}, {patterns, 1},
                       {flags, 1}, {merge_configurations, 1}]},
                 onviso_server:read_from_file("test.conf")),
    ?assertEqual(ok, file:delete("test.conf")).

read_and_update_trace_runs() ->
    TraceCase = test_tracecase(),
    onviso_server:add_trace_case(TraceCase),
    ?assertMatch(ok, onviso_server:run_trace(1)),
    ?assertMatch(ok, onviso_server:stop_trace(1)),
    Trace = onviso_server:get_trace_case(1),
    ?assert(length(Trace#trace_case.all_traces) /= 0),
    onviso_server:save_to_file("test.conf"),
    onviso_server:read_from_file("test.conf"),
    ?assertMatch(Trace, onviso_server:get_trace_case(1)),
    onviso_server:save_to_file("test.conf"),
    onviso:clean(),
    onviso_server:read_from_file("test.conf"),
    Trace2 = onviso_server:get_trace_case(1),
    ?assertMatch([], Trace2#trace_case.all_traces).

read_from_non_existing_file() ->
    ?assertEqual({error, enoent}, onviso_server:read_from_file("bogus.conf")).

read_corrupted_file() ->
    ?assertEqual(ok, file:write_file("corrupt.conf", term_to_binary("epic"))),
    ?assertEqual({error, file_corrupted}, 
                 onviso_server:read_from_file("corrupt.conf")),
    ?assertEqual(ok, file:delete("corrupt.conf")).

usable_fun_after_read() ->
    TraceCase = test_tracecase(),

    Fun = fun(X) -> X * X end,
    MConf = hd(TraceCase#trace_case.merge_confs),
    UpdConf = MConf#merge_conf{workfun = Fun},
    UpdCase = TraceCase#trace_case{merge_confs = [UpdConf]},

    ?assertMatch(#trace_case{}, onviso_server:add_trace_case(UpdCase)),
    {ok, _Size} = onviso_server:save_to_file("test.conf"),    

    onviso_server:delete_merge_conf(1),

    ?assertEqual({ok, [{trace_cases, 1}, {patterns, 1},
                       {flags, 1}, {merge_configurations, 1}]},
                 onviso_server:read_from_file("test.conf")),

    [#merge_conf{workfun=WorkFun}] = onviso_server:get_merge_configurations(),
    ?assert(is_function(WorkFun)),
    ?assert(WorkFun(2) == 4),    

    ?assertEqual(ok, file:delete("test.conf")).

%% ---- Control onviso tests
run_trace() ->
    ?assertEqual(ok, onviso_server:run_trace(1)),
    ?assertMatch({ok, _}, onviso:status(1)).


run_trace_no_exist() ->
    ?assertEqual({error, no_trace}, onviso_server:run_trace(123123)).

stop_trace() ->
    ?assertEqual(ok, onviso_server:stop_trace(1)).

stop_trace_no_exist() ->
    ?assertEqual(ok, onviso_server:stop_trace(123123)).

merge_trace() ->
    ?assertEqual(ok, onviso_server:run_trace(1)),
    ?assertMatch({ok, _}, onviso_server:merge_trace(1,1)).

merge_different_runs() ->
    ?assertEqual(ok, onviso_server:run_trace(1, "comm1")),
    ?assertEqual(ok, onviso_server:run_trace(1, "comm2")),
    Trace = onviso_server:get_trace_case(1),
    Runs = Trace#trace_case.all_traces,
    ?assertMatch([{_, _, "comm2"}, {_, _, "comm1"}], Runs),
    [{N1, _, _}, {N2, _, _}] = Runs,
    ?assertMatch({ok, _}, onviso_server:merge_trace(1, N1, 1)),    
    ?assertMatch({ok, _}, onviso_server:merge_trace(1, N2, 1)).
    
merge_trace_no_exist() ->
    ?assertEqual({error, no_trace_running}, onviso_server:merge_trace(1231231, 12)).

merge_run_no_exist() ->
    ?assertEqual(ok, onviso_server:run_trace(1)),
    ?assertEqual({error, unknown_trace_execution}, onviso_server:merge_trace(1,12323,1)).

merge_trace_unlinked() ->
    ?assertEqual({error, unknown_merge}, onviso_server:merge_trace(1, 3)).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------
test_tracecase() ->
    Pattern = config_generator:create_pattern(io, format, '_', caller),
    Flag = config_generator:create_flags(all, [call]),       
    
    
    EmptyCase = config_generator:create_trace_case("Simple identifier"),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("server@" ++ Host),
    NodeCase = config_generator:add_nodes(EmptyCase, [Node]),
    PatternCase = config_generator:add_pattern(NodeCase, Pattern),
    FlagCase = config_generator:add_flags(PatternCase, Flag),

    MergeConf = test_mergeconf(file),
    MergeCase = config_generator:add_merge_conf(FlagCase, MergeConf),
    
    MergeCase.

test_mergeconf() ->
    test_mergeconf(file).

test_mergeconf(Where) ->    
    WorkFun = 
        fun(Node, Trace, Pidmappings, Data) ->
                [{Node, Trace, Pidmappings}|Data]
        end,
    config_generator:create_merge_conf(void, WorkFun, Where).

