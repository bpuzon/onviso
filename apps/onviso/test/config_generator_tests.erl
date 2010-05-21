%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%%% @doc  The module provides test cases for config_generator
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
-module(config_generator_tests).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

-include_lib("eunit/include/eunit.hrl").
-include("onviso.hrl").

setup() -> 
    ?assertMatch({ok, _}, onviso_server:start_link()).    

cleanup(_) ->
    onviso_server:stop().

run_test_() ->    
    {inorder, 
     {foreach,
      fun setup/0, 
      fun cleanup/1, 
      lists:append(create_parts(),
                   [
                    ?_test(add_pattern()),
                    ?_test(add_flags()),
                    ?_test(add_mergeconf()),
                    ?_test(add_nodes()),
                    ?_test(add_invalid_matchspec_pattern()),
                    ?_test(create_fun()),
                    ?_test(create_fun_with_period()),
                    ?_test(create_fun_wrong_syntax()),
		    ?_test(validate_multiple_merges()),
                    ?_test(validate_config_basic("_", '_')),
                    ?_test(validate_config_basic("2", 2)),
		    ?_test(validate_fail_wrong_arity("-3")),
		    ?_test(validate_fail_wrong_arity("sd")),
		    validate_trace_options_tests(),
		    ?_test(validate_empty_test())
                   ])
     }
    }.


create_parts() ->
    [?_assert(#pattern{id = undefined,
                       module = io, 
                       function = format, 
                       arity = '_',
                       matchspec = []} =:=
              config_generator:create_pattern(io, 
                                              format, 
                                              '_',
                                              [])),              
     ?_assert(#flags{id = undefined, 
                     scope = all,
                     flags = [call]} =:= 
              config_generator:create_flags(all, [call])),
     ?_assertMatch(#trace_case{case_id = undefined,
                               name = test,
                               nodes = [],
                               patterns = [], 
                               timestamp = _}, 
                   config_generator:create_trace_case(test)),
     ?_assert(#merge_conf{id = undefined,
                          beginfun = void,
                          workfun = void,
                          endfun = file,
			  comment = ""
                         } =:=
              config_generator:create_merge_conf())
    ].           
    
add_pattern() ->
    TraceCase = config_generator:create_trace_case(),
    Pattern = config_generator:create_pattern(io, format, '_', return),

    ?assertMatch(#trace_case{case_id = undefined,
                             patterns = [#pattern{id = undefined, 
                                                  module = io, 
                                                  function = format, 
                                                  arity = '_', 
                                                  matchspec = return}]},
                 config_generator:add_pattern(TraceCase, Pattern)).

add_invalid_matchspec_pattern() ->
    ?assertEqual({error, invalid_matchspec}, 
                 config_generator:create_pattern(lists, append, '_', invalid)).

add_flags() ->
    TraceCase = config_generator:create_trace_case(),
    Flags = config_generator:create_flags(all, [call]),
    
    ?assertMatch(#trace_case{case_id = undefined,
                             trace_flag = #flags{id = undefined,
                                                 scope = all, 
                                                 flags = [call]}},
                 config_generator:add_flags(TraceCase, Flags)).

add_mergeconf() ->
    TraceCase = config_generator:create_trace_case(),
    MergeConf = config_generator:create_merge_conf(void, void, file,
						    "comment"),

    ?assertMatch(#trace_case{case_id = undefined,
                             merge_confs = [#merge_conf{id = undefined,
							beginfun = void, 
							workfun = void, 
							endfun = file,
							comment = "comment"
						       }]},
                 config_generator:add_merge_conf(TraceCase, MergeConf)).

add_nodes() ->
    TraceCase = config_generator:create_trace_case(),
    {ok, StrHostName} = inet:gethostname(),
    NShort = ["server", "client"],
    Nodes = [list_to_atom(NS ++ "@" ++ StrHostName) || NS <- NShort],
    ?assertMatch(#trace_case{case_id = undefined, 
                             nodes = Nodes}, 
                 config_generator:add_nodes(TraceCase, Nodes)).


    
create_fun() ->
    Test = "fun(X) -> X * X end",
    {ok, Fun} = config_generator:create_fun(Test),
    ?assert(is_function(Fun)),
    ?assert(Fun(3) =:= 9).

create_fun_with_period() ->
    Test = "fun(X) -> X * X end.",
    {ok, Fun} = config_generator:create_fun(Test),
    ?assert(is_function(Fun)),
    ?assert(Fun(3) =:= 9).

create_fun_wrong_syntax() ->
    Test = "fun(X) -> X * X; end",
    ?assertEqual({error, syntax}, config_generator:create_fun(Test)).


validate_multiple_merges() ->
    Patterns = [{"io", "format", "_", "[]"}],
    Nodes = ["server@linux-hdsc"],
    Flags = ["{all, [call]}"],

    WorkFun = 
        "fun(Node, Trace, PidMappings, Data) ->"
        "[{Node, Trace, PidMappings}|Data]"
        "end.",
    EndFun = 
        "fun(Data) ->"
        "ioformat(\"~p~n\", [Data])"
        "end.",

    Merge = [{beginfun, ""}, {workfun, WorkFun}, {endfun, EndFun}, {comment, ""}],

    Options = [{overload, "[]"}, 
               {name, "TestValidate"}, 
               {output_file, "file.txt"}],

    ?assertMatch({ok, _}, 
                 config_generator:validate(Patterns, Nodes, Flags, [Merge, Merge], Options)).


validate_config_basic(ArityString, Arity) ->
    Patterns = [{"io", "format", ArityString, "[]"}],
    Nodes = ["server@linux-hdsc"],
    Flags = ["{all, [call]}"],

    WorkFun = 
        "fun(Node, Trace, PidMappings, Data) ->"
        "[{Node, Trace, PidMappings}|Data]"
        "end.",
    EndFun = 
        "fun(Data) ->"
        "io:format(\"~p~n\", [Data])"
        "end.",
    Merge = [{beginfun, ""}, {workfun, WorkFun}, {endfun, EndFun}, {comment, ""}],

    Options = [{overload, "[]"}, 
               {name, "TestValidate"}, 
               {output_file, "file.txt"}],
    
    {ok, #trace_case{case_id=undefined,
                     name = "TestValidate",
                     patterns = [AddedPattern],
                     trace_flag = AddedFlag,
                     nodes = ['server@linux-hdsc'],
                     merge_confs = [AddedMerge],
                     overload = [], 
                     output = "file.txt"}} = 
        config_generator:validate(Patterns, Nodes, Flags, [Merge], Options),
    
    ?assertEqual(#pattern{id = undefined,
                          module = io,
                          function = format,
                          arity = Arity,
                          matchspec = []}, AddedPattern),
    ?assertEqual(#flags{id = undefined, 
                        scope = all,
                        flags = [call]}, AddedFlag),
    
    #merge_conf{beginfun = void,
                workfun = AddedWorkFun,
                endfun = AddedEndFun
               } = AddedMerge,
    
    ?assert(is_function(AddedWorkFun)),
    ?assert(is_function(AddedEndFun)).

validate_trace_options_tests() ->
    [?_test(validate_trace_options("[]", ok)),
     ?_test(validate_trace_options("[],", error)),
     ?_test(validate_trace_options("atom", error)),
     ?_test(validate_trace_options("[{resume_disabled, 3}]", error)),
     ?_test(validate_trace_options("[{autoresume, [{buggy, 3}]}]", error)),
     ?_test(validate_trace_options("[{autoresume, [{nodes, [{ariel@client, 2}]}]}]", ok))].

validate_trace_options(TraceOptsString, ResultType) ->
    Patterns = [{"io", "format","3", "[]"}],
    Nodes = ["server@linux-hdsc"],
    Flags = ["{all, [call]}"],

    WorkFun = 
        "fun(Node, Trace, PidMappings, Data) ->"
        "[{Node, Trace, PidMappings}|Data]"
        "end.",
    EndFun = 
        "fun(Data) ->"
        "io:format(\"~p~n\", [Data])"
        "end.",
    Merge = [{beginfun, ""}, {workfun, WorkFun}, {endfun, EndFun}, {comment, ""}],

    Options = [{overload, "[]"}, 
               {name, "TestValidate"}, 
               {output_file, "file.txt"}],
    
    Return =  config_generator:validate(Patterns, Nodes, Flags,
					[Merge], Options, TraceOptsString),
    ?assertMatch({ResultType, _}, Return).
		


validate_empty_test() ->
    Patterns = [],
    Nodes = [],
    Flags = [],
    Options = [{overload, "[]"}, 
               {name, "TestValidate"}, 
               {output_file, "file.txt"}],

    {Reply, Details} = config_generator:validate(Patterns, Nodes, Flags, [], Options),
    
    ?assertMatch(error, Reply), 
    ?assertMatch(true, lists:member({merge_confs, error, no_data}, Details)),
    ?assertMatch(true, lists:member({flags, error, no_data}, Details)),
    ?assertMatch(true, lists:member({nodes, error, no_data}, Details)),
    ?assertMatch(true, lists:member({patterns, error, no_data}, Details)).
    
validate_fail_test() ->
    Patterns = [{"io", "format", "_", "[]"}, {"wrong", "arguments", "passed"}],
    Nodes = ["server@linux-hdsc"],
    Flags = ["all, [call]}"],

    WorkFun = 
        "fun(Node, Trace, PidMappings, Data) ->"
        "[{Node, Trace, PidMappings}|Data]"
        "end.",
    EndFun = 
        "fun(Data) ->"
        "ioformat(\"~p~n\", [)"
        "end.",
    Merge = [{beginfun, ""}, {workfun, WorkFun}, {endfun, EndFun}],

    Options = [{overload, "[]"}, 
               {name, "TestValidate"}, 
               {output_file, "file.txt"}],

    ?assertMatch({error, _}, 
                 config_generator:validate(Patterns, Nodes, Flags, [Merge], Options)).

validate_fail_wrong_arity(Arity) ->
    Patterns = [{"io", "format", Arity, "[]"}],
    Nodes = ["server@linux-hdsc"],
    Flags = ["all, [call]}"],

    WorkFun = 
        "fun(Node, Trace, PidMappings, Data) ->"
        "[{Node, Trace, PidMappings}|Data]"
        "end.",
    EndFun = 
        "fun(Data) ->"
        "ioformat(\"~p~n\", [)"
        "end.",
    Merge = [{beginfun, ""}, {workfun, WorkFun}, {endfun, EndFun}, {comment, ""}],

    Options = [{overload, "[]"}, 
               {name, "TestValidate"}, 
               {output_file, "file.txt"}],

    ?assertMatch({error, _}, 
                 config_generator:validate(Patterns, Nodes, Flags, [Merge], Options)).
