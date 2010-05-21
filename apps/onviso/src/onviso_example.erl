%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%% @doc <p>Onviso_example contains a number of examples on how merging 
%% using {@link onviso:merge/4. 'onviso:merge/4'} can be conducted. 
%% The examples in this module is by no means conclusive but aims to 
%% help new users of Onviso or Inviso grasp the possibilities that 
%% merging traces provides. It is fully possible to build customised 
%% profiling tools and property checking tools based solely on the traces
%% generated and the merge function.</p>
%% 
%% <p>Please have a look at the source code of this file to understand the
%% examples.</p>
%% 
%%% Version: 1.0

%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met: * Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer. * Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution. * Neither the name of the Erlang Training & Consulting nor the names of its
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
-module(onviso_example).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

-export([profiling_count_function_calls/1,
         profiling_count_function_calls_shell/1,
         profiling_time_function_calls/1,
         node_trace_counter/1,
         overload_protection/0]).

-record(mdata, {calls, 
               results}).


%%--------------------------------------------------------------------
%% @doc This example implements a function call counter based on traces
%% generated. All results are stored as tuples in the handler data, 
%% which is a list.
%% 
%% <p>Each trace is evaluated using a separate internal function. Since
%% the format of the incoming traces are not always the same, the 
%% <code>workerfun</code> must match several heads. Everything which is 
%% not a function call is however ignored.</p>
%% @end
%%--------------------------------------------------------------------
-spec profiling_count_function_calls(integer()) -> {ok, integer()}.
profiling_count_function_calls(Ref) ->
    onviso:merge(Ref, void, fun workerfun/4, file).

%%--------------------------------------------------------------------
%% @doc Same as the example above except that it sends all traces to 
%% the shell when merging is complete. Note that merging and sending 
%% traces to shell is not the same as directly relaying them to to a 
%% collector node. Here the merging will take place as defined first. 
%% @end
%%--------------------------------------------------------------------
-spec profiling_count_function_calls_shell(integer()) -> {ok, integer()}.
profiling_count_function_calls_shell(Ref) ->
    onviso:merge(Ref, void, fun workerfun/4, shell).

%% Work fun used by function count profiler
workerfun(_Node, {_TraceInfo,_Pid,call,{Mod, Fun, _}, _Time}, 
          _Pidmappings, HandlerData) ->
    evaluate({Mod, Fun}, HandlerData);
workerfun(_Node, {_TraceInfo,_Pid,call,{Mod, Fun, _}, _Caller, _Time}, 
          _Pidmappings, HandlerData) -> 
    evaluate({Mod, Fun}, HandlerData);
workerfun(_Node, _Trace, _PidMappings, HandlerData) ->
    {ok, HandlerData}.

%% Helper function for profiling_count_function_calls
evaluate(ModFun, HandlerData) ->
    NewData = case lists:keyfind(ModFun, 1, HandlerData) of
                  {Function, Count} ->
                      lists:keyreplace(Function, 1, 
                                       HandlerData,
                                       {Function, Count+1});
                  false ->
                      [{ModFun, 1}|HandlerData]
              end,
    {ok, NewData}.


%%--------------------------------------------------------------------
%% @doc This example demonstrates how traces and their return signals
%% can be used to implement a simple profiler by measuring the execution  
%% time of each function call.
%% 
%% <p>This is done by storing each 'call' and the time when it was 
%% generated and comparing it with the 'return_from' trace's time. The
%% list of processed calls are stored in a record which is passed in
%% as the handler data. </p>
%%
%% <pre>
%% -record(mdata, {calls, results}).
%% </pre>
%%
%% <p>
%% The same record also stores the evaluated results in a separate list.</p>
%% <p>Although the <code>WorkFun</code> doesn't necessarily classify as
%% beautiful code, it demonstrates that quite a lot can be done without 
%% help from external modules or functions. </p>
%%
%% <p><strong>Note:</strong> this may neither be the most accurate nor 
%% most efficient way of profiling function calls. </p>
%% @end
%%--------------------------------------------------------------------
-spec profiling_time_function_calls(integer()) -> {ok, integer()}.
profiling_time_function_calls(Ref) ->
    BeginFun = 
        fun(_HandlerData) ->
                NewData = #mdata{calls = [],
                                results = []},
                {ok, NewData}
        end,

    WorkerFun = 
        fun(_Node, {trace_ts, Pid, call, {Mod, Fun, _Arity},
                    {MegaSeconds, Seconds, MicroSeconds}}, 
            _PidMappings, HandlerData) ->
                Call = {{Mod, Fun, Pid}, 
                        (MicroSeconds + 
                         (Seconds * 1000000) + 
                         (MegaSeconds * 1000000000000))},

                NewCallList = [Call|HandlerData#mdata.calls],
                NewData = HandlerData#mdata{calls = NewCallList},
                {ok, NewData};

           (_Node, {trace_ts, Pid, return_from, {Mod, Fun, _Arity}, _Return,
                    {MegaSeconds, Seconds, MicroSeconds}}, 
            _PidMappings, HandlerData) ->
                FinishTime = (MicroSeconds + 
                              (Seconds * 1000000) + 
                              (MegaSeconds * 1000000000000)),

                case lists:keyfind({Mod, Fun, Pid}, 1, HandlerData#mdata.calls) of
                    {Call, StartTime} ->
                        RunTime = FinishTime - StartTime,
                        NewResults = [{Call, RunTime}|HandlerData#mdata.results],
                        NewCallList = lists:keydelete(Call, 1, 
                                                      HandlerData#mdata.calls),
                        NewData = #mdata{calls = NewCallList, 
                                        results = NewResults},
                        {ok, NewData};
                    false ->
                        {ok, HandlerData} % function called before trace started
                end;
           (_Node, _Trace, _Other, HandlerData) ->
                {ok, HandlerData} % Unhandled message, carry on
        end,

    EndFun = 
        fun(#mdata{results = Results}) ->                   
                {ok, OutDevice} = file:open("profiling.txt", [write]),
                [io:format(OutDevice, "~p~n", [Out]) || Out <- Results],
                file:close(OutDevice),
                ok
        end,
    onviso:merge(Ref, BeginFun, WorkerFun, EndFun).

%%--------------------------------------------------------------------
%% @doc This function summarises the traces by node.  
%% @end.
%%--------------------------------------------------------------------
-spec node_trace_counter(integer()) -> {ok, integer()}.    
node_trace_counter(Ref) ->    
    WorkFun = 
        fun(Node, _Trace, _PidMapping, Data) ->
                NewData = case lists:keyfind(Node, 1, Data) of
                               {Node, Count} ->
                                   lists:keyreplace(Node, 1, Data, 
                                                    {Node, Count+1});
                               false ->
                                   [{Node, 1}|Data]
                           end,        
                {ok, NewData}
        end,
    
    EndFun = fun(Data) ->
                     io:format("Distribution of traces: ~n~p~n", [Data])
             end,

    onviso:merge(Ref, void, WorkFun, EndFun).
                           

%%--------------------------------------------------------------------
%% @doc In this example, a trace is set-up with overload protection 
%% activated. The {@link overload_handler. 'overload_handler'} module
%% provides the three functions required when activating overload protection. 
%% @end.
%%--------------------------------------------------------------------
-spec overload_protection() -> {ok, integer()}.
overload_protection() ->    
    {ok, Ref} = onviso:trace([{heart, send_heart_beat, '_', []}],
                             [node()], {all, [call]}, 
                             [{overload, {{overload_handler, check},
                                          15000,
                                          {overload_handler, start, []},
                                          {overload_handler, stop, []}}}]),
    timer:sleep(30000),
    
    onviso:merge(Ref, void, void, shell).

