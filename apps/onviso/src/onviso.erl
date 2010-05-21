%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Bartlomiej Puzon <bartlomiej@erlang-solutions.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%% @doc <p>The onviso module facilitates a safe interface towards the 
%% Inviso application. </p>
%% <p>The most common use case of this module is:</p>
%% <pre>
%% 1. Start trace
%% 2. Define initial handler (BeginFun)
%% 3. Define work handler (WorkFun)
%% 4. Define post processing handler (EndFun)
%% 5. Merge 
%% </pre>
%% <p>Of course there maybe a number of variations to this, but it gives a 
%% brief introduction to this module's theoretical usage.</p>
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
-module(onviso). 
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

-export([trace/2, trace/3, trace/4, trace/5,
         stop/1,
         merge/4,merge/5,
         status/0,status/1,
         last_trace/0,
	 reconnect/1,
	 node_status/0,
	 do_reconnect/1,
         clean/0]).

-export([string2ms/1]).

-define(MS_RETURN_FROM, [{'_',[],[{return_trace}]}]).
-define(MS_CALLER, [{'_',[],[{message,{caller}}]}]).
-define(SETTINGS, onviso_settings).
-define(SETTINGS_FILE, onviso_settings.ets).

-record(trace_data, {patterns,
		     flags,
		     overload,
		     collector,
		     nodes,
		     options}).

-type o_node() :: atom() | {relayer, atom()} | {collector, atom()}.
-type vfun() :: void | fun().
-type endfun() :: shell | file | fun().

%%--------------------------------------------------------------------
%% @spec trace(Patterns, Nodes, Flags, OverloadProtection, Options) -> Result
%% 	Nodes = list(atom()) | list(tuple(NodeType, atom()))
%%        NodeType = relayer | collector
%% 	Patterns = {Module, Function, Arity, RestrictedMatchSpec}
%%      Arity = list()
%%      RestrictedMatchSpec = return | caller | {fun2ms, FunStr} | [] | MS
%%      FunStr = string()
%%      Flags = tuple()
%%      OverloadProtection = list()
%%      Options = [{autoresume, AutoresumeOptions}]
%% 	Result = {ok, int()} | {error, Reason}
%%      Reason = no_connection | node_format | pattern_format | flag_format |
%%               no_meta_tracer | node_specification
%% 
%% @doc <p>Set-up and start a trace. Use 
%%      merge/4-5 to retreive and merge. Trace will automatically try 
%%      to connect to the nodes specified in <code>Nodes</code> and fail
%%      if the nodes does not share the same magic cookie.</p> 
%%      <p>Returns the reference as an integer which is used by merge/4-5.</p>
%%      <p>If more than one error occurs, the first error is the one returned.</p>
%%      <p>The following <code>RestrictedMatchSpecs</code> are recognised:
%%      <ul>
%%       <li><code>return</code> - equivalent to [{'_', [], [{return_trace}]}]</li>
%%       <li><code>caller</code> - equivalent to [{'_', [], [{message, {caller}}]}]</li>
%%       <li><code>{fun2ms, FunStr}</code> - where <code>FunStr</code> is a string with
%%       a fun body as required by dbg:fun2ms/1</li>
%%      </ul>
%%      Any other value will be trated as a literal match specification</p>
%%      <p>The autoresume feature (onviso_monitor) can be controlled
%%       using the Options paramenter.
%%      See {@link onviso_monitor:configure/1.} for possible options.
%%      By default, if a node goes down, onviso will try to reestablish the connection
%%      and reinitialise tracing. The monitor will use the default interval for
%%      each node without an interval specified unless the autoresume is disabled.
%%      With <code>resume_nodes</code> it is possible to pass a list specyfying the intervals
%%      for each node. The interval of <code>0</code> will disable autoresuming for
%%      a node:
%%
%%      <pre>
%%      {autoresume, [{nodes, [{alice@server, 1200}, {bob@server, 0}]}]}
%%      </pre>
%%      Nodes omitted in the list will use the default interval</p>
%%      <p>For example, a trace may look like this:</p>
%% 
%%      <pre>
%% 1> trace([{io, format, '_', []}], ['server@linux'], {all, [call]}, []).
%% {ok, 1}
%%      </pre>
%% 
%%      <p>When a trace is started using the <code>NodeType</code> it is not 
%%      possible to merge the trace as all traces are directly relayed to the 
%%      shell of the collecting node. To set-up a direct relay of traces to
%%      the control node, do the following: </p>
%% 
%%      <pre>
%% 2> trace([{io, format, '_', []}], [{relayer, 'server@linux'},
%%                                    {collector, node()}],
%%           {all, [call]}, []).
%% {ok, 2}
%%      </pre>
%% @end
%%--------------------------------------------------------------------
-spec trace(list(), list(), tuple(), list(), list()) -> {ok, integer()} | {error, term()}.
trace(Patterns, Nodes, Flags, OverloadProtection, Options) ->
    init_onviso(),
    stop_previous_traces(),
    init_inviso(),

    Ref = generate_unique_key(),
    OutputDir = onviso_file_helper:output_dir(Ref),
    onviso_monitor:configure(proplists:get_value(autoresume, Options, [])),
    case get_tracedata(Patterns, Flags, OverloadProtection, Nodes, Options) of
	{ok, TData} ->
	    %%If the output directory can't be created, it's better to report it
	    %%before the tracing actually starts
	    case file:make_dir(OutputDir) of
		ok ->
		    add_to_onviso(Ref, TData),
		    Result = [ add_node(TData, Node, Ref) 
			       || Node <- TData#trace_data.nodes ],
		    case lists:dropwhile(fun(ok) -> true;
					    (_) -> false
					 end, Result) of
			[] ->
			    onviso_monitor:run(),
			    {ok, Ref};
			[Error | _] ->
			    %%If we're here, we can safely delete the directory,
			    %%as we have create it
			    file:del_dir(OutputDir),
			    remove_from_onviso(Ref),
			    clean_up(),
			    Error
		    end;
		{error, Reason} ->
		    {error, {Reason, OutputDir}}
	    end;
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @spec trace(Patterns, Nodes, Flags, OverloadProtection) -> Result
%% 	Nodes = list()
%% 	Patterns = list()
%%      Flags = tuple()
%%      OverloadProtection = list()
%% 	Result = {ok, integer()} | {error, Reason}
%%      
%% 
%% @doc Set-up and start a trace using default options.
%%      For all error reasons see {@link trace/5. 'trace/5'}.
%% @end
%%--------------------------------------------------------------------
-spec trace(list(), list(), tuple(), list()) -> {ok, integer()} | {error, term()}.
trace(Patterns, Nodes, Flags, OverloadProtection) ->
    trace(Patterns, Nodes, Flags, OverloadProtection, []).

%%--------------------------------------------------------------------
%% @spec trace(Patterns, Nodes, Flags) -> Result
%% 	Nodes = list()
%% 	Patterns = list()
%%      Flags = term()
%% 	Result = {ok, integer()} | {error, Reason}
%%      
%% 
%% @doc Set-up and start a trace using default overload protection.
%%      For all error reasons see {@link trace/4. 'trace/4'}.
%% @end
%%--------------------------------------------------------------------
-spec trace(list(), list(), term()) -> {ok, integer()} | {error, term()}.
trace(Patterns, Nodes, Flags) ->
    ?MODULE:trace(Patterns, Nodes, Flags, []).

%%--------------------------------------------------------------------
%% @spec trace(Patterns, Flags) -> Result
%% 	Patterns = list()
%%      Flags = term()
%% 	Result = {ok, integer()} | {error, Reason}
%%      
%% @doc Set-up and start a trace using default overload protection and
%%      on the local node only. 
%%      For all error reasons see {@link trace/4. 'trace/4'}.
%% @end
%%--------------------------------------------------------------------
-spec trace(list(), tuple(atom(), list())) -> {ok, integer()} | {error, term()}.
trace(Patterns, Flags) ->
    ?MODULE:trace(Patterns, [node()], Flags, []).
    
%%--------------------------------------------------------------------
%% @spec stop(integer()) -> Result
%%      Result = ok | {error, no_trace_running}
%% @doc Retrieve the log files to the control node. 
%% @end
%%--------------------------------------------------------------------
-spec stop(integer()) -> ok | {error, no_trace_running}.
stop(Ref) ->
    case onviso:status(Ref) of
	{ok, Data} ->
	    TData = element(3, Data),
	    %%The following may be needed if the node has been restarted
	    %%It will shut down all inviso_rt as long as the network is
	    %%not partitioned
	    inviso:start(),
	    OnlyNodes = extract_nodes(TData#trace_data.nodes),
	    inviso:add_nodes(OnlyNodes, inviso, []),

	    %%Now we can try to stop the tracing
	    case catch inviso:stop_tracing() of
		{ok, _} ->
		    case catch ets:member(?SETTINGS, Ref) of
			true ->
			    mark_as_stopped(Ref),
			    retrieve_logs(Ref),
			    clean_up(); % in case this is the last command exectuted
			_ ->
			    {error, no_trace_running}
		    end;
		_ -> 
		    {error, no_trace_running}
	    end;
	_ ->
	    {error, no_trace_running}
    end.
	
%%--------------------------------------------------------------------
%% @spec merge(Ref, BeginFun, WorkFun, EndFun) -> Result
%%      BeginFun = void | fun(InitHandlerData)
%%      WorkFun = void | fun(Node, Trace, PidMapping, HandlerData)
%%        Node = node()
%%        Trace = {trace_ts, pid(), Type, {Module, Function, Arity}, Time}
%%          Type = call | return_from
%%          Module = atom()
%%          Function = atom()
%%          Arity = list()
%%          Time = now()
%%      EndFun = shell | file | fun()
%%      Result = {ok, Count} | {error, Reason}
%%        Count = integer()
%%        Reason = no_trace_running | no_file
%% 
%% @doc Merge and analyse log files. If they haven't been retrieved yet 
%%      this function will do it for you. Note that all Funs must match 
%%      Inviso specification. Default <code>HandlerData</code> is
%%       <code>[]</code>but can be 
%%      overridden with merge/4, however, then all funs must be manually
%%      provided.
%% @end
%%--------------------------------------------------------------------
-spec merge(integer(), vfun(), vfun(), endfun()) -> {ok, integer()} | {error, term()}.
merge(Ref, BeginFun, WorkFun, EndFun) ->
    merge(Ref, BeginFun, WorkFun, EndFun, []).

%%--------------------------------------------------------------------
%% @doc Same as merge/4 but possible to specify HandlerData structure
%% @end
%%--------------------------------------------------------------------
-spec merge(integer(), vfun(), vfun(), endfun(), list()) -> {ok, integer()} | {error, term()}.
merge(Ref, void, WorkFun, EndFun, Handler) ->
    merge(Ref, fun(Data) -> {ok, Data} end, WorkFun, EndFun, Handler);
merge(Ref, BeginFun, void, EndFun, Handler) ->
    StandardWork = 
        fun(Node, Trace, Pidmappings, HandlerData) ->
                {ok, [{Node, Trace, Pidmappings}|HandlerData]}
        end,
    merge(Ref, BeginFun, StandardWork, EndFun, Handler);
merge(Ref, BeginFun, WorkFun, shell, Handler) ->
    Shell = fun(Data) ->
                    io:format("~n~p~n", [Data])
            end,
    merge(Ref, BeginFun, WorkFun, Shell, Handler);   
merge(Ref, BeginFun, WorkFun, file, Handler) ->
    File = fun(Data) -> 
                   {ok, OutDevice} = file:open("output" ++ 
                                               integer_to_list(Ref) ++
                                               ".txt", [write]),
                   [io:format(OutDevice, "~p~n", [ToPrint]) || ToPrint <- Data],
                   file:close(OutDevice),
                   ok
           end,
    merge(Ref, BeginFun, WorkFun, File, Handler);
merge(Ref, BeginFun, WorkFun, {file, Name}, Handler) when is_list(Name)->
    File = fun(Data) -> 
                   {ok, OutDevice} = file:open(Name, [write]),
                   [io:format(OutDevice, "~p~n", [ToPrint]) || ToPrint <- Data],
                   file:close(OutDevice),
                   ok
           end,
    merge(Ref, BeginFun, WorkFun, File, Handler);
merge(Ref, BeginFun, WorkFun, {file_prefix, Name}, Handler) when is_list(Name)->
    File = fun(Data) -> 
                   {ok, OutDevice} = file:open(Name ++ 
                                               integer_to_list(Ref) ++
                                               ".txt", [write]),
                   [io:format(OutDevice, "~p~n", [ToPrint]) || ToPrint <- Data],
                   file:close(OutDevice),
                   ok
           end,
    merge(Ref, BeginFun, WorkFun, File, Handler);
merge(Ref, BeginFun, WorkFun, EndFun, Handler) ->
    stop_previous_traces(),
    case status(Ref) of
        {ok, Data} ->
            do_merge(Ref, 
                     element(4, Data),
                     BeginFun, 
                     WorkFun, 
                     EndFun, 
                     Handler);
        _ ->
            {error, no_trace_running}
    end.

%%--------------------------------------------------------------------
%% @spec status(integer()) -> list() | {error, no_trace_running}
%% @doc Retrieve a list of the current settings that Inviso are 
%%      operating on. If no trace exists by that reference, then 
%%      {error, no_trace_running} is returned. 
%% @end
%%--------------------------------------------------------------------
-spec status(integer()) -> {ok, tuple()} | {error, no_trace_running}.
status(Ref) ->
    case settings_exist() of
        undefined ->
            {error, no_trace_running};
        _Ok ->            
	    case ets:lookup(?SETTINGS, Ref) of
		[Reply] ->
		    {ok, Reply};
		[] ->
		    {error, no_trace_running}
	    end
    end.    

-spec status() -> {ok, list()} | {error, no_trace_running}.
status() ->
    case settings_exist() of
        undefined ->
            {error, no_trace_running};
        _ok ->
            Reply = ets:match(?SETTINGS, '$1'),
            {ok, [Data || [Data] <- Reply]}
    end.

-spec last_trace() -> {ok, integer()} | {error, no_trace_running}.
last_trace() ->            
    case settings_exist() of
        undefined ->
            {error, no_trace_running};
        _Ok ->	  
	    case ets:last(?SETTINGS) of
		'$end_of_table' ->
		    {error, no_trace_running};
		Ref ->
		    {ok, Ref}
	    end
    end.

%%--------------------------------------------------------------------
%% @spec clean() -> ok
%% @doc Manually clear all settings. This always succeeds.
%% @end
%%--------------------------------------------------------------------
-spec clean() -> ok.
clean() ->
    clean_up_onviso(),
    catch clean_up(),
    ok.

%%--------------------------------------------------------------------
%% @spec reconnect(Node) -> Result
%% 	Node = atom()
%% 	Result = ok| {error, Reason}
%%      
%% @doc Try to resume tracing on a specified node. This may be used to
%%      resume trace on a node when the autoresume feature is turned off
%%      or to force a faster reconnect
%% @end
%%--------------------------------------------------------------------
-spec reconnect(atom()) -> ok | {error, term()}.
reconnect(Node) ->
    onviso_monitor:reconnect(Node).

%%--------------------------------------------------------------------
%% @spec node_status() -> Result
%% 	Result = [{Node, active} | {Node, inactive}]
%%      Node = atom()
%% 
%% @doc Returns a list of nodes and the inviso_rt status on each
%% @end
%%--------------------------------------------------------------------
-spec node_status() -> [{atom(), active | inactive}].
node_status() ->
    onviso_monitor:status().

%%====================================================================
%% Internal Functions
%%====================================================================

-define(UPDATE_ETS(Command),
	ResultTKN = Command,
	store_settings(),
	ResultTKN).

%% Set-up ETS tables for persistant storage
init_onviso() ->
    onviso_monitor:start_link(),
    case settings_exist() of 
        undefined ->
	    ets:new(?SETTINGS, [named_table, public, ordered_set]);
        _Else ->
            ok
    end.

%%Adds data to an ETS table
%%{ID, state, trace data, run counters (incremented if
%% a node had to be restarted}
add_to_onviso(Ref, TData) ->
    ?UPDATE_ETS(ets:insert(?SETTINGS, [{Ref, running, TData, dict:new()}])).

-spec remove_from_onviso(integer()) -> true.
remove_from_onviso(Ref) ->
    ?UPDATE_ETS(ets:delete(?SETTINGS, Ref)).

%%
generate_unique_key() ->
    Info = ets:info(?SETTINGS),
    {size, Size} = lists:keyfind(size, 1, Info),
    Size+1. % sufficiently unique unless someone screws with table manually
    
%% Start necessary apps for inviso
init_inviso() ->
    application:start(runtime_tools),
    inviso:start().

%% Nodes handling inviso. 
add_node_to_onviso(Node, OverloadProtection) ->
    case net_kernel:connect(Node) of
        true ->
	    case inviso:add_nodes([Node], inviso, OverloadProtection) of
		{ok, _} ->
		    ok;
		Other ->
		    {error, {node_format, Other}}
	    end;
        false ->
            {error, no_connection}
    end.

extract_nodes(Nodes) ->
    lists:map(fun({_Type, Node}) ->                      
                      Node;
                 (Node) ->
                      Node
              end, Nodes).

%%Converts trace() arguments to a proper #trace_data{}
-spec get_tracedata(list(), tuple(), list(), list(), list()) -> 
			   {ok, #trace_data{}} | {error, node_specification}.
get_tracedata(Patterns, Flags, OverloadProtection, Nodes, Options) ->
    case {proplists:is_defined(relayer, Nodes),
	  proplists:is_defined(collector, Nodes)} of
	{true, true} ->
	    %% "collectors" before "relayers".
	    %% This order is needed if we want to add nodes to onviso
	    %% one by one
	    SortedNodes = lists:keysort(1, Nodes),
	    {ok, #trace_data{patterns = Patterns,
			     flags = Flags,
			     overload = OverloadProtection,
			     collector = proplists:get_value(collector, Nodes),
			     nodes = SortedNodes,
			     options = Options}};
	{false, true} ->
	    {error, node_specification};     
	{true, false} ->
	    {error, node_specification};
	_AssumeOK ->
	    {ok, #trace_data{patterns = Patterns,
			     flags = Flags,
			     overload = OverloadProtection,
			     nodes = Nodes,
			     options = Options}}
    end.

-spec add_node(#trace_data{}, o_node(), integer()) -> ok | {error, term()}.
add_node(TData, Node, Ref) ->
    RunNr = get_counter(Ref, Node) + 1,
    case do_add_node(TData, Node, Ref, RunNr) of
	ok ->
	    inc_counter(Ref, Node),
	    ok;
	Error ->
	    Error
    end.

%%Initializes a runtime component. We use Ref and RunNumber to create file name extensions
-spec do_add_node(#trace_data{}, o_node(), integer(), integer()) -> ok | {error, term()}.
do_add_node(TData, Node, Ref, RunNumber) ->

    [OneNode] = extract_nodes([Node]),
    AddResult = add_node_to_onviso(OneNode, TData#trace_data.overload),

    InitResult = case initialise_trace(Ref, TData, Node, RunNumber) of
                     {ok, {normal, _}} ->
                         link_localnames(OneNode);
                     {ok, {relay, _}} ->
                         ok;
                     Error ->
                         Error
                 end,
    PatternResult = add_patterns(OneNode, TData#trace_data.patterns),
    FlagResult = add_flags(OneNode, TData#trace_data.flags),

    case {AddResult, InitResult, PatternResult, FlagResult} of
        {ok, ok, ok, ok} ->
	    case RunNumber of
		1 ->
		    %%We need to add the node to the monitor
		    onviso_monitor:add(OneNode);
		_ ->
		    ok
	    end,
	    ok;
        Errors ->
            [FirstError|_] = lists:filter(fun(Reason) ->
                                                  Reason /= ok
                                          end, tuple_to_list(Errors)),     
	    FirstError
    end.
		   
%% Set trace information 
-spec initialise_trace(integer(), #trace_data{}, o_node(), integer()) ->
			      {ok, {normal | relay, term()}} | {error, term()}.
initialise_trace(Ref, TData, Node, RunNumber) ->

    case TData#trace_data.collector of
        undefined ->            
	    Ti = onviso_file_helper:get_trace_setup(Node, Ref, RunNumber),
	    try_init_tracing(normal, Ti);
        CollectorNode ->
	    case Node of
		{relayer, N} ->
		    TiRelays = [{N, [{trace, {relayer, CollectorNode}}]}];
		{collector, N} ->
		    TiRelays = [{N, [{trace, collector}]}]
	    end,
	    try_init_tracing(relay, TiRelays)
    end.

-spec try_init_tracing(atom(), list()) -> {ok, {atom(), list()}} | {error, term()}.
try_init_tracing(Type, Data) ->
    case inviso:init_tracing(Data) of
	{ok, _} ->
	    {ok, {Type, Data}};
	Error ->
	    Error
    end.
    

%% Map registered processes to their names
link_localnames(Node) ->
    case inviso:tpm_localnames([Node]) of
	{ok, _} ->
	    ok;
	Other ->
	    {error, {tpm, Other}}
    end.

%% Set the trace patterns
add_patterns(_Node, []) ->
    ok;
add_patterns(Node, [{Module, Function, Arity, return}|Patterns]) ->
    % exchange shortcut 'return' for real match spec
    add_pattern(Node, {Module, Function, Arity, ?MS_RETURN_FROM}),
    add_patterns(Node, Patterns);
add_patterns(Node, [{Module, Function, Arity, caller}|Patterns]) ->
    % exchange shortcut 'caller' for real match spec
    add_pattern(Node, {Module, Function, Arity, ?MS_CALLER}),
    add_patterns(Node, Patterns);
add_patterns(Node, [{Module, Function, Arity, {fun2ms, MatchSpec}}|Patterns]) ->
    case string2ms(MatchSpec) of
	{ok, MS} ->
	    add_pattern(Node, {Module, Function, Arity, MS}),
	    add_patterns(Node, Patterns);
	{error, _} ->
	    {error, pattern_format}
    end;
add_patterns(Node, [{_Module, _Function, _Arity, _MatchSpec} 
                     = Pattern|Patterns]) ->
    add_pattern(Node, Pattern),
    add_patterns(Node, Patterns);
add_patterns(_Node, _IncorrectFormat) ->
    {error, pattern_format}.

add_pattern(Node, {Module, Function, Arity, MatchSpec}) ->
    inviso:tpl([Node], Module, Function, Arity, MatchSpec).

%% Set trace flags
add_flags(Node, {Scope, Flags}) when is_list(Flags) ->
    case inviso:tf([Node], Scope, lists:append([timestamp], Flags)) of
	{ok, _} ->
	    ok;
	Other ->
	    {error, {flag_format, Other}}
    end.

%% Nice and tidy!  
clean_up() ->
    onviso_monitor:clean(),
    inviso:stop_tracing(),
    inviso:stop_nodes(),
    inviso:stop().

clean_up_onviso() ->
    onviso_file_helper:purge_logs(),
    file:delete(?SETTINGS_FILE),
    catch ets:delete(?SETTINGS).

%%
stop_previous_traces() ->
    case last_trace() of
        {ok, Ref} ->
            Ref,
            stop(Ref);
        _ -> 
            ok
    end.

%% 
mark_as_stopped(Ref) ->
    ?UPDATE_ETS(ets:update_element(?SETTINGS, Ref, {2, stopped})).

%% Retrieve logs from nodes
%% FileData = [{trace_log,Dir,FileList},{ti_log,Dir,FileList}]
retrieve_logs(Ref) ->
    {ok, Value} = status(Ref),
    Counters = element(4, Value),
    OnlyNodes = dict:fetch_keys(Counters),
    FileData = onviso_file_helper:get_files_to_fetch(OnlyNodes,
						     Ref,
						     Counters),
    OutputDir = onviso_file_helper:output_dir(Ref),
    CopyTag = onviso_file_helper:copy_tag(),
    {ok, Results} = inviso:fetch_log(FileData, OutputDir, CopyTag),
    check_fetch_result(Results).

check_fetch_result([]) ->
    ok;
check_fetch_result([{_Node, {complete, _}}| List]) ->
    check_fetch_result(List);
check_fetch_result([{_Node, {error, own_node}}| List]) ->
    check_fetch_result(List); % ignore and take next
check_fetch_result([{_Node, _Err}| _List]) ->
    {error, no_file}.

%% Merge retrieved files according to funs. 
-spec do_merge(integer(), dict(), fun(), fun(), fun(), term()) ->
       {ok, integer()} | {error, term()}.
do_merge(Ref, Counters, BeginFun, WorkFun, EndFun, Handler) ->
    FileData = onviso_file_helper:get_files_to_merge(Ref, Counters),    
    inviso_lfm:merge(FileData, BeginFun, WorkFun, EndFun, Handler).

-spec do_reconnect(atom()) -> ok | {error, term()}.			  
do_reconnect(Node) ->
    case last_trace() of
	{ok, Ref} ->
	    {ok, {Ref, running, TData, _}} = status(Ref),
	    case add_node(TData, Node, Ref) of
		ok ->
		    ok;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

-spec get_counter(integer(), atom()) -> integer() | {error, no_trace_running}.
get_counter(Ref, Node) ->
    case status(Ref) of
	{ok, Status} ->
	    Counters = element(4, Status),
	    case dict:find(Node, Counters) of
		{ok, Value} ->
		    Value;
		error ->
		    0
	    end;
	Error ->
	    Error
    end.

-spec inc_counter(integer(), atom()) ->	boolean() | {error, no_trace_running}.
inc_counter(Ref, Node) ->
    case status(Ref) of
	{ok, Status} ->
	    Counters = element(4, Status),
	    D2 = dict:update_counter(Node, 1, Counters),
	    ?UPDATE_ETS(ets:update_element(?SETTINGS, Ref, {4, D2}));
	Error ->
	    Error
    end.

-spec store_settings() -> ok.
store_settings() ->
    ets:tab2file(?SETTINGS, ?SETTINGS_FILE).

-spec settings_exist() -> list() | undefined.		    
settings_exist() ->
    case ets:info(?SETTINGS) of
	undefined ->
	    case filelib:is_file(?SETTINGS_FILE) of
		true ->
		    ets:file2tab(?SETTINGS_FILE),
		    ets:info(?SETTINGS);
		false ->
		    undefined
	    end;
	Info ->
	    Info
    end.

-spec fix_dot(string()) -> string().
fix_dot(FunStr) ->
    [H | Rest]  = lists:reverse(FunStr),
    case H of
	$. ->
	    FunStr;
	H -> 
	    lists:reverse([$., H | Rest])
    end.

-spec string2ms(string()) -> {ok, list()} | {error, fun_format}.
string2ms(FunStr) ->
    case erl_scan:string(fix_dot(FunStr)) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_exprs(Tokens) of
		{ok, [Expression]} ->
		    case Expression of
			{_, _, {clauses, Clauses}} ->
			    {ok, ms_transform:transform_from_shell(dbg, Clauses, [])};
			_ ->
			    {error, fun_format}
		    end;
		_ ->
		    {error, fun_format}
	    end;
	_ ->{error, fun_format}
    end.

