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
%%This module contains the extentions to dbg. The contents
%%are to serve as a proof of concept and will be merged with
%%dbg later on.

-module(dbge).
-author('<protest@erlang-consulting.com>').


-export([run_trace/4,
	stop_trace/1,
	info_trace/1]).

%%This needs to be exported, but does not belong to the API
-export(['_send_file'/2]).

-type ext_ms() :: caller | return | {fun2ms, list()} | list().
-type trace_handler() :: list().

-define(MS_RETURN_FROM, [{'_',[],[{return_trace}]}]).
-define(MS_CALLER, [{'_',[],[{message,{caller}}]}]).

%%Consts for filenames
-define(DIR_PREFIX, "tdata").       %%Local directory to store trace data into
-define(DATA_FILE, "data").         %%File with trace details in DIR_PREFIX
-define(TRACEFILE_PREFIX, "trace"). %%Trace dumps prefix
-define(TRACEFILE_SUFFIX, ".trc").  %%Trace dumps suffix

%%Msg length for file fetcher
-define(READ_BYTES, 4096).
-define(IP_QUEUE, 50).

-define(DEST_FILE(Node, File),
	atom_to_list(Node) ++ "_" ++ File).

%%%%%%%%%%%%%%
%%API
%%%%%%%%%%%%%%
%%Options:
%%{trace_dir, Dir} - the directory for this trace
%%{data_handler, Type} - how to collect the results see dbg:trace_port for parameter desc.)
%%file handler allows different merges
%% Type = 
%%        process - the data will be shown on  to the console
%%        {process, Handler} - overwrite the default message handler
%%        ip - traces are sent through TCP/IP
%%        {ip, PortFun} - overwrite the default ports and queue sizes
%%        {ip, PortFun, Handler} - overwrite the default message handler
%%        file - traces are dumped to files on traced nodes and then fetched
%%           (this is the default)
%%        {file, {wrap, WrapSize, WrapCnt}} - log wrapper
%% Handler = {HandlerFun, InitValue}} - see dbg:trace/2
%% PortFun = fun (Node) -> PortNum | {PortNum, QueSize}
-spec run_trace(list(), list(), list(), list()) -> {ok, trace_handler()} | {error, term()}.
run_trace(Patterns, Nodes, Flags, Options) ->
    dbg:stop_clear(),
    %%Checking the patterns here we can find error before the tracing has started.
    FixedPatterns = lists:map(fun fix_pattern/1, Patterns),
    FixedOptions = fix_options(Options),
    FixedNodes = fix_nodes(Nodes),

    init_tracers(proplists:get_value(data_handler, FixedOptions), FixedNodes),
    %%We have to add timestamp to the tracing in order to
    %%merge correctly
    {Procs, Types} = Flags,
    apply(dbg, p, tuple_to_list({Procs, [timestamp | Types]})),
    [apply(dbg, tpl, tuple_to_list(Pattern)) || Pattern <- FixedPatterns],


    TraceDir = get_tracename(proplists:get_value(trace_dir, FixedOptions)),
    write_trace_data(TraceDir, [FixedPatterns, FixedNodes, Flags, FixedOptions]),
    {ok, TraceDir}.

-spec stop_trace(trace_handler()) -> ok.
stop_trace(THandler) ->
    [_, Nodes, _, Options | _] = read_trace_data(THandler),
    dbg:stop_clear(),

    ProcessType = proplists:get_value(data_handler, Options),
    case ProcessType of
	file ->
	    fetch_logs(THandler, Nodes, ProcessType);
	{file, _} ->
	    fetch_logs(THandler, Nodes, ProcessType);
	_ ->
	    %%We don't need to fetch anything here
	    ok
    end.

-spec info_trace(trace_handler()) -> {ok, term()}.
info_trace(THandler) ->
    {ok, read_trace_data(THandler)}.

						
%%%%%%%%%%%%%%%%
%%Helpers
%%%%%%%%%%%%%%%%
-spec fetch_logs(list(), list(), tuple()) -> ok.
fetch_logs(DestDir, Nodes, ProcessType) ->
    FileNames = get_lognames(ProcessType),
    lists:foreach(fun(Node) ->
			  VerifiedFileNames = existing_files(Node, FileNames),
			  [do_fetch_file(Node, File, DestDir) || File <- VerifiedFileNames]
		  end, Nodes).


-spec do_fetch_file(atom(), list(), list()) -> term().
do_fetch_file(Node, File, DestDir) when Node == node()->
    Dst = filename:join(DestDir, ?DEST_FILE(node(), File)),
    file:copy(File, Dst);
		      
do_fetch_file(Node, File, DestDir) ->
    Dst = filename:join(DestDir, ?DEST_FILE(Node, File)),
    {ok, IoDev} = file:open(Dst, [write, raw, delayed_write]),
    Pid = spawn(Node, dbge, '_send_file', [self(), File]),
    Pid ! start,
    write_loop(IoDev),
    file:close(IoDev),
    ok.

-spec write_loop(pid()) -> ok.
write_loop(IoDev) ->
    receive
	{data, Data} ->
	    file:write(IoDev, Data),
	    write_loop(IoDev);
	eof ->
	    ok
    end.

%%This won't be picked up by Cover, as it is invoked on other nodes.
-spec '_send_file'(pid(), list()) -> ok.
'_send_file'(Receiver, File) ->
    receive start -> ok end,
    {ok, IoDev2} = file:open(File, [read, raw, read_ahead]),
    read_loop(Receiver, IoDev2),
    file:close(IoDev2).

-spec read_loop(pid(), pid()) -> eof.		       
read_loop(Receiver, IoDev) ->
    case file:read(IoDev, ?READ_BYTES) of
	{ok, Data} ->
	    Receiver ! {data, Data},
	    read_loop(Receiver, IoDev);
	eof ->
	    Receiver ! eof
    end.
	    
-spec existing_files(atom(), list()) -> list().
existing_files(Node, FileNames) ->
    {ok, ExistingFiles} = case Node == node() of
			      true ->
				  file:list_dir(".");
			      false ->
				  rpc:call(Node, file, list_dir, ["."])
			  end,
    lists:filter(fun(FName) ->
			 lists:member(FName, ExistingFiles)
		 end, FileNames).
    
-spec get_lognames(file | {file, tuple()}) -> list(). 			  
get_lognames(file) ->
    [?TRACEFILE_PREFIX ++ ?TRACEFILE_SUFFIX];

get_lognames({file, {wrap, _Size, Cnt}}) ->
    [?TRACEFILE_PREFIX ++ integer_to_list(N) ++ ?TRACEFILE_SUFFIX
     || N <- lists:seq(0, Cnt)].
			
-spec init_tracers(list(), list()) -> list().
init_tracers(process, Nodes) ->
    init_tracers({process, {fun dhandler/2, user}}, Nodes);

init_tracers({process, {_Fun, _Initval} = Handler}, Nodes) ->
    %%We can still use dbg:n/1 with process handler
    dbg:tracer(node(), process, Handler),
    [dbg:n(Node) || Node <- Nodes, Node /= node()];

init_tracers(file, Nodes) ->
    init_tracers({file, nowrap}, Nodes);

init_tracers({file, WrapSettings}, Nodes) ->
    try_init_local(Nodes),
    TracePortFun = tport_file({file, WrapSettings}),
    [dbg:tracer(Node, port, TracePortFun) || Node <- Nodes ];

init_tracers(ip, Nodes) ->
    init_tracers({ip, fun dport_fun/1}, Nodes);

init_tracers({ip, PortFun}, Nodes) when is_function(PortFun) ->
    init_tracers({ip, PortFun, {fun dhandler/2, user}}, Nodes);

init_tracers({ip, PortFun, {_Fun, _InitVal} = Handler}, Nodes) ->
    try_init_local(Nodes),
    lists:foreach(fun(Node) ->
			  PortSpec = PortFun(Node),
			  {ok, Node} = dbg:tracer(Node, port, dbg:trace_port(ip, PortSpec)),
			  {ok, Port} = dbg:trace_port_control(Node, get_listen_port),
			  {ok, Hostname} = rpc:call(Node, inet, gethostname, []),
			  dbg:trace_client(ip, {Hostname, Port}, Handler)
		  end, Nodes).

-spec try_init_local(list()) -> term().
try_init_local(Nodes) ->
    %%If the local node belongs to the nodes list, we don't have to 
    %%start additional tracer
    case hd(Nodes) == node() of
	true ->
	    ok;
	false ->
	    dbg:tracer()
    end.

-spec tport_file({file, term()}) -> fun().			
tport_file({file, nowrap}) ->
    dbg:trace_port(file, ?TRACEFILE_PREFIX ++ ?TRACEFILE_SUFFIX);

tport_file({file, {wrap, WrapSize, WrapCnt}}) ->
    dbg:trace_port(file, {?TRACEFILE_PREFIX,
			  wrap,
			  ?TRACEFILE_SUFFIX,
			  WrapSize,
			  WrapCnt}).
    
-spec write_trace_data(list(), term()) -> term().
write_trace_data(Dir, Data) ->
    ok = file:make_dir(Dir),
    file:write_file(filename:join(Dir, ?DATA_FILE), term_to_binary(Data)),
    Data.
    
-spec read_trace_data(list()) -> term().
read_trace_data(Dir) ->
    {ok, Binary} = file:read_file(filename:join(Dir, ?DATA_FILE)),
    binary_to_term(Binary).

-spec get_tracename(default | list()) -> list().
get_tracename(default) ->
    {A, B, C} = now(),
    Tail = lists:foldl(fun(Item, Acc) ->
			       lists:append([Acc, "_", integer_to_list(Item)])
		       end, "", [A, B, C]),
    ?DIR_PREFIX ++ Tail;

get_tracename(Name) when is_list(Name)->
    Name.
    
-spec fix_dot(string()) -> string().
fix_dot(FunStr) ->
    [H | Rest]  = lists:reverse(FunStr),
    case H of
	$. ->
	    FunStr;
	H -> 
	    lists:reverse([$., H | Rest])
    end.

%%Parses the provided fun code and creates a match spec like fun2ms
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

-spec fix_pattern({atom(), atom(), atom(), ext_ms()}) -> {atom(), atom(), atom(), list()}.
fix_pattern({Mod, Fun, Args, caller}) ->
    {Mod, Fun, Args, ?MS_CALLER};
fix_pattern({Mod, Fun, Args, return}) ->
    {Mod, Fun, Args, ?MS_RETURN_FROM};
fix_pattern({Mod, Fun, Args, {fun2ms, FunStr}}) when is_list(FunStr) ->
    {Mod, Fun, Args, string2ms(FunStr)};
fix_pattern({Mod, Fun, Args, MS}) when is_list(MS) ->
    {Mod, Fun, Args, MS}.

-spec fix_options(list()) -> list().
fix_options(Options) ->
    Defaults = [{trace_dir, default},
		{data_handler, file}],
    lists:foldl(fun({K, V}, Acc) ->
			lists:keystore(K, 1, Acc, {K,V})
		end, Defaults, Options).

-spec fix_nodes(list()) -> list().
fix_nodes(Nodes) ->
    case lists:member(node(), Nodes) of
	true ->
	    [node() | lists:delete(node(), Nodes)];
	false ->
	    Nodes
    end.

-spec nodify(pid()) -> list().
nodify(Pid) ->    
    case node(Pid) == node() of
	false ->
	    atom_to_list(node(Pid)) ++ ":" ++ pid_to_list(Pid);
	true ->
	    pid_to_list(Pid)
    end.

-spec dport_fun(atom()) -> {integer(), integer()}.
dport_fun(_) ->
    %%Having "0" as a port number is possible because of the underlying driver
    %%implementation (it uses bind(), which treats '0' as 'any available'). Despite
    %%the implementation dependency, it is really helpful to do it that way.
    {0, ?IP_QUEUE}.
		       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INVISO_LFM
% Based on work done by Lennart Öhman, lennart.ohman@st.se


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DBG,
% Changes: Prefixing traces from non-local node
dhandler(end_of_trace, Out) ->
    Out;
dhandler(Trace, Out) when element(1, Trace) == trace, tuple_size(Trace) >= 3 ->
    dhandler1(Trace, tuple_size(Trace), Out);
dhandler(Trace, Out) when element(1, Trace) == trace_ts, tuple_size(Trace) >= 4 ->
    dhandler1(Trace, tuple_size(Trace)-1, Out);
dhandler(Trace, Out) when element(1, Trace) == drop, tuple_size(Trace) =:= 2 ->
    io:format(Out, "*** Dropped ~p messages.~n", [element(2,Trace)]),
    Out;
dhandler(Trace, Out) when element(1, Trace) == seq_trace, tuple_size(Trace) >= 3 ->
    SeqTraceInfo = case Trace of
		       {seq_trace, Lbl, STI, TS} ->
			   io:format(Out, "SeqTrace ~p [~p]: ",
				     [TS, Lbl]),
			   STI;
		       {seq_trace, Lbl, STI} ->
			  io:format(Out, "SeqTrace [~p]: ",
				     [Lbl]),
			   STI 
		   end,
    case SeqTraceInfo of
	{send, Ser, Fr, To, Mes} ->
	    io:format(Out, "(~s) ~s ! ~p [Serial: ~p]~n",
		      [nodify(Fr), nodify(To), Mes, Ser]);
	{'receive', Ser, Fr, To, Mes} ->
	    io:format(Out, "(~s) << ~s [Serial: ~p, From: ~s]~n",
		      [nodify(To), Mes, Ser, nodify(Fr)]);
	{print, Ser, Fr, _, Info} ->
	    io:format(Out, "-> ~p [Serial: ~p, From: ~s]~n",
		      [Info, Ser, nodify(Fr)]);
	Else ->
	    io:format(Out, "~p~n", [Else])
    end,
    Out;
dhandler(_Trace, Out) ->
    Out.

dhandler1(Trace, Size, Out) ->
%%%!    Self = self(),
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message -> io:format(Out, "(~s) << ~p~n", [nodify(From),Message])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    case element(5, Trace) of
%%%! This causes messages to disappear when used by ttb (observer). Tests
%%%! so far show that there is no difference in results with dbg even if I
%%%! comment it out, so  I hope this is only some old code which isn't
%%%! needed anymore... /siri
%%%!		Self -> ok;
		To -> io:format(Out, "(~s) ~s ! ~p~n", [nodify(From),nodify(To),Message])
	    end;
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io:format(Out, "(~s) call ~s (~p)~n", [nodify(From),ffunc(MFA),Message]);
		MFA ->
		    io:format(Out, "(~s) call ~s~n", [nodify(From),ffunc(MFA)])
	    end;
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(Out, "(~s) old_ret ~s -> ~p~n", [nodify(From),ffunc(MFA),Ret]);
		MFA ->
		    io:format(Out, "(~s) old_ret ~s~n", [nodify(From),ffunc(MFA)])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io:format(Out, "(~s) returned from ~s -> ~p~n", [nodify(From),ffunc(MFA),Ret]);
	return_to ->
	    MFA = element(4, Trace),
	    io:format(Out, "(~s) returning to ~s~n", [nodify(From),ffunc(MFA)]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(Out, "(~s) spawn ~p as ~s~n", [nodify(From),Pid,ffunc(MFA)]);
	Op ->
	    io:format(Out, "(~s) ~p ~s~n", [nodify(From),Op,ftup(Trace,4,Size)])
    end,
    Out.

%%% These f* functions returns non-flat strings

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M,F,Argl}) when is_list(Argl) ->
    io_lib:format("~p:~p(~s)", [M, F, fargs(Argl)]);
ffunc({M,F,Arity}) ->
    io_lib:format("~p:~p/~p", [M,F,Arity]);
ffunc(X) -> io_lib:format("~p", [X]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity) when is_integer(Arity) -> integer_to_list(Arity);
fargs([]) -> [];
fargs([A]) -> io_lib:format("~p", [A]);  %% last arg
fargs([A|Args]) -> [io_lib:format("~p,", [A]) | fargs(Args)];
fargs(A) -> io_lib:format("~p", [A]). % last or only arg

%% {A_1, A_2, ..., A_N} -> "A_Index A_Index+1 ... A_Size"
ftup(Trace, Index, Index) -> 
    io_lib:format("~p", [element(Index, Trace)]);
ftup(Trace, Index, Size) -> 
    [io_lib:format("~p ", [element(Index, Trace)]) 
     | ftup(Trace, Index+1, Size)].
