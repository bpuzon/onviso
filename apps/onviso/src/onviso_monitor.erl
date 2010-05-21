%%%-------------------------------------------------------------------
%%% @author  Bartlomiej Puzon <bartlomiej@erlang-solutions.com>
%% @doc <p>The internal module used by onviso to monitor the nodes and
%% provide the autoresume feature.</p>
%%
%% Nodes being added to an onviso trace are added to onviso_monitor
%% which erlang:monitor_nodes them. Then, if a {nodedown, Node} signal is
%% received, the monitor tries to reconnect and restart the trace in
%% a defined intervals 

%%% Copyright (c) 2010, Erlang Solutions Ltd.
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
-module(onviso_monitor).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Solutions Ltd.').

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL, 5000).

%% API
-export([start_link/0,
	 status/0,
	 reconnect/1,
	 configure/1,
	 validate_options/1,
	 add/1,
	 run/0,
	 clean/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(options, {resume_disabled = false,
		  resume_default = ?CHECK_INTERVAL,
		  resume_nodes = []}).

-record(state, {current_tref,     %%Used to indicate valid ticks
	        intervals,        %%Intervals to connect
	        nodes,            %%Nodes' statuses
		running,          %%
		options
	       }).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> ok | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{current_tref = make_ref(),
	        intervals = dict:new(),
	        nodes = dict:new(),
	        running = false,
	        options = #options{}}}.

%%--------------------------------------------------------------------
%% @spec run() -> ok
%% @doc Enables the autoresume feature after all the nodes have been added
%% @end
%%--------------------------------------------------------------------
-spec run() -> ok.		 
run() ->
    gen_server:cast(?MODULE, run).

%%--------------------------------------------------------------------
%% @spec add(Node) -> ok
%%      Node = atom()
%%
%% @doc Adds a node to the monitored nodes set.
%% @end
%%--------------------------------------------------------------------
-spec add(atom()) -> ok.		 
add(Node) ->
    gen_server:call(?MODULE, {monitor, Node}).

%%--------------------------------------------------------------------
%% @spec clean() -> ok
%% @doc Stops all monitoring and cleans the monitored node set
%% @end
%%--------------------------------------------------------------------
-spec clean() -> ok.
clean()->
    gen_server:cast(?MODULE, clean).

%%--------------------------------------------------------------------
%% @spec configure([{Option, Value}]) -> ok
%% @doc Sets configuration for the monitor. The possible Options are: <p>
%% <code>disabled</code> - if set to true, turns autoresume off <br/>
%% <code>default_interval</code> - default time to wait between connection
%%  attempts to the failing node (ms) <br/>
%% <code>nodes</code> - list specyfying {node, value} pairs where value
%%  is the connection attempt interval for the specified node </p>
%% @end
%%--------------------------------------------------------------------
-spec configure(list()) -> ok.
configure(List) ->
    gen_server:call(?MODULE, {configure, 
			      read_options(#options{}, List)}).

%%--------------------------------------------------------------------
%% @spec status() -> [{Node, active | inactive}]
%%      Node = atom()
%%       
%% @doc Return the lists of the monitored nodes along with the
%%      inviso_rt status on them
%% @end
%%--------------------------------------------------------------------
-spec status() -> [{atom(), active | inactive}].
status() ->
    dict:to_list((gen_server:call(?MODULE, status))#state.nodes).

%%--------------------------------------------------------------------
%% @spec reconnect(Node) -> Result
%%      Node = atom()
%%      Result = ok | {error, term()}
%%
%% @doc Force-reconnect a node
%% @end
%%--------------------------------------------------------------------
-spec reconnect(atom()) -> ok | {error, term()}.
reconnect(Node) ->
    gen_server:call(?MODULE, {reconnect, Node}).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(status, _From, State) ->
    {reply, State, State};

handle_call({configure, Options}, _From, State) ->
    {reply, ok, State#state{options = Options}};

handle_call({reconnect, Node}, _From, State) ->
    {Reply, State2} = pinger_reconnect(State, Node),
    {reply, Reply, State2};

handle_call({monitor, Node}, _From, State) ->
    Interval = get_interval(Node, State#state.options),
    monitor_node(Node, true),
    {reply, ok, State#state{
		  intervals = dict:store(Node, Interval, State#state.intervals),
		  nodes = dict:store(Node, active, State#state.nodes)}};
    
handle_call(_Request, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(run, State) ->
    {noreply, State#state{running = true}};

handle_cast(clean, State) ->
    lists:foreach(fun(Node) ->
			  monitor_node(Node, false)
		  end, dict:fetch_keys(State#state.nodes)),
    {noreply, State#state{current_tref = make_ref(),
			  intervals = dict:new(),
			  nodes = dict:new(),
			  running = false,
			  options = #options{}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({nodedown, Node}, State) ->
    case node_status(State, Node) of
	active ->
	    send_tick(Node, State),
	    Nodes = State#state.nodes,
	    {noreply, State#state{nodes = dict:store(Node, inactive, Nodes)}};
	_ ->
	    {noreply, State}
    end;

%%Monitor is running, so we try to reconnect
handle_info({tick, TRef, Node}, #state{running = true} = State)  ->
    case State#state.current_tref of
	TRef ->
	    %%We reconnect only if the node is inactive now
	    case node_status(State, Node) of
		inactive ->
		    {Reply, State2} = pinger_reconnect(State, Node),
		    case Reply of
			ok ->
			    {noreply, State2};
			_ ->
			    %%intervals don't change, we can use State
			    send_tick(Node, State),
			    {noreply, State2}
		    end;
		_ ->
		    {noreply, State}
	    end;
	_ ->
	    {noreply, State}
    end;

%%If the monitor is turned off, we don't reconnect, but we can't loose
%%the tick
handle_info({tick, TRef, Node}, State)  ->
    case State#state.current_tref of
	TRef ->
	    send_tick(Node, State),
	    {noreply, State};
	_ ->
	    {noreply, State}
    end;

handle_info(Info, State) ->
    io:format("info: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec pinger_reconnect(#state{}, atom()) -> {ok | {error, term()}, #state{}}.
pinger_reconnect(State, Node) ->
    case net_adm:ping(Node) of
	pong ->
	    Nodes = State#state.nodes,
	    case onviso:do_reconnect(Node) of
		ok ->
		    monitor_node(Node, true),
		    {ok, State#state{nodes = dict:store(Node, active, Nodes)}};
		Error ->
		    {Error, State}
	    end;
	_ ->
	    {{error, connection_failed}, State}
    end.

-spec node_status(#state{}, atom()) -> inactive | active | unknown.
node_status(State, Node) ->
    Nodes = State#state.nodes,
    case dict:find(Node, Nodes) of
	{ok, inactive} ->
	    inactive;
	{ok, active} ->
	    active;
	error ->
	    unknown
    end.

-spec send_tick(atom(), #state{}) -> ok.
send_tick(Node, State) ->
    {ok, Interval} = dict:find(Node, State#state.intervals),
    %%In cases of send_tick other than the node_down one
    %%this check is not mandatory
    case Interval of
	0 ->
	    ok;
	Interval ->
	    timer:send_after(Interval, {tick, State#state.current_tref, Node})
    end,
    ok.

-spec get_interval(atom(), #options{}) -> integer().
get_interval(Node, #options{resume_disabled = false} = Options) ->
    Default = Options#options.resume_default,
    case proplists:get_value(Node, Options#options.resume_nodes) of
	undefined ->
	    Default;
	Value ->
	    Value
    end;

get_interval(_, _) ->
    0.

-spec read_options(#options{}, list()) -> #options{}.
read_options(O, []) ->
    O;
read_options(O, [{nodes, Nodes} | T]) ->
    read_options(O#options{resume_nodes = Nodes}, T);
read_options(O, [{disabled, Val} | T]) ->
    read_options(O#options{resume_disabled = Val}, T);
read_options(O, [{default_interval, Val} | T]) ->
    read_options(O#options{resume_default = Val}, T).

-spec validate_options(list()) -> ok | {error, term()}.
validate_options(List) ->
    case catch read_options(#options{}, List) of
	#options{} ->
	    ok;
	{'EXIT', {function_clause, _}} ->
	    {error, unknown_autoresume_option}
    end.
			      
