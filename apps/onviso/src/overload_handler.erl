%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%% @doc This module contains the necessary functions to demonstrate the
%% overload protection facilities of Onviso.
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
-module(overload_handler).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

-export([start/0,    
         stop/0,
         check/1]).

%%--------------------------------------------------------------------
%% @doc Initiates the overload handler process. In this case it actually
%% only returns <code>ok</code> since no set-up is required. 
%% <p>What is considered an overload situation depends on the type of 
%% system you are running. That is why it has to be defined outside the 
%% scope of Onviso.</p>
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.                 
start() ->
    ok.

%%--------------------------------------------------------------------
%% @doc Cleans up the overload handler process.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.                 
stop() ->
    ok.

%%--------------------------------------------------------------------
%% @doc This function is called according to the interval defined in 
%% the overload specification. In this example the check compares the 
%% amount of free memory of the system to a threshold. If the amount 
%% of free memory is less than the threshold, <code>{suspend, Reason}</code>
%% is returned and all trace patterns are deactivated by Onviso. In the 
%% other case tracing will continue as normal. 
%% <p>It is possible to pass data to the <code>check</code> function. It 
%% accepts one argument and it is the return value from the 
%% {@link start/0. 'start/0'} function which is passed. Note that it is 
%% possible to pass arguments to the start function from the overload
%% specification, i.e the <code>start</code> arity doesn't have to be 0.</p>
%% <p><strong>Note:</strong> this example only works on *nix like 
%% systems with the <code>free</code> script installed.</p>
%% <pre>
%% free | grep -i mem | awk '{print $4}'
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec check(any()) -> ok.
check(_) ->
    MemFree = os:cmd("free | grep -i mem | awk '{print $4}'"),
    if 
        MemFree < 10000 ->
            io:format("Memcheck: High memory usage, suspending.~n"),
            {suspend, high_memory_usage};
        true ->
            io:format("Memcheck: Continuing as normal~n"),
            ok
    end.
