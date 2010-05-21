%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%

-define(et_report(Mod1,Mod2,Event,Content),
        case whereis(et_elev) of
             undefined -> ok;
             Pid when pid(Pid) ->
                 et_collector:report_event(Pid,90,Mod1,Mod2,Event,
                       io_lib:format("~p\n",[Content]))
        end).


%%% start with
% {ok,Viewer} = et_viewer:start().
% catch unregister(et_elev).
% Collector = et_viewer:get_collector_pid(Viewer).
% register(et_elev,Collector).
