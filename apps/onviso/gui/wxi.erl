%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%% @author Atilla Erdodi <atilla.erdodi@erlang-consulting.com>
%% @doc Initial module for the wx based GUI. Still under development.

%%% Version: 0.1

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
-module(wxi).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

-include_lib("wx/include/wx.hrl").

-export([start/0,
         stop/0]).

%% General window definitions
-define(winTitle, "Onviso").
-define(winWidth, 1000).
-define(winHeight, 800).

%% Menu definitions
-define(menuFileLoadConf, 10).
-define(menuFileSaveConf, 11).
-define(menuWindowConfigure, 20).
-define(menuWindowRun, 21).

%% Window states
-record(state, {main_sizer,
                config_sizer,
                run_sizer}).

-spec start() -> pid().
start() ->
    spawn_link(fun() ->
                       onviso_server:start_link(),
                       wx:new(),
                       
                       {MainWindow, State} = setup_window([]),
                       loop(MainWindow, State),
                       
                       wx:destroy(),
                       onviso_server:stop(),
                       % purely temporary
                       erlang:halt()
               end).

-spec stop() -> ok.
stop() ->
    ok.


%% @doc Set up the main window and return the frame and state.
%% @end
setup_window(_Args) ->
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, ?winTitle, 
                        [{size, {?winWidth, ?winHeight}}]),
    
    % make menu                                            
    MenuBar = wxMenuBar:new(), 
    setup_menubar(MenuBar),
    wxFrame:setMenuBar(Frame, MenuBar),        

    % make main sizer
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    % make configure sizer
    ConfigSizer = setup_config(MainSizer, Frame),
    wxSizer:add(MainSizer, ConfigSizer, [{flag, ?wxEXPAND}]),

    % make run sizer
    RunSizer = setup_run(MainSizer, Frame),

    % make status bar
    wxFrame:createStatusBar(Frame),

    % attach main panel
    wxFrame:setSizer(Frame, MainSizer),

    % attach event handlers
    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, close_window),
    
    % return frame and state
    wxFrame:show(Frame),
    {Frame, #state{main_sizer=MainSizer,
                   config_sizer=ConfigSizer,
                   run_sizer=RunSizer}}.

setup_config(_MainSizer, Frame) ->
    ConfigPanel = wxPanel:new(Frame),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),

    Text = wxStaticText:new(ConfigPanel, ?wxID_ANY, "This is the config panel"),   

    wxSizer:add(Sizer, Text, [{flag, ?wxALL}, {border, 5}]),
    wxPanel:setSizer(ConfigPanel, Sizer),
    ConfigPanel.
    
 
setup_run(_MainSizer, Frame) ->    
    RunPanel = wxPanel:new(Frame),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),

    Text = wxStaticText:new(RunPanel, ?wxID_ANY, "Run Forest, run!"),

    wxSizer:add(Sizer, Text, [{flag, ?wxALL}, {border, 5}]),
    wxPanel:setSizer(RunPanel, Sizer),
    RunPanel.


setup_menubar(Menu) ->
    File = wxMenu:new(),
    wxMenu:append(File, ?menuFileLoadConf, "L&oad configuration"),
    wxMenu:append(File, ?menuFileSaveConf, "&Save configuration"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?wxID_EXIT, "&Quit"),
    wxMenuBar:append(Menu, File, "&File"),

    View = wxMenu:new(),
    wxMenu:append(View, ?menuWindowConfigure, "&Configuration"),
    wxMenu:append(View, ?menuWindowRun, "&Run"),
    wxMenuBar:append(Menu, View, "&Window"),
    
    Help = wxMenu:new(),
    wxMenu:append(Help, ?wxID_ABOUT, "About"),
    wxMenuBar:append(Menu, Help, "&Help").    
       


%% Event handler
loop(Frame, State) ->
    receive 
        #wx{id=?wxID_EXIT} ->
            wxWindow:close(Frame, []);
        #wx{} = GuiEvent ->
            NewState = handle_gui_event(GuiEvent, Frame, State),
            loop(Frame, NewState)
    end.

handle_gui_event(Event, Frame, State) ->
    case Event of
        #wx{id=?menuWindowConfigure} ->
            io:format("Changing to configure panel~n"),
            
            wxSizer:replace(State#state.main_sizer, 
                            State#state.run_sizer,
                            State#state.config_sizer),

            %wxSizer:hide(State#state.run_sizer, Frame, [{recursive, true}]),
            %wxSizer:show(State#state.config_sizer, Frame, [{show, true},
            %                                               {recursive, true}]),
            
            State;
        #wx{id=?menuWindowRun} ->
            io:format("Changing to run panel~n"),

            wxSizer:replace(State#state.main_sizer, 
                            State#state.config_sizer,
                            State#state.run_sizer),

            %wxSizer:hide(State#state.config_sizer, Frame, [{recursive, true}]),
            %wxSizer:show(State#state.run_sizer, Frame, [{show, true},
            %                                            {recursive, true}]),
            
            State
    end.
        
        
                                 
