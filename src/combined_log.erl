%%% Apache CouchDB Plugin: combined_log
%%%
%%% Copyright 2013 Jason Smith
%%%
%%%    Licensed under the Apache License, Version 2.0 (the "License");
%%%    you may not use this file except in compliance with the License.
%%%    You may obtain a copy of the License at
%%%
%%%         http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%    Unless required by applicable law or agreed to in writing, software
%%%    distributed under the License is distributed on an "AS IS" BASIS,
%%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%    See the License for the specific language governing permissions and
%%%    limitations under the License.

-module('combined_log').
-author('Jason Smith <jason.h.smith@gmail.com>').

-include("couch_plugin.hrl").
-export([on/1]).


% Time to start this plugin. Return 'ok' to indicate success. Any other return
% value or thrown error will deactivate this plugin.
on(init) -> ok
    , lager:start()
    , case couch_config:get("log", "file")
        of undefined -> ok
            , {error, no_couch_log}
        ; Couch_log -> ok
            , Log_dir = filename:dirname(Couch_log)
            , Access_path = Log_dir ++ "/access.log"
            , Error_path  = Log_dir ++ "/error.log"
            , 
        end

    % XXX The idea is to start lager, but somehow link with it. I am not sure if that is the
    % best idea, but maybe look for gen_event handlers and link to all those PIDs. So if this
    % crashes, lager restarts, and if lager crashes, this restarts. The goal is that anything
    % can crash and all the logging is restarted correctly.
    %
    % Also there is the question of keeping state in this module. How to balance simplicity
    % with the fact that there is no way to keep state at all? I am thinking use the process
    % dictionary.
%  case couch_config:get("log", "file")
%
%  couch_config:get("log", "file")
%  lager:info("Lager is running"),
%  supervisor:start_child(lager_handler_watcher_sup, [lager_event, Module, Config]) ||
%        {Module, Config} <- expand_handlers(Handlers)],
  ok;


% This catch-all handler ignores all other events.
on(_) -> ok.

% vim: sts=4 sw=4 et
