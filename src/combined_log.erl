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

-define(WATCHER, lager_handler_watcher_sup).

% Time to start this plugin. Return 'ok' to indicate success. Any other return
% value or thrown error will deactivate this plugin.
on(init) -> ok
    , lager:start()

    % Find the lager handler supervisor, lager_handler_watcher_sup. Link to it, so if this plugin crashes,
    % it will reinstall the watchers; or if the supervisor crashes, this plugin will crash (thus reinstalling
    % the watchers).
    , case start_log_file()
        of ok -> ok
            , lager:info("~s is running", [?MODULE])
            , ok
        ; Failed -> ok
            , couch_log:error("Failed to find Lager handler watcher")
            , Failed
        end
    ;

on({log_request, #httpd{}=R}) -> ok
    'TODO'
    ;

% This catch-all handler ignores all other events.
on(_) -> ok.


start_log_file() -> ok
    , start_log_file(whereis(?WATCHER))
    .

start_log_file(undefined) -> ok
    , {error, not_registered, ?WATCHER}
    ;

start_log_file(Watcher_pid) when is_pid(Watcher_pid) -> ok
    , case couch_config:get("log", "file")
        of undefined -> ok
            , {error, no_couch_log}
        ; Couch_log -> ok
            , Log_dir = filename:dirname(Couch_log)
            , start_log_file(Watcher_pid, Log_dir)
        end
    .

start_log_file(Watcher, Log_dir) -> ok
    , start_log_file(Watcher, Log_dir, access, "access.log")
    , start_log_file(Watcher, Log_dir, error, "error.log")
    .

start_log_file(_Watcher_pid, Log_dir, Type, Filename) -> ok
    , Path = Log_dir ++ "/" ++ Filename
    , Module = {lager_file_backend, Path}
    , Config = {Path, none} % No log level. Tracing will be used to send data.
    , {ok, Child_pid} = supervisor:start_child(?WATCHER, [lager_event, Module, Config])
    , link(Child_pid)
    , couch_log:info("Log (~w): ~s", [Type, Filename])
    .


%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
