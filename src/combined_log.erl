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
-define(BLANK, <<"-">>).

-define(LOG(Level, Msg), lager:Level([{type,error}], Msg)).
-define(LOG(Level, Msg, Args), lager:Level([{type,error}], Msg, Args)).

% Time to start this plugin. Return 'ok' to indicate success. Any other return
% value or thrown error will deactivate this plugin.
on(init) -> ok
    , lager:start()
    , case start_log_file()
        of ok -> ok
            , ?LOG(info, "~s is running", [?MODULE])
            , ok
        ; Failed -> ok
            , couch_log:error("Failed to set up log files: ~p", [Failed])
            , Failed
        end
    ;

on({log_request, #httpd{mochi_req=MochiReq, peer=Peer, user_ctx=UserCtx}, Code}) -> ok
    % "%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-agent}i\""
    , {[Year,_,Month,_,Day], [Hour,_,Minute,_,Second,_,_Millis]} = lager_util:format_time()
    , Timestamp = [Day, $/, month(Month), $/, Year, $:, Hour, $:, Minute, $:, Second]

    , User = case UserCtx
        of #user_ctx{name=null}      -> ?BLANK
        ;  #user_ctx{name=User_name} -> User_name
        ;  _                         -> ?BLANK
        end
    , Method = MochiReq:get(method)
    , Path = MochiReq:get(raw_path)
    , {Ver_maj, Ver_min} = MochiReq:get(version)
    , Size = ?BLANK % This will require CouchDB changes to get.
    , Referer = case MochiReq:get_header_value(referer)
        of undefined -> ""
        ;  Referer0 -> Referer0
        end
    , Agent = case MochiReq:get_header_value('user-agent')
        of undefined -> ""
        ; Agent0 -> Agent0
        end

    , Format = "~s - ~s [~s +0000] \"~s ~s HTTP/~B.~B\" ~B ~s \"~s\" \"~s\""
    , Args = [Peer, User, Timestamp, Method, Path, Ver_maj, Ver_min, Code, Size, Referer, Agent]
    , lager:info([{type,access}], Format, Args)
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
    , Section = atom_to_list(?MODULE)
    , Prefix = couch_config:get(Section, "prefix", "")
    , Suffix = couch_config:get(Section, "suffix", ".log")
    , Access = couch_config:get(Section, "access_name", "access")
    , Error  = couch_config:get(Section, "error_name", "error")

    , start_log_file(Watcher, Log_dir, access, Prefix++Access++Suffix)
    , start_log_file(Watcher, Log_dir, error, Prefix++Error++Suffix)
    .

start_log_file(_Watcher_pid, Log_dir, Type, Filename) -> ok
    , Path = Log_dir ++ "/" ++ Filename
    , Module = {lager_file_backend, Path}
    , Format = case Type
        of access -> ok
            , [message, <<"\n">>]
        ; error -> ok
            , [ date, " ", time," [",severity,"] ", pid, " ", module, ":", line, "/", function, " ", message, "\n"]
        end
    , Config = [ {file,Path}, {level,none}, {size, 0}, {date, ""}
               , {formatter,lager_default_formatter}, {formatter_config,Format}
               ]

    % Link to the file backend, so if either this plugin or Lager crashes, both will restart.
    , {ok, Child_pid} = supervisor:start_child(?WATCHER, [lager_event, Module, Config])
    , link(Child_pid)

    , case Type
        of error -> lager:trace_file(Path, [{type,error}], debug)
        ; access -> lager:trace_file(Path, [{type,access}], info)
        end

    , couch_log:info("Log (~w): ~s", [Type, Filename])
    .

month("01") -> <<"Jan">>;
month("02") -> <<"Feb">>;
month("03") -> <<"Mar">>;
month("04") -> <<"Apr">>;
month("05") -> <<"May">>;
month("06") -> <<"Jun">>;
month("07") -> <<"Jul">>;
month("08") -> <<"Aug">>;
month("09") -> <<"Sep">>;
month("10") -> <<"Oct">>;
month("11") -> <<"Nov">>;
month("12") -> <<"Dec">>.

%%%      1         2         3         4         5         6         7         8         9         0         1         2         3
%%% 567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
