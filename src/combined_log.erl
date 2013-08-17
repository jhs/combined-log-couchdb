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

-export([on/1]).


on(init) ->
  couch_log:info("CouchDB plugin loaded: ~p", [?MODULE]).