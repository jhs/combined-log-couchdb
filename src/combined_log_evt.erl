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

%% CouchDB Plugin authors:
%%
%% Do not to edit this file. Your code is in combined_log.erl.

-module('combined_log_evt').
-behaviour(gen_event).

-include("couch_plugin.hrl").

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------

init([Server]) ->
    {ok, Server}.

handle_event(Event, Server) ->
    gen_server:cast(Server, {event, Event}),
    {ok, Server}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
