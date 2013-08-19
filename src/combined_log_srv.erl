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

-module('combined_log_srv').
-behaviour(gen_server).

-include("couch_plugin.hrl").
-import('combined_log', [on/1]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    PluginBase = ?PRIV_DIR ++ "/..",
    activate_dep(PluginBase),
    State = case on(init) of
        ok ->
            couch_log:info("CouchDB plugin loaded: ~w", [?SERVER]),
            Args;
        _  ->
            % Maybe some disabled state
            Args
    end,
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

activate_dep(Dir) ->
    Deps = Dir ++ "/deps",
    case file:list_dir(Deps) of
        {error, enoent} ->
            ok;
        {ok, Files} ->
            CheckDep = fun(File) -> check_dep(Deps, File) end,
            lists:foreach(CheckDep, Files)
    end,
    ok.

check_dep(Deps, File) ->
    Package = Deps ++ "/" ++ File,
    Ebin = Package ++ "/ebin",
    SubDeps = Package ++ "/deps",
    case filelib:is_dir(Ebin) of
        false -> ok;
        true ->
            % Add this dependency and possible sub-dependencies.
            code:add_pathz(Ebin),
            activate_dep(SubDeps)
    end.
