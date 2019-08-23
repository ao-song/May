%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @copyright (C) 2019, Ao Song
%%% @doc
%%%
%%% @end
%%% Created : 2019-04-26 19:38:39.558421
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Copyright 2019 Ao Song <ao.song@outlook.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(receptionist_sup).

-behaviour(supervisor).

-include("server.hrl").

%% API
-export([start_link/0]).
-export([add_receptionist/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Add a new receptionist to handle the socket
%%
%% @spec add_receptionist(Socket) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
add_receptionist(Socket, IsTlsEnabled) ->
    {ok, Child} = supervisor:start_child(?MODULE, []),
    ?LOG_INFO("Add a receptionist to handle the incoming connection with"
              " socket: ~p~n", [Socket]),
    case IsTlsEnabled of
        true ->
            ok = ssl:controlling_process(Socket, Child);
        false ->
            ok = gen_tcp:controlling_process(Socket, Child)
    end,
    receptionist:set_socket(Child, Socket, IsTlsEnabled).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ok = logger:set_module_level(?MODULE, debug),
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    AChild = {receptionist, {receptionist, start_link, []},
              Restart, Shutdown, Type, [receptionist]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



