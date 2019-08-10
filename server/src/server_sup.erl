%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @doc
%%%
%%% @end
%%% Created : 2019-04-27 11:44:32.215529
%%%
%%% Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
%%% Distributed under the Apache License, Version 2.0
%%% (https://www.apache.org/licenses/LICENSE-2.0)
%%%
%%%-------------------------------------------------------------------
-module(server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [child(reception), child(receptionist_sup)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
child(Mod) ->
    {Mod, {Mod, start_link, []},
     permanent, 2000, worker, [Mod]}.