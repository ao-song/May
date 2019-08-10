%%%-------------------------------------------------------------------
%% @doc client top level supervisor.
%% @end
%%
%% Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
%% Distributed under the Apache License, Version 2.0
%% (https://www.apache.org/licenses/LICENSE-2.0)
%%
%%%-------------------------------------------------------------------

-module(client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags,
        [child(reception_rest), child(reception),
         child(agent), child(receptionist_sup)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
child(Mod) ->
    {Mod, {Mod, start_link, []},
     permanent, 2000, worker, [Mod]}.