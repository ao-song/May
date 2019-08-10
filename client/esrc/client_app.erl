%%%-------------------------------------------------------------------
%% @doc client public API
%% @end
%%
%% Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
%% Distributed under the Apache License, Version 2.0
%% (https://www.apache.org/licenses/LICENSE-2.0)
%%
%%%-------------------------------------------------------------------

-module(client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    client_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
