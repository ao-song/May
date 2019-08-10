%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%
%% Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
%% Distributed under the Apache License, Version 2.0
%% (https://www.apache.org/licenses/LICENSE-2.0)
%%
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

%% Application callbacks
-export([install/0, install/1]).
-export([start/2, stop/1]).

-include("server.hrl").

%%====================================================================
%% API
%%====================================================================

start(retry, BadTabList) ->
    case mnesia:wait_for_tables(BadTabList, ?DEFAULT_TIMEOUT*3) of
        ok -> server_sup:start_link();
        _TabsNotOK ->
            {error, "Retried once, database still not prepared."}
    end;
start(_StartType, _StartArgs) ->
    install(),
    case mnesia:wait_for_tables([service], ?DEFAULT_TIMEOUT) of
        ok -> server_sup:start_link();
        {timeout, BadTabList} ->
            start(retry, BadTabList);
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Install should only be done once. By default only install on single
%% node.
%%
%% @spec install() -> {atomic, ok} | {aborted, Reason}
%% @end
%%--------------------------------------------------------------------
install() ->
    install([node()]).

%%--------------------------------------------------------------------
%% @doc
%% Install should only be done once. 
%%
%% @spec install(Nodes) -> {atomic, ok} | {aborted, Reason}
%% @end
%%--------------------------------------------------------------------
install(Nodes) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(service,
                        [{disc_copies, Nodes},
                         {attributes, record_info(fields, service)},
                         {type, set},
                         {index, [#service.name, #service.properties]}]).
%%====================================================================
%% Internal functions
%%====================================================================
