%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @doc
%%%
%%% @end
%%% Created : 2019-04-26 10:56:50.499593
%%%
%%% Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
%%% Distributed under the Apache License, Version 2.0
%%% (https://www.apache.org/licenses/LICENSE-2.0)
%%%
%%%-------------------------------------------------------------------

%%====================================================================
%% Includes
%%====================================================================
%% logger
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Macros
%%====================================================================
-define(DEFAULT_TIMEOUT, 5000).

-define(SOCK_OPTIONS,
        [{active, once},
         binary,
         {packet, 0},
         {nodelay, true},
         {reuseaddr, true}]).

-define(DEFAULT_PORT, 8383).

%%====================================================================
%% Records
%%====================================================================
-record(service, {id, name, address, port, properties, owner}).