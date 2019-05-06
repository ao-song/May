%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @copyright (C) 2019, Ao Song
%%% @doc
%%%
%%% @end
%%% Created : 2019-04-26 10:56:50.499593
%%%-------------------------------------------------------------------

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
-record(service, {id, name, address, port, properties}).