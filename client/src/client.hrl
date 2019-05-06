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

-define(CONFIG_FILE, "client.conf").

-define(DEFAULT_SERVER_PORT, 8383).
-define(DEFAULT_RECEPTION_PORT, 8585).

-define(CODE_200_OK, 200).
-define(CODE_CLIENT_ERROR, 400).
-define(CODE_SERVER_ERROR, 500).
-define(SERVICE_SUCCESFULLY_REGISTERED, "Service succesfully registered.").
-define(SERVICE_FAILED_REGISTERED, "Service failed registered.").

-define(WATCH_ENDPOINT_BASE, "/v1/health/service/").
-define(REGISTER_ENDPOINT, "/v1/agent/service/register").
-define(DEREGISTER_ENDPOINT_BASE, "/v1/agent/service/deregister/").
-define(DUMMY_REQUEST_ENDPOINT, "/v1/catalog/services").
-define(HTTP_MAX_BLOCKING_TIME, "&wait=1s").
-define(SERVICE_INDEX, "?index=").

%%====================================================================
%% Records
%%====================================================================
-record(service, {id, name, address, port, properties}).