%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @copyright (C) 2019, Ao Song
%%% @doc
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

-define(CLIENT_IP, "127.0.0.1").
-define(CLIENT_PORT, 8080).

-define(SOCK_OPTIONS,
        [{active, once},
         binary,
         {packet, 0},
         {nodelay, true},
         {reuseaddr, true}]).

-define(CONFIG_FILE, "client.conf").

-define(DEFAULT_SERVER_IP, "127.0.0.1").
-define(DEFAULT_SERVER_PORT, 8383).
-define(DEFAULT_RECEPTION_PORT, 8585).

-define(CODE_200_OK, 200).
-define(CODE_CLIENT_ERROR, 400).
-define(CODE_SERVER_ERROR, 500).
-define(CODE_SERVER_UNAVAILABLE, 503).
-define(SERVICE_SUCCESFULLY_REGISTERED, "Service succesfully registered.").
-define(SERVICE_SUCCESFULLY_DEREGISTERED, "Service succesfully deregistered.").
-define(SERVICE_SUCCESFULLY_WATCHED, "Service watch succesfully created.").
-define(REQUEST_FAILED, "Request failed.").
-define(SERVICE_NOT_SUPPORTED, "Service not supported.").
-define(JSON_TYPE, "application/json").

-define(WATCH_ENDPOINT_BASE, "/v1/health/service/").
-define(REGISTER_ENDPOINT, "/v1/agent/service/register").
-define(DEREGISTER_ENDPOINT_BASE, "/v1/agent/service/deregister/").
-define(GET_ENDPOINT_BASE, "/v1/agent/service/get/").
-define(DUMMY_REQUEST_ENDPOINT, "/v1/catalog/services").
-define(HTTP_MAX_BLOCKING_TIME, "&wait=1s").
-define(SERVICE_INDEX, "?index=").
-define(CONSUL_INDEX_HEADER, "X-Consul-Index").

%%====================================================================
%% Records
%%====================================================================
-record(service, {id, name, address, port, properties, owner}).