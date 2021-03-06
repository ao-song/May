%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @copyright (C) 2019, Ao Song
%%% @doc
%%% @end
%%% Created : 2019-04-26 10:56:50.499593
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

-define(CONFIG_FILE, "config/client.conf").

-define(DEFAULT_SERVER_IP, "127.0.0.1").
-define(DEFAULT_SERVER_PORT, 8383).
-define(DEFAULT_RECEPTION_PORT, 8585).

-define(CODE_200_OK, 200).
-define(CODE_CLIENT_ERROR, 400).
-define(CODE_SERVER_ERROR, 500).
-define(CODE_SERVER_UNAVAILABLE, 503).
-define(SERVICE_SUCCESFULLY_REGISTERED, "Service succesfully registered.").
-define(SERVICE_SUCCESFULLY_DEREGISTERED, "Service succesfully deregistered.").
-define(SERVICE_SUCCESFULLY_SUBSCRIBED, "Service subscribe succesfully created.").
-define(REQUEST_FAILED, "Request failed.").
-define(SERVICE_NOT_SUPPORTED, "Service not supported.").
-define(JSON_TYPE, "application/json").

-define(SUBSCRIBE_ENDPOINT_BASE, "/v1/health/service/").
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