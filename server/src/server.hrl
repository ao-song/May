%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @doc
%%%
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

-define(CONFIG_FILE, "server.conf").

-define(SOCK_OPTIONS,
        [{active, once},
         binary,
         {packet, 0},
         {nodelay, true},
         {reuseaddr, true}]).

-define(DEFAULT_LISTEN_PORT, 8383).

%%====================================================================
%% Records
%%====================================================================
-record(service, {id, name, address, port, properties, owner}).