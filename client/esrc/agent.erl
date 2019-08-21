%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @copyright (C) 2019, Ao Song
%%% @doc
%%%
%%% @end
%%% Created : 2019-04-29 12:25:02.679847
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

-module(agent).

-behaviour(gen_server).

-include("client.hrl").

%% API
-export([start_link/0]).
-export([send/1, send/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {srv_ip, srv_port, srv_sock, is_tls_enabled, tls_config}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Send data to server
%%
%% @spec send(Data) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
send(Data) when is_binary(Data) ->
    gen_server:call(?MODULE, {send_packet, Data});
send(Data) ->
    Bin = term_to_binary(Data),
    gen_server:call(?MODULE, {send_packet, Bin}).

send(Data, ActiveMode) when is_binary(Data) ->
    gen_server:call(?MODULE, {send_packet, Data, ActiveMode});
send(Data, ActiveMode) ->
    Bin = term_to_binary(Data),
    gen_server:call(?MODULE, {send_packet, Bin, ActiveMode}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Conf =
    case file:consult(?CONFIG_FILE) of
        {ok, Config} -> 
            Config;
        {error, _Reason} ->
            ?LOG_ERROR("Client config file not found,"
                       "default settings will be used"),
            []
    end,
    Port = get_srv_port(Conf),
    Host = get_srv_Host(Conf),
    IsTlsEnabled = is_tls_enabled(Conf),
    TlsConfig = get_tls_config(Conf),

    ConnectRet =
    case IsTlsEnabled of
        true ->
            ssl:start(),
            ssl:connect(Host, Port, lists:merge(TlsConfig, ?SOCK_OPTIONS));
        false ->
            gen_tcp:connect(Host, Port, ?SOCK_OPTIONS)
    end,
    case ConnectRet of
        {ok, Socket} ->
            {ok, #state{srv_ip = Host, srv_port = Port, srv_sock = Socket,
                        is_tls_enabled = IsTlsEnabled, tls_config = TlsConfig}};
        _Other ->
            {stop, "Server not ready now!"}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send_packet, Data}, _From,
            #state{srv_sock = Socket,
                   is_tls_enabled = IsTlsEnabled} = State) ->
    Reply =
    case IsTlsEnabled of
        true ->
            ssl:send(Socket, Data);
        false ->
            gen_tcp:send(Socket, Data)
    end,
    {reply, Reply, State};
handle_call({send_packet, Data, {active, false}}, _From,
            #state{srv_sock = Socket,
                   is_tls_enabled = IsTlsEnabled} = State) ->
    inet:setopts(Socket, [{active, false}]),
    Reply =
    case IsTlsEnabled of
        true ->
            ok = ssl:send(Socket, Data),
            ssl:recv(Socket, 0);
        false ->
            ok = gen_tcp:send(Socket, Data),
            gen_tcp:recv(Socket, 0)
    end,
    %% socket should be active by default.
    inet:setopts(Socket, [{active, once}]),
    {reply, Reply, State};
handle_call({send_packet, Data, ActiveMode}, _From,
            #state{srv_sock = Socket,
                   is_tls_enabled = IsTlsEnabled} = State) ->
    inet:setopts(Socket, [ActiveMode]),
    Reply =
    case IsTlsEnabled of
        true ->
            ssl:send(Socket, Data);
        false ->
            gen_tcp:send(Socket, Data)
    end,
    %% socket should be active by default.
    inet:setopts(Socket, [{active, once}]),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, #state{srv_sock = Socket} = State) ->
    inet:setopts(Socket, [{active, once}]),
    Data = binary_to_term(Bin),
    receptionist:handle_response(Data),
    {noreply, State};
handle_info({tcp_closed, Socket}, #state{srv_sock = Socket} = State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{srv_sock = Socket,
                          is_tls_enabled = IsTlsEnabled}) ->
    case IsTlsEnabled of
        true ->
            ssl:shutdown(Socket, read_write),
            ssl:stop();
        false ->
            gen_tcp:shutdown(Socket, read_write)
    end,    
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_srv_port(Config) ->
    case lists:keyfind(server_port, 1, Config) of
        {server_port, PortConf} ->
            PortConf;
        false ->
            ?DEFAULT_SERVER_PORT
    end.

get_srv_Host(Config) ->
    SrvAddr =
    case lists:keyfind(server_ip, 1, Config) of
        {server_ip, AddrConf} ->
            AddrConf;
        false ->
            ?DEFAULT_SERVER_IP
    end,
    {ok, Addr} = inet:parse_address(SrvAddr),
    Addr.

is_tls_enabled(Config) ->
    case lists:keyfind(tls, 1, Config) of
        {tls, IsTlsEnabled} ->
            IsTlsEnabled;
        false ->
            % not enabled by default
            false
    end.

get_tls_config(Config) ->
    lists:filter(
        fun({cacertfile, _V}) -> true;
           ({certfile, _V}) -> true;
           ({keyfile, _V}) -> true;
           (_Other) -> false
        end, Config).