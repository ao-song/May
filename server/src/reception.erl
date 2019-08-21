%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @copyright (C) 2019, Ao Song
%%% @doc
%%%
%%% Reception desk of the server which will assign a receptionist for
%%% an incoming connection request.
%%%
%%% @end
%%% Created : 2019-04-26 10:08:27.011782
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
-module(reception).

-behaviour(gen_server).

-include("server.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {listener, listen_port, is_tls_enabled, tls_config}).

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
            ?LOG_ERROR("Server config file not found,"
                       "default settings will be used"),
            []
    end,
    Port = get_listen_port(Conf),
    IsTlsEnabled = is_tls_enabled(Conf),
    TlsConfig = get_tls_config(Conf),

    ListenRet =
    case IsTlsEnabled of
        true ->
            ssl:start(),
            ssl:listen(Port, lists:merge(TlsConfig, ?SOCK_OPTIONS));
        false ->
            gen_tcp:listen(Port, ?SOCK_OPTIONS)
    end,

    case ListenRet of
        {ok, ListenSocket} ->            
            {ok, accept(#state{listener = ListenSocket,
                               listen_port = Port,
                               is_tls_enabled = IsTlsEnabled,
                               tls_config = TlsConfig})};
        {error, Reason} ->
            {stop, Reason}
    end.

accept(#state{listener = ListenSocket,
              is_tls_enabled = IsTlsEnabled} = State) ->
    proc_lib:spawn(fun() ->
        accept_loop(ListenSocket, IsTlsEnabled) end),
    State.

accept_loop(ListenSocket, IsTlsEnabled) ->   
    Socket =
    case IsTlsEnabled of
        true ->
            {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket),
            {ok, TlsSocket} = ssl:handshake(TLSTransportSocket),
            TlsSocket;
        false ->
            {ok, Sock} = gen_tcp:accept(ListenSocket),
            Sock
    end,
    gen_server:cast(?SERVER, accepted),
    receptionist_sup:add_receptionist(Socket, IsTlsEnabled).

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
handle_cast(accepted, State) ->
    {noreply, accept(State)};
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
terminate(_Reason, #state{listener = ListenSocket,
                          is_tls_enabled = IsTlsEnabled}) ->
    case IsTlsEnabled of
        true ->
            ssl:close();
        _NotTrue ->
            do_nothing
    end,
    gen_tcp:close(ListenSocket).

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
get_listen_port(Config) ->
    case lists:keyfind(listen_port, 1, Config) of
        {listen_port, PortConf} ->
            PortConf;
        false ->
            ?DEFAULT_LISTEN_PORT
    end.

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