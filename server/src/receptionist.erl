%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @copyright (C) 2019, Ao Song
%%% @doc
%%%
%%% Handle the requests from client.
%%%
%%% @end
%%% Created : 2019-04-26 19:37:46.202266
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
-module(receptionist).

-behaviour(gen_server).

-include("server.hrl").

%% API
-export([start_link/0]).
-export([set_socket/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket = null,
                watching_services = [],
                db_events = [],
                is_tls_enabled = false}).

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
    gen_server:start_link(?MODULE, [], []).

set_socket(Child, Socket, IsTlsEnabled) ->
    gen_server:cast(Child, {socket_ready, Socket, IsTlsEnabled}).

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
    ok = logger:set_module_level(?MODULE, debug),
    process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_cast({socket_ready, Socket, IsTlsEnabled}, State) ->
    ?LOG_INFO("Set socket."),
    set_opts(Socket, ?SOCK_OPTIONS, IsTlsEnabled),
    {noreply, State#state{socket = Socket, is_tls_enabled = IsTlsEnabled}};
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
handle_info({Prot, Socket, Bin},
            #state{socket = Socket,
                   is_tls_enabled = IsTlsEnabled} = State)
    when Prot == tcp orelse Prot == ssl ->
    set_opts(Socket, [{active, once}], IsTlsEnabled),
    Data = binary_to_term(Bin),
    ?LOG_INFO("Server: Receptionist received data, ~p: ~p~n", [Prot, Data]),
    {Reply, NewState} = handle_request(Data, State),
    case Reply of
        noreply -> 
            do_nothing;
        _Else ->
            send(Socket, term_to_binary(Reply), IsTlsEnabled)          
    end,
    {noreply, NewState};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};
handle_info({ssl_closed, Socket}, #state{socket = Socket} = State) ->
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
terminate(_Reason, #state{socket = Socket, is_tls_enabled = IsTlsEnabled}) ->
    {ok, _Node} = mnesia:unsubscribe({table, service, simple}),
    case IsTlsEnabled of
        true ->
            ssl:shutdown(Socket, read_write);
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
handle_request({register, #service{id = ID, owner = Owner} = Service},
               State) ->
    F = fun() ->
        mnesia:write(Service)
    end,    
    try mnesia:activity(transaction, F) of
        ok ->
            handle_table_event({write, Service}, State),
            {{registered, ID, Owner}, State}
    catch
        exit:{aborted, Reason} ->
            ?LOG_ERROR("Write mnesia failed, ~p~n", [Service]),
            {{exit, caught, Reason, ID, Owner}, State}
    end;
handle_request({deregister, #service{id = ServiceId, owner = Owner}},
               State) ->
    ServiceList = mnesia:dirty_read({service, ServiceId}),
    F = fun() ->
        mnesia:delete({service, ServiceId})
    end,
    try mnesia:activity(transaction, F) of
        ok ->
            handle_table_event({delete, ServiceList}, State),
            {{deregistered, ServiceId, Owner}, State}
    catch
        exit:{aborted, Reason} ->
            ?LOG_ERROR("Delete mnesia failed, ~p~n", [ServiceId]),
            {{exit, caught, Reason, ServiceId, Owner}, State}
    end;
handle_request({get, #service{name = ServiceName, owner = Owner}},
               State) ->
    try mnesia:dirty_match_object(#service{_ = '_',
                                           name = ServiceName}) of
        ServiceList ->
            {{got, ServiceList, Owner}, State}
    catch
        exit:{aborted, Reason} ->
            ?LOG_ERROR("Read mnesia failed, ~p~n", [ServiceName]),
            {{exit, caught, Reason, ServiceName, Owner}, State}
    end;
handle_request({watch,
               #service{name = ServiceName, properties = Tags, owner = Owner}},
               #state{watching_services = WsList} = State) ->
    NewState = State#state{watching_services =
        update_watching_list({ServiceName, Tags, Owner}, WsList)},

    case get(update_watch) of
        {true, WatchID} ->
            erase(update_watch),
            {{watch_updated, WatchID, Owner}, NewState};
        _Other ->
            WatchID = erlang:phash2({node(), erlang:timestamp()}),
            NewWsList =
                [{WatchID, ServiceName, Tags, Owner} | WsList],
            {{watched, WatchID, Owner},
                 NewState#state{watching_services = NewWsList}}
    end;
handle_request({cancel_watch, #service{id = WatchID, owner = Owner}},
               #state{watching_services = WsList} = State) ->
    NewState = State#state{watching_services = lists:keydelete(WatchID, 1, WsList)},
    {{watch_cancelled, WatchID, Owner}, NewState}.

handle_table_event({write, Service},
                   #state{socket = Socket,
                          watching_services = WsList,
                          is_tls_enabled = IsTlsEnabled}) ->
    notify_watching_client(write, WsList, Service, Socket, IsTlsEnabled);
handle_table_event({delete, DeletedRecs},
                   #state{socket = Socket,
                          watching_services = WsList,
                          is_tls_enabled = IsTlsEnabled}) ->
    [notify_watching_client(deleted, WsList, X, Socket, IsTlsEnabled) ||
     X <- DeletedRecs, is_record(X, service)].

notify_watching_client(Event, WatchingList,
                       #service{name = Name, properties = Tags} = Service,
                       Socket, IsTlsEnabled) ->
    
    WsMatched =
        [X || {_WatchID, ServiceName, WatchingTags, _Owner} = X <- WatchingList,
              ServiceName == Name, (WatchingTags -- Tags) == []],
    case WsMatched of
        [] ->
            do_nothing;
        _WatchingList ->
            lists:map(fun({_WatchID, _ServiceName, _WatchingTags, Owner}) ->
                send(Socket,
                     term_to_binary({watching_notice,
                                     Event,
                                     Service,
                                     Owner}),
                     IsTlsEnabled)
            end, WsMatched)
    end.

update_watching_list({ServiceName, Tags, Owner}, List) ->
    lists:foldl(
        fun({WatchID, ServiceNameX, _OldTags, OwnerX}, L)
              when ServiceNameX == ServiceName andalso OwnerX == Owner ->
            put(update_watch, {true, WatchID}),
            [{WatchID, ServiceName, Tags, Owner} | L];
           (Other, L) -> [Other | L]
        end, [], List).

set_opts(Socket, Opts, IsTlsEnabled) ->
    case IsTlsEnabled of
        true ->
            ssl:setopts(Socket, Opts);
        false ->
            inet:setopts(Socket, Opts)
    end.

send(Socket, Data, IsTlsEnabled) ->
    case IsTlsEnabled of
        true ->
            ok = ssl:send(Socket, Data);
        false ->
            ok = gen_tcp:send(Socket, Data)
    end.