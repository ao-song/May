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
-module(receptionist).

-behaviour(gen_server).

-include("server.hrl").

%% API
-export([start_link/0]).
-export([set_socket/2]).

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
                db_events = []}).

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

set_socket(Child, Socket) when is_pid(Child), is_port(Socket) ->
    gen_server:cast(Child, {socket_ready, Socket}).

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
    process_flag(trap_exit, true),
    {ok, _Node} = mnesia:subscribe(activity),
    {ok, _Node} = mnesia:subscribe({table, service, detailed}),
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
handle_cast({socket_ready, Socket}, State) ->
    inet:setopts(Socket, ?SOCK_OPTIONS),
    {noreply, State#state{socket = Socket}};
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
handle_info({tcp, Socket, Bin}, #state{socket = Socket} = State) ->
    inet:setopts(Socket, [{active, once}]),
    Data = binary_to_term(Bin),
    {Reply, NewState} = handle_request(Data, State),
    case Reply of
        noreply -> 
            do_nothing;
        _Else ->
            ok = gen_tcp:send(Socket, term_to_binary(Reply))
    end,
    {noreply, NewState};
handle_info({mnesia_table_event,
             {write, service, _Service, _OldRecs, _ActivityId} = Event},
            #state{db_events = Events} = State) ->
    NewState = State#state{db_events = [Event | Events]},
    {noreply, NewState};
handle_info({mnesia_table_event,
             {delete, service, _What, _OldRecs, _ActivityId} = Event},
            #state{db_events = Events} = State) ->
    NewState = State#state{db_events = [Event | Events]},
    {noreply, NewState};
handle_info({mnesia_activity_event, {complete, ActivityId}},
            #state{db_events = Events} = State) ->
    NewState =
    case lists:keyfind(ActivityId, 5, Events) of
        {_Action, service, _What, _OldRecs, ActivityId} = Event ->
            handle_table_event(Event, State),
            State#state{db_events = lists:delete(Event, Events)};
        false ->
            State
    end,
    {noreply, NewState};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
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
terminate(_Reason, #state{socket = Socket}) ->
    {ok, _Node} = mnesia:unsubscribe({table, service, simple}),
    gen_tcp:shutdown(Socket, read_write),
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
            {{registered, ID, Owner}, State}
    catch
        exit:{aborted, Reason} ->
            {{exit, caught, Reason, ID, Owner}, State}
    end;
handle_request({deregister, #service{id = ServiceId, owner = Owner}},
               State) ->
    F = fun() ->
        mnesia:delete({service, ServiceId})
    end,
    try mnesia:activity(transaction, F) of
        ok ->
            {{deregistered, ServiceId, Owner}, State}
    catch
        exit:{aborted, Reason} ->
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
            io:format("Fist watch: ~p~n", [NewWsList]),
            {{watched, WatchID, Owner},
                 NewState#state{watching_services = NewWsList}}
    end;
handle_request({cancel_watch, #service{id = WatchID, owner = Owner}},
               #state{watching_services = WsList} = State) ->
    NewState = State#state{watching_services = lists:keydelete(WatchID, 1, WsList)},
    {{watch_cancelled, WatchID, Owner}, NewState}.

handle_table_event({write, service, Service, _OldRecs, _ActivityId},
                   #state{socket = Socket, watching_services = WsList}) ->
    io:format("Table event coming, ~p~n", [Service]),
    notify_watching_client(write, WsList, Service, Socket);
handle_table_event({delete, service, _What, DeletedRecs, _ActivityId},
                   #state{socket = Socket, watching_services = WsList}) ->
    [notify_watching_client(deleted, WsList, X, Socket) ||
     X <- DeletedRecs, is_record(X, service)].

notify_watching_client(Event, WatchingList,
                       #service{name = Name, properties = Tags} = Service,
                       Socket) ->
    
    WsMatched =
        [X || {_WatchID, ServiceName, WatchingTags, _Owner} = X <- WatchingList,
              ServiceName == Name, (WatchingTags -- Tags) == []],
    io:format("Watching list is: ~p, Watching clients: ~p~n", [WatchingList, WsMatched]),
    case WsMatched of
        [] ->
            do_nothing;
        _WatchingList ->
            lists:map(fun({_WatchID, _ServiceName, _WatchingTags, Owner}) ->
                          gen_tcp:send(Socket,
                                       term_to_binary({watching_notice,
                                                       Event,
                                                       Service,
                                                       Owner}))
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