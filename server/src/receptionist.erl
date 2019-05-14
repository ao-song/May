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
                watching_services = []}).

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
%% Table events received when watch, could be more powerful, current implementation
%% is just a compromise with consul.
handle_info({write, service, #service{name = Name}, _OldRecs, _ActivityId}, State) ->
    handle_table_event(Name, State),
    {noreply, State};
handle_info({delete, service, #service{name = Name}, _OldRecs, _ActivityId},
            State) ->    
    handle_table_event(Name, State),
    {noreply, State};
handle_info({delete, service, {service, _Key}, [#service{name = Name} | _OldRecs], _ActivityId},
            State) ->    
    handle_table_event(Name, State),
    {noreply, State};
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
handle_request({register, #service{id = ID} = Service}, State) ->
    try mnesia:dirty_write(Service) of
        ok ->
            {{registered, ID}, State}
    catch
        exit:{aborted, Reason} ->
            {{exit, caught, Reason}, State}
    end;
handle_request({deregister, ServiceId}, State) ->
    try mnesia:dirty_delete({service, ServiceId}) of
        ok ->
            {{deregistered, ServiceId}, State}
    catch
        exit:{aborted, Reason} ->
            {{exit, caught, Reason}, State}
    end;
handle_request({get, ServiceName}, State) ->
    try mnesia:dirty_match_object(#service{_ = '_',
                                           name = ServiceName}) of
        ServiceList ->
            {{got, ServiceList}, State}
    catch
        exit:{aborted, Reason} ->
            {{exit, caught, Reason}, State}
    end;
handle_request({watch, ServiceName, BlockingTimeout},
               #state{watching_services = WsList} = State) ->
    TimeStamp = erlang:system_time(second),
    case lists:keymember(ServiceName, 1, WsList) of
        true ->                 
            {{watched, ok}, State#state{watching_services =
             update_watching_list({ServiceName, TimeStamp, BlockingTimeout},
                                  WsList)}};                                
        false ->
            try mnesia:dirty_match_object(#service{_ = '_',
                                                   name = ServiceName}) of
                ServiceList ->
                    NewWsList =
                        [{ServiceName, TimeStamp, BlockingTimeout} | WsList],
                    {{watched, ServiceList},
                     State#state{watching_services = NewWsList}}
            catch
                exit:{aborted, Reason} ->
                    {{exit, caught, Reason}, State}
            end
    end.

handle_table_event(Name, #state{socket = Socket, watching_services = WsList}) ->
    Now = erlang:system_time(second),
    case lists:keyfind(Name, 1, WsList) of
        {Name, TimeStamp, BlockingTimeout} ->
            notify_client({Name, TimeStamp, BlockingTimeout}, Now, Socket);
        false ->
            do_nothing
    end.

notify_client({Name, TimeStamp, BlockingTimeout}, Now, Socket) ->
    case (Now - TimeStamp - BlockingTimeout) > 0 of
        true ->
            try mnesia:dirty_match_object(#service{_ = '_',
                                                   name = Name}) of
                ServiceList ->
                    gen_tcp:send(Socket, term_to_binary({event, ServiceList}))          
            catch
                exit:{aborted, Reason} ->
                    gen_tcp:send(Socket, term_to_binary({exit, caught, Reason}))
            end;
        false ->
            noreply
    end.

update_watching_list({ServiceName, _Timestamp, _Timeout} = Tuple, List) ->
    lists:keyreplace(ServiceName, 1, List, Tuple).