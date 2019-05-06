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
    ok = gen_tcp:send(Socket, term_to_binary(Reply)),
    {noreply, NewState};
%% Table events received when watch, need check how sdc interact with consul
handle_info({write, #service{name = Name}, _ActivityId} = TabEvent,
            #state{watching_services = WsList} = State) ->
    case lists:member(Name, WsList) of
        true ->
            notify_client(term_to_binary(TabEvent), State);
        false ->
            ok            
    end,
    {noreply, State};
handle_info({delete_object, #service{name = Name}, _ActivityId} = TabEvent,
            #state{watching_services = WsList} = State) ->
    case lists:member(Name, WsList) of
        true ->
            notify_client(term_to_binary(TabEvent), State);
        false ->
            ok            
    end,
    {noreply, State};
handle_info({delete, {service, Key}, _ActivityId} = TabEvent,
            #state{watching_services = WsList} = State) ->
    case lists:member(Key, WsList) of
        true ->
            notify_client(term_to_binary(TabEvent), State);
        false ->
            ok            
    end,
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
handle_request({deregister, #service{id = ID} = Service}, State) ->
    %% Need update with qlc query according to index flag when creating table
    ok = mnesia:dirty_delete(Service),
    {ID, State};
handle_request({watch, #service{id = ID, name = Name}},
               #state{watching_services = WsList} = State) ->
    %% Need update later with watch investigation
    mnesia:subscribe({table, service, simple}),
    {ID, State#state{watching_services = [Name | WsList]}}.

notify_client(Bin, #state{socket = Socket}) ->
    gen_tcp:send(Socket, Bin).




