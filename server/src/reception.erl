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
%%%
%%% Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
%%% Distributed under the Apache License, Version 2.0
%%% (https://www.apache.org/licenses/LICENSE-2.0)
%%%
%%%-------------------------------------------------------------------
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

-record(state, {listener}).

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
    Port = get_server_port(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

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
init([Port]) ->
    case gen_tcp:listen(Port, ?SOCK_OPTIONS) of
        {ok, ListenSocket} ->            
            {ok, accept(#state{listener = ListenSocket})};
        {error, Reason} ->
            {stop, Reason}
    end.

accept(#state{listener = ListenSocket} = State) ->
    proc_lib:spawn(fun() -> accept_loop(ListenSocket) end),
    State.

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    gen_server:cast(?SERVER, accepted),
    receptionist_sup:add_receptionist(Socket).

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
terminate(_Reason, #state{listener = ListenSocket}) ->
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
get_server_port() -> ?DEFAULT_PORT.

