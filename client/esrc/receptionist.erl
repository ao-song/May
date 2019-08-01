%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @copyright (C) 2019, Ao Song
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-09 13:41:31.471236
%%%-------------------------------------------------------------------
-module(receptionist).

-behaviour(gen_server).

-include("client.hrl").

%% API
-export([start_link/0]).
-export([set_socket/2]).
-export([handle_response/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

% Field event should be a list or a single event?
-record(state, {socket = null,
                event}).

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

%%--------------------------------------------------------------------
%% @doc
%% Handle the response from server
%%
%% @spec handle_response(Response) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
handle_response(Packet) when is_binary(Packet) ->
    handle_response(binary_to_term(Packet));
handle_response({registered, ID}) ->
    gen_server:cast(?MODULE, {registered, ID});
handle_response({deregistered, ID}) ->
    gen_server:cast(?MODULE, {deregistered, ID});
handle_response({got, ServiceList}) ->    
    gen_server:cast(?MODULE, {got, ServiceList});





%% todo, json handling in erlang httpd response? watch part should
%% be implemented in another approach.
handle_response({watched, ok}) ->
    {proceed, [{response, {?CODE_200_OK, ?SERVICE_SUCCESFULLY_WATCHED}}]};
handle_response({watched, ServiceList}) ->
    Body = service_list_to_json(ServiceList, []),
    {proceed, [{response, {response, [{code, ?CODE_200_OK},
                                      {content_type, ?JSON_TYPE}],
                           Body}}]};
%% event should not be handled like this!
handle_response({event, ServiceList}) ->
    Body = service_list_to_json(ServiceList, []),
    {proceed, [{response, {response, [{code, ?CODE_200_OK},
                                      {content_type, ?JSON_TYPE}],
                           Body}}]};
handle_response({exit, caught, _Reason}) ->
    {proceed, [{response, {?CODE_SERVER_ERROR, ?REQUEST_FAILED}}]};
handle_response(_Response) -> ok.

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
handle_cast({registered, ID}, #state{socket = Socket,
                                     event = {register, ID}} = State) ->
    gen_tcp:send(Socket, binary_to_list(jsone:encode({registered, c2a(ID)}))),
    {noreply, State};
handle_cast({deregistered, ID}, #state{socket = Socket,
                                       event = {deregister, ID}} = State) ->
    gen_tcp:send(Socket, binary_to_list(jsone:encode({deregistered, c2a(ID)}))),
    {noreply, State};

% how to handle this???
handle_cast({got, []}, #state{socket = Socket,
                              event = {deregister, ID}} = State) ->
    gen_tcp:send(Socket, binary_to_list(jsone:encode({deregistered, c2a(ID)}))),
    {noreply, State};
% handle_cast({got, ServiceList}, #state{socket = Socket,
%                                       event = {deregister, ID}} = State) ->
%    ok;
% Body = service_list_to_json(ServiceList, []),
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
    Request = construct_request_msg(Bin),
    NewState =
    case Request of
        {get, #service{name = Name}} ->
            State#state{event = {get, Name}};
        {watch, #service{name = Name, properties = Tags}} ->
            State#state{event = {watch, {Name, Tags}}};
        {Action, #service{id = ID}} ->
            State#state{event = {Action, ID}};
        _Other ->
            State
    end,
    agent:send(term_to_binary(Request)),
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
terminate(_Reason, _State) ->
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
construct_request_msg(Body) ->
    % todo, error handling
    ParsedBody = jsone:decode(Body),
    
    case c2l(maps:get(list_to_binary("action"), ParsedBody)) of
        "REG" ->
            {register, 
             #service{id = c2l(maps:get(list_to_binary("id"), ParsedBody)),
                      name = c2l(maps:get(list_to_binary("name"), ParsedBody)),
                      address = c2l(maps:get(list_to_binary("address"),
                                             ParsedBody)),
                      port = c2l(maps:get(list_to_binary("port"), ParsedBody)),
                      properties =
                          [c2l(X) || X <- maps:get(list_to_binary("tags"),
                                                   ParsedBody)]}};
        "DEREG" ->
            {deregister,
             #service{id = c2l(maps:get(list_to_binary("id"), ParsedBody))}};
        "WATCH" ->
            {watch,
             #service{id = c2l(maps:get(list_to_binary("id"), ParsedBody)),
                      name = c2l(maps:get(list_to_binary("name"), ParsedBody)),
                      address = c2l(maps:get(list_to_binary("address"),
                                             ParsedBody)),
                      port = c2l(maps:get(list_to_binary("port"), ParsedBody)),
                      properties =
                          [c2l(X) || X <- maps:get(list_to_binary("tags"),
                                                   ParsedBody)]}};
        "GET" ->
            {get,
             #service{name = c2l(maps:get(list_to_binary("name"), ParsedBody))}}
    end.

c2l(I) when is_binary(I) -> binary_to_list(I);
c2l(I) when is_integer(I) -> I;
c2l(I) when is_list(I) -> I.

service_list_to_json([], JsonList) ->
    jsone:encode(JsonList);
service_list_to_json([#service{id = ID,
                               name = Name,
                               address = Address,
                               port = Port,
                               properties = Props} | ServiceList],
                     JsonList) ->
    Service = [{id, c2a(ID)},
               {name, c2a(Name)},
               {address, c2a(Address)},
               {port, c2a(Port)},
               {tags, [c2a(X) || X <- Props]}],
    service_list_to_json(ServiceList, [Service | JsonList]).


c2a(I) when is_atom(I) -> I;
c2a(I) when is_integer(I) -> I;
c2a(I) when is_list(I) -> list_to_atom(I).

