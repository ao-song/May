%%%-------------------------------------------------------------------
%%% @author Ao Song
%%% @copyright (C) 2019, Ao Song
%%% @doc
%%%
%%% A simple http server handle the requests from application.
%%%
%%% @end
%%% Created : 2019-04-29 12:25:37.577465
%%%-------------------------------------------------------------------
-module(reception).

-behaviour(gen_server).

-include("client.hrl").

-include_lib("inets/include/httpd.hrl").

%% API
-export([start_link/0]).
-export([do/1]).
-export([handle_response/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {http_pid}).

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
%% Handle the response from server
%%
%% @spec handle_response(Response) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
handle_response(Packet) when is_binary(Packet) ->
    handle_response(binary_to_term(Packet));
handle_response({registered, _ID}) ->
    {proceed, {?CODE_200_OK, ?SERVICE_SUCCESFULLY_REGISTERED}};
handle_response({deregistered, _ID}) ->
    {proceed, {?CODE_200_OK, ?SERVICE_SUCCESFULLY_DEREGISTERED}};
handle_response({exit, caught, _Reason}) ->
    {proceed, {?CODE_SERVER_ERROR, ?REQUEST_FAILED}};
handle_response(_Response) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Handle http requests
%%
%% @spec do(Data) -> {proceed, OldData} | {proceed, NewData} | 
%%                   {break, NewData} | done
%% @end
%%--------------------------------------------------------------------
do(#mod{request_uri = ?DUMMY_REQUEST_ENDPOINT}) ->
    %% always ok with a dummy request.
    {proceed, {?CODE_200_OK, "ok"}};
do(#mod{request_uri = ?REGISTER_ENDPOINT, entity_body = Body}) ->
    RegMsg = construct_register_msg(Body),
    case agent:send(RegMsg, {active, false}) of
        {ok, Packet} ->
            handle_response(Packet);
        {error, _Reason} ->
            {proceed, {?CODE_CLIENT_ERROR, ?REQUEST_FAILED}}
    end;
do(#mod{request_uri = ?DEREGISTER_ENDPOINT_BASE ++ ServiceId}) ->
    case agent:send({deregister, ServiceId}, {active, false}) of
        {ok, Packet} ->
            handle_response(Packet);
        {error, _Reason} ->
            {proceed, {?CODE_CLIENT_ERROR, ?REQUEST_FAILED}}
    end;
do(#mod{request_uri = ?WATCH_ENDPOINT_BASE ++ _ServiceNameAndParas,
        absolute_uri = AbUri}) ->
    {ok, {_Scheme, _UserInfo, _Host, _Port, Path, Query}} =
    http_uri:parse(AbUri),
    Queries = httpd:parse_query(Query),
    BlockingTimeout = get_wait_time_in_query(Queries),
    ?WATCH_ENDPOINT_BASE ++ ServiceName = Path,
    case agent:send({watch, ServiceName, BlockingTimeout}) of
        {ok, Packet} ->
            handle_response(Packet);
        {error, _Reason} ->
            {proceed, {?CODE_CLIENT_ERROR, ?REQUEST_FAILED}}
    end;
do(#mod{method = _Method, request_uri = _RequestUri}) ->
    {proceed, {?CODE_SERVER_UNAVAILABLE, ?SERVICE_NOT_SUPPORTED}}.

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
    ok = inets:start(permanent),
    {ok, Pid} = inets:start(httpd, get_http_config()),
    {ok, #state{http_pid = Pid}}.

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
terminate(_Reason, #state{http_pid = HttpPid}) ->
    inets:stop(httpd, HttpPid),
    inets:stop(),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%l
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_port() -> ?DEFAULT_RECEPTION_PORT.

get_http_config() ->
    [{port, get_port()}, {server_name, "localhost"},
     {server_root, "/tmp/sd"},
     {document_root, "/tmp/sd/htdocs"},
     {modules, [?MODULE]}].

construct_register_msg(Body) ->
    ParsedBody = jsone:decode(list_to_binary(Body)),
    {register, 
     #service{id = maps:get(list_to_binary("ID"), ParsedBody),
              name = maps:get(list_to_binary("Name"), ParsedBody),
              address = maps:get(list_to_binary("Address"), ParsedBody),
              port = maps:get(list_to_binary("Port"), ParsedBody),
              properties = maps:get(list_to_binary("Tags"), ParsedBody)}}.

%% legacy from Consul, get wait time in uri query, unit in second
%% therefore hardcoded 1 sec for now as we know the query is also
%% hardcoded with 1 sec.
get_wait_time_in_query(_Queries) -> 1.

