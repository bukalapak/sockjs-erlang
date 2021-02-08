-module(sockjs_cowboy_handler).

-behaviour(cowboy_handler).

%% Cowboy http callbacks
-export([init/2, terminate/3]).

%% Cowboy ws callbacks
-export([
    websocket_handle/2,
    websocket_info/2,
    websocket_init/2,
    websocket_terminate/2
]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

init(#{ref := http} = Req, Service) ->
    case sockjs_handler:is_valid_ws(Service, {cowboy, Req}) of
        {true, _Reason} ->
            {cowboy_websocket, Req, Service};
        {false, _Reason} ->
            {ok, Req, Service}
    end.

terminate(_Reason, _Req, _Service) -> ok.

%% --------------------------------------------------------------------------

websocket_init(
    _TransportName,
    _Service
) ->
    self() ! go,
    {ok, {}}.

websocket_handle(
    {text, Data},
    {RawWebsocket, SessionPid} = S
) ->
    case
        sockjs_ws_handler:received(
            RawWebsocket,
            SessionPid,
            Data
        )
    of
        ok -> {ok, S};
        shutdown -> {stop, S}
    end;
websocket_handle(_Unknown, S) ->
    {stop, S}.

websocket_info(
    go,
    {RawWebsocket, SessionPid} = S
) ->
    case sockjs_ws_handler:reply(RawWebsocket, SessionPid) of
        wait ->
            {ok, S};
        {ok, Data} ->
            self() ! go,
            {reply, {text, Data}, S};
        {close, <<>>} ->
            {stop, S};
        {close, Data} ->
            self() ! shutdown,
            {reply, {text, Data}, S}
    end;
websocket_info(shutdown, S) ->
    {stop, S}.

websocket_terminate(
    _Reason,
    {RawWebsocket, SessionPid}
) ->
    sockjs_ws_handler:close(RawWebsocket, SessionPid),
    ok.

%% --------------------------------------------------------------------------
