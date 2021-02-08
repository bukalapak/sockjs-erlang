-module(sockjs_cowboy_handler).

-behaviour(cowboy_handler).

%% Cowboy http callbacks
-export([init/2, terminate/3]).

%% Cowboy ws callbacks
-export([
    websocket_handle/3,
    websocket_info/3,
    websocket_init/3,
    websocket_terminate/3
]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

init(#{ref := http} = Req, Service) ->
    case sockjs_handler:is_valid_ws(Service, {cowboy, Req}) of
        {true, _Reason} ->
            {upgrade, protocol, cowboy_websocket};
        {false, _Reason} ->
            {ok, Req, Service}
    end.

terminate(_Reason, _Req, _Service) -> ok.

%% --------------------------------------------------------------------------

websocket_init(
    _TransportName,
    Req,
    Service = #service{
        logger = Logger,
        subproto_pref = SubProtocolPref
    }
) ->
    Req1 =
        case
            cowboy_req:header(
                'Sec-Websocket-Protocol',
                Req
            )
        of
            undefined ->
                Req;
            SubProtocols ->
                SelectedSubProtocol =
                    choose_subprotocol_bin(SubProtocols, SubProtocolPref),
                cowboy_req:set_resp_header(
                    #{
                        <<"Sec-Websocket-Protocol">> =>
                            SelectedSubProtocol
                    },
                    Req
                )
        end,
    Logger(Service, {cowboy, Req1}, websocket),
    Service1 = Service#service{
        disconnect_delay =
            5 * 60 * 1000
    },
    Info = sockjs_handler:extract_info(Req1),
    SessionPid = sockjs_session:maybe_create(
        undefined,
        Service1,
        Info
    ),
    RawWebsocket =
        case
            sockjs_handler:get_action(
                Service,
                Req1
            )
        of
            {match, WS} when WS =:= websocket orelse WS =:= rawwebsocket ->
                WS
        end,
    self() ! go,
    {ok, Req1, {RawWebsocket, SessionPid}}.

websocket_handle(
    {text, Data},
    Req,
    {RawWebsocket, SessionPid} = S
) ->
    case
        sockjs_ws_handler:received(
            RawWebsocket,
            SessionPid,
            Data
        )
    of
        ok -> {ok, Req, S};
        shutdown -> {shutdown, Req, S}
    end;
websocket_handle(_Unknown, Req, S) ->
    {shutdown, Req, S}.

websocket_info(
    go,
    Req,
    {RawWebsocket, SessionPid} = S
) ->
    case sockjs_ws_handler:reply(RawWebsocket, SessionPid) of
        wait ->
            {ok, Req, S};
        {ok, Data} ->
            self() ! go,
            {reply, {text, Data}, Req, S};
        {close, <<>>} ->
            {shutdown, Req, S};
        {close, Data} ->
            self() ! shutdown,
            {reply, {text, Data}, Req, S}
    end;
websocket_info(shutdown, Req, S) ->
    {shutdown, Req, S}.

websocket_terminate(
    _Reason,
    _Req,
    {RawWebsocket, SessionPid}
) ->
    sockjs_ws_handler:close(RawWebsocket, SessionPid),
    ok.

%% --------------------------------------------------------------------------

choose_subprotocol_bin(SubProtocols, Pref) ->
    choose_subprotocol(re:split(SubProtocols, ", *"), Pref).

choose_subprotocol(SubProtocols, undefined) ->
    erlang:hd(lists:reverse(lists:sort(SubProtocols)));
choose_subprotocol(SubProtocols, Pref) ->
    case
        lists:filter(
            fun(E) ->
                lists:member(E, SubProtocols)
            end,
            Pref
        )
    of
        [Hd | _] -> Hd;
        [] -> choose_subprotocol(SubProtocols, undefined)
    end.
