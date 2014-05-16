%%------------------------------------------------------------------------------
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% 1. Redistributions of source code must retain the above copyright notice, this
%% list of conditions and the following disclaimer.
%%
%% 2. Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%%
%% 3. Neither the name of the copyright holder nor the names of its contributors
%% may be used to endorse or promote products derived from this software without
%% specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
%% THE POSSIBILITY OF SUCH DAMAGE.
%% @end
%%
%% @author Thomas Moulia <jtmoulia@pocketknife.io>
%% @copyright Copyright (c) 2014, ThusFresh, Inc.
%% @end
%%------------------------------------------------------------------------------

%% @doc Worker/client
%% <a href="http://tools.ietf.org/html/rfc6455">websocket</a> API. This
%% provides an interface for external clients to connect to and use
%% some of the Switchboard application's functionality.
%%
%% <h3>Application Protocol</h3>
%%
%% Commands and responses are in <a href="www.json.org">JSON</a>. Each
%% command will receive a single response.
%%
%% There are two types of responses:
%% <ul>
%%   <li>Resp
%%
%% By default, the websocket's path is at `/clients'. On
%% 
%%
%% @todo Add authorization.
%% @todo Switch from json?


-module(switchboard_sockets).
-behaviour(cowboy_websocket_handler).
-include("switchboard.hrl").

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-ifdef(TEST).
-export([handle/5,
         reply/3,
         response/1, response/2,
         parse_seqset/1,
         account_to_active/1]).
-endif.


%%==============================================================================
%% Cowboy Websocket Handler Callbacks
%%==============================================================================

%% @private
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


%% @private
websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.


%% @private
websocket_handle({text, RawPayload}, Req, State) ->
    Payload = jsx:decode(RawPayload),
    case {proplists:get_value(<<"cmd">>, Payload),
          proplists:get_value(<<"account">>, Payload)} of
        {undefined, _} ->
            reply(response([{error, no_cmd}]), Req, State);
        {Cmd, undefined} ->
            %% @todo R17 dicts would be an excellent fit for Payload...
            handle(Cmd, undefined, Payload, Req, State);
        {Cmd, Account} ->
            Active = case catch account_to_active(Account) of
                         %% @todo make this catch more specific
                         {'EXIT', _} ->
                             undefined;
                         Else ->
                             Else
                     end,
            handle(Cmd, Active, Payload, Req, State)
    end;
websocket_handle(Data, Req, State) ->
    lager:info("Unexpected data: ~p", [Data]),
    {ok, Req, State}.


websocket_info({new, {Account, Mailbox}, Item}, Req, State) ->
    reply(response(new, [{item, Item}, {account, Account}, {mailbox, Mailbox}]),
          Req, State);
websocket_info(Info, Req, State) ->
    lager:info("Unexpected info: ~p", [Info]),
    {ok, Req, State}.


websocket_terminate(_Reason, _Req, _State) ->
    ok.


%%==============================================================================
%% Internal functions
%%==============================================================================

%% @doc handle the given Cmd and Payload
%% @todo -- fill in the connect command to actually connect a new account
handle(<<"connect">>, undefined, _Payload, Req, State) ->
    reply(response([{error, connected}]), Req, State);
handle(<<"connect">>, _Active, _Payload, Req, State) ->
    reply(response([{error, connected}]), Req, State);

handle(<<"idle">>, _Active, _Payload, Req, State) ->
    case catch switchboard:subscribe(new) of
        true ->
            reply(response([{idle, ok}]), Req, State);
        {'EXIT', _} ->
            reply(response([{error, badsub}]), Req, State)
    end;

handle(<<"select">>, undefined, _Payload, Req, State) ->
    reply(response([{error, no_account}]), Req, State);
handle(<<"select">>, Active, Payload, Req, State) ->
    case proplists:get_value(<<"mailbox">>, Payload) of
        undefined ->
            reply(response([{error, no_mailbox_arg}]), Req, State);
        MailBox ->
            case imap:call(Active, {select, MailBox}) of
                {ok, _} ->
                    reply(response([{selected, MailBox}]), Req, State);
                {error, _} ->
                    reply(response([{error, select_err}]), Req, State)
            end
    end;

handle(<<"fetch">>, undefined, _Payload, Req, State) ->
    reply(response([{error, no_account}]), Req, State);
handle(<<"fetch">>, Active, Payload, Req, State) ->
    case {proplists:get_value(<<"seqset">>, Payload),
          proplists:get_value(<<"items">>, Payload)} of
        {SeqSet, Items} when Items =:= undefined; SeqSet =:= undefined ->
            lager:info("badfetchreq; payload: ~p", [Payload]),
            reply(response([{error, badfetchreq}]), Req, State);
        {SeqSet, Items} ->
            Fetch = {fetch, SeqSet, Items},
            Cmd = case proplists:get_value(<<"uid">>, Payload) of
                      true ->
                          {uid, Fetch};
                      _ ->
                          Fetch
                  end,
            case imap:call(Active, Cmd) of
                {ok, Fetched} ->
                    reply(response(lists:map(fun imap:clean/1, Fetched)), Req, State);
                {error, Reason} ->
                    lager:info("badfetchcall; Reason: ~p,"
                               " Items: ~p, SeqSet: ~p, ParseSeqSet: ~p, Pid: ~p",
                               [Reason, Items, SeqSet, parse_seqset(SeqSet), Active]),
                    reply(response([{error, badfetchcall}]), Req, State)
            end
    end;

handle(_Cmd, _Active, _Payload, Req, State) ->
    reply(response([{error, badcmd}]), Req, State).


%% @private
%% @doc Reply over the websocket with the given msg.
reply(Msg, Req, State) ->
    lager:debug("Socket Replying with: ~p", [Msg]),
    {reply, {text, jsx:encode(Msg)}, Req, State}.

%% @private
%% @doc Returns a status resonse.
-spec response(binary()) ->
    [proplist:property()].
response(Info) ->
    response(response, Info).

-spec response(ok | error, atom() | binary()) ->
    [proplist:property()].
response(Type, Info) ->
    [{type, Type} | Info].


%% @private
%% @doc Parse valid seqset.
-spec parse_seqset([proplist:property()]) ->
    imap:seqset().
parse_seqset([{<<"uid">>, SeqSet}]) ->
    {uid, parse_seqset(SeqSet)};
parse_seqset([_, _] = Range) ->
    [parse_seqset(Id) || Id <- Range];
parse_seqset(Id) when is_binary(Id) ->
    parse_seqset(binary_to_integer(Id));
parse_seqset(Id) when is_integer(Id) ->
    Id.


%% @private
%% @doc parse a valid account
-spec account_to_active(binary()) ->
    pid().
account_to_active(Username) when is_binary(Username) ->
    switchboard:where(Username, active).
