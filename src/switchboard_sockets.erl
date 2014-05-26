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

%% @doc Worker/client websocket API. This provides an interface for
%% external clients to connect to and use some of the Switchboard
%% application's functionality.
%%
%% By default, the websocket's path is at `/clients', and the webserver
%% is listening on port `8081'.
%%
%% <h3>Switchboard Commands and Responses</h3>
%%
%% Commands and responses are in <a target="_parent"
%% href="http://www.json.org">JSON</a>. Each response object has a
%% type key which indicates the type of response: `cmd' if it is
%% solicited in direct response to a command, and `new' if it is an
%% unsolicited response notifying the arrival of new email.
%%
%% To simplify writing a client, it is recommended that solicited and
%% unsolicited responses are handled separately. See
%% thusfresh/spatch-python -- `/spatch/maildispatch.py' for an example
%% client -- in this implementation solicited and unsolicited
%% responses are read into separate queues, allowing commands to
%% monitor for their response without blocking handling of
%% unsolicited messages.
%%
%% Commands:
%%
%% <dl>
%%  <dt><b>Idle:</b> `{cmd: "idle"}'</dt>
%%
%%    <dd>
%%      Response: `{type: "cmd", idle: "ok"}'
%%
%%      If successful, the server will send all idle messages,
%%      i.e. new emails, to the client. These unsolicited messages
%%      take the form of {type: "new", item: ..., account: "Account",
%%      mailbox: "Mailbox"}. This command must be issued for the
%%      client to receive notifications of new emails.
%%    </dd>
%%
%%  <dt><b>Select:</b> `{cmd: "select", account: Account, mailbox: Mailbox}'</dt>
%%
%%    <dd>
%%      Response: `{type: "cmd", select: Mailbox}'
%%
%%      Selects the mailbox for the given account.
%%    </dd>
%%
%%  <dt><b>Fetch:</b>
%%    `{cmd: "fetch", account: Account, seqset: SeqSet, items: [Items]}'
%%  </dt>
%%
%%    <dd>
%%      Response: `{type: "cmd", fetch: ..., ...}'
%%
%%      todo -- add mailbox option to automatically select
%%      todo -- document seqset and items
%%
%%      Fetches emails for the given account. The content of the
%%      response depends on what items are specified.
%%   </dd>
%% </dl>
%%
%%
%% @todo Add authorization.


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


%% @private
websocket_info({new, {Account, Mailbox}, Item}, Req, State) ->
    reply(response(new, [{item, Item}, {account, Account}, {mailbox, Mailbox}]),
          Req, State);
websocket_info(Info, Req, State) ->
    lager:info("Unexpected info: ~p", [Info]),
    {ok, Req, State}.


%% @private
websocket_terminate(_Reason, _Req, _State) ->
    ok.


%%==============================================================================
%% Internal functions
%%==============================================================================

%% @private
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

%% @private
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
