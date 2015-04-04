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
%% @copyright Copyright (c) 2014, ThusFresh, Inc
%% @end
%%------------------------------------------------------------------------------

%% @doc JMAP command handler.
%%
%% This module is a websocket interface that handles JMAP commands.
%%
%% 1. Receives a list of JMAP commands encoded in JSON over the websocket.
%% 2. Decodes the commands, and converts each command list into a tuple.
%% 3. Handles each command using the `call' function.
%% 4. Encodes and replies with the list of responses.
%%
%% @todo change order of function args -- State should be last to be consistent

%% Steps:
%% 1. Receive JMAP cmd
%% 2. jmap_to_erl -- convert a jmap data str

%% with cowboy

-module(switchboard_jmap).
-behaviour(cowboy_websocket_handler).
-include("switchboard.hrl").

-export([state_by_account/1,
         encode/1,
         decode/1,
         mailbox_name_to_id/2,
         message_id/2,
         mailbox_id/2,
         connect/1,
         get_mailboxes/1, get_mailboxes/2,
         call/2,
         err/1, err/2]).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%% NB: Currently the join values must be different.
%% Potential Message ID: INBOX!1?msgid
-define(MESSAGE_JOINER, $?).
-define(MAILBOX_JOINER, $!).

-record(state, {connspec = none :: imap:connspec() | none,
                account = none :: imap:account(),
                owner = true :: boolean(),
                watched_mailboxes = sets:new() :: sets:set()}).



-type jmap_method() :: binary().
-type jmap_arg() :: {binary(), binary() | integer()}.
-type jmap() :: {Method :: jmap_method(),
                 Args :: [jmap_arg()],
                 ClientID :: undefined | binary()}.

-export_type([jmap/0]).


%%==============================================================================
%% Public interface.
%%==============================================================================

%% @doc Returns the minimal jmap state using the Account provided.
-spec state_by_account(binary()) ->
    #state{}.
state_by_account(Account) ->
    #state{account=Account}.


%% @doc Encode JMAP call proplists as JSON.
-spec encode([jmap()]) ->
    binary().
encode(JMAPs) ->
    jsx:encode([erl_to_jmap(JMAP) || JMAP <- JMAPs]).


%% @doc Decode a JMAP call binary into a proplist data structure.
-spec decode(binary()) ->
    [jmap()].
decode(JSON) ->
    [jmap_to_erl(Cmd) || Cmd <- jsx:decode(JSON)].


%% @doc Returns the mailbox ID given its name.
-spec mailbox_name_to_id(binary() | pid(), binary()) ->
    {ok, binary()} | {error, _}.
mailbox_name_to_id(Conn, Name) ->
    case mailbox_to_uidvalidity(Conn, Name) of
        {ok, UIDValidity} ->
            {ok, mailbox_id(Name, UIDValidity)};
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Encode a messageId from a mailboxId and UID.
-spec message_id(binary(), binary() | integer()) ->
    binary().
message_id(MailboxId, Uid) when is_integer(Uid) ->
    message_id(MailboxId, integer_to_binary(Uid));
message_id(MailboxId, Uid) when is_binary(Uid) ->
    %% XXX - currently using `?' and `!' as delimiters. Need to make sure that they're IMAP savvy.
    <<MailboxId/binary, ?MESSAGE_JOINER, Uid/binary>>.


%% @doc Encode a mailboxId from the mailbox name and UID validity number.
-spec mailbox_id(binary(), binary() | integer()) ->
    binary().
mailbox_id(MailboxName, UIDValidity) when is_integer(UIDValidity) ->
    mailbox_id(MailboxName, integer_to_binary(UIDValidity));
mailbox_id(MailboxName, UIDValidity) when is_binary(UIDValidity)  ->
    <<MailboxName/binary, ?MAILBOX_JOINER, UIDValidity/binary>>.


%% @doc Connect a new account using a proplistsconnspec augmented with
%% auth data, e.g.
%%
%% ```
%% ConnSpec = [{host, Host}, {port, Port}, {auth, Auth}].
%% '''
-spec connect([proplists:property()]) ->
    {ok, imap:account(), imap:connspec(), boolean()} | {error, _}.
connect(Args) ->
    case jmap_connect_parse_args(Args) of
        {ok, {ConnSpec, Auth}} ->
            Account = imap:auth_to_account(Auth),
            case lists:member(Account, switchboard:accounts()) of
                false ->
                    {ok, _} = switchboard:add(ConnSpec, Auth),
                    {ok, Account, ConnSpec, true};
                true ->
                    {ok, Account, ConnSpec, false}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% @private
%% @doc Implements JMAP's getMailboxes.
%%
%% <a href="http://jmap.io/#getmailboxes">`getMailboxes'</a>
get_mailboxes(Account) ->
    get_mailboxes(Account, []).

%% @private
get_mailboxes(Account, Args) when is_binary(Account), is_list(Args) ->
    switchboard:with_imap(Account, fun(IMAP) -> get_mailboxes(IMAP, Args) end);
get_mailboxes(IMAP, Args) when is_pid(IMAP) ->
    {ok, ListResps} = imap:clean_list(imap:call(IMAP, {list, <<"\"\"">>, <<"*">>})),
    get_mailboxes(IMAP, Args, ListResps).

%% @private
get_mailboxes(IMAP, Args, ListResps) ->
    SortedResps =
        lists:sort(fun(A, B) ->
                           proplists:get_value(name, A) < proplists:get_value(name, B)
                   end, ListResps),
    get_mailboxes(IMAP, Args, SortedResps, [], []).

%% @private
%% @todo check if the UIDValidities should be sorted by something like name
get_mailboxes(_IMAP, Args, [], UIDValidities, Acc) ->
    <<CurrentState:160/integer>> = crypto:hash(sha, UIDValidities),
    case CurrentState =:= proplists:get_value(<<"state">>, Args) of
        true ->
            {ok, CurrentState};
        false ->
            {ok, CurrentState, Acc}
    end;
get_mailboxes(IMAP, Args, [ListResp | Rest], UIDValidities, Acc) ->
    NameAttrs = proplists:get_value(name_attrs, ListResp),
    case lists:member(<<"\\Noselect">>, NameAttrs) of
        false ->
            Name = proplists:get_value(name, ListResp),
            {ok, UIDValidity} = mailbox_to_uidvalidity(IMAP, Name),
            get_mailboxes(IMAP, Args, Rest, [UIDValidity | UIDValidities],
                          [[{name, Name}, {id, mailbox_id(Name, UIDValidity)}] | Acc]);
        true ->
            get_mailboxes(IMAP, Args, Rest, UIDValidities, Acc)
    end.


%% @doc Create an error message using only the error `Type'.
-spec err(binary()) ->
    jmap().
err(Type) ->
    {<<"error">>, [{<<"type">>, Type}], undefined}.


%% @doc Create an error message for the JMAP command by including the
%% command `Method' and `Args' with the error `Type'.
-spec err(binary() | atom(), jmap()) ->
    jmap().
err(Type, {Method, Args, ClientID}) ->
    {<<"error">>,
     [{<<"type">>, Type},
      {<<"method">>, Method},
      {<<"arguments">>, Args}],
     ClientID};
err(Type, {Args, ClientID}) ->
    {<<"error">>,
     [{<<"type">>, Type} | Args],
     ClientID}.


%%==============================================================================
%% Cowboy Websocket Handler Callbacks
%%==============================================================================

%% @private
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


%% @private
websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #state{}}.


%% @private
websocket_handle({text, Data}, Req, State) when is_binary(Data) ->
    Calls = decode(Data),
    {Resps, State2} = call_all(Calls, State),
    {reply, {text, encode(Resps)}, Req, State2};
websocket_handle(Data, Req, State) ->
    lager:warning("Unexpected data: ~p", [Data]),
    {ok, Req, State}.


%% @private
websocket_info({new, {Account, Mailbox}, Item}, Req, #state{account=Account} = State) ->
    {ok, MailboxId} = mailbox_name_to_id(Account, Mailbox),
    Resp = [{mailboxId, MailboxId},
            {messageId, message_id(MailboxId, proplists:get_value(uid, Item))}],
    Reply = {<<"newMessage">>, Resp, undefined},
    lager:debug("Websocket received new msg: ~p", [Item]),
    {reply, {text, encode([Reply])}, Req, State};
websocket_info({new, _, _}, Req, State) ->
    {ok, Req, State};
websocket_info(Info, Req, State) ->
    lager:warning("Unexpected info: ~p", [Info]),
    {ok, Req, State}.


%% @private
websocket_terminate(_Reason, _Req, #state{account=Account, owner=true})
  when Account =/= none ->
    switchboard:stop(Account);
websocket_terminate(_Reason, _Req, _State) ->
    ok.


%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private
%% @doc Call all JMAP commands, returning the responses and updated State.
-spec call_all([jmap()], #state{}) ->
    {[jmap()], #state{}}.
call_all(Cmds, State) ->
    call_all(Cmds, State, []).

%% @private
-spec call_all([jmap()], #state{}, [jmap()]) ->
    {[jmap()], #state{}}.
call_all([], State, Resps) ->
    {lists:reverse(Resps), State};
call_all([Cmd | Rest], State, Resps) ->
    {Resp, State2} = call(Cmd, State),
    call_all(Rest, State2, [Resp | Resps]).


%% @private
%% @doc Convert the JMAP call to a data structure that's easier to work with.
-spec jmap_to_erl([_]) ->
    jmap().
jmap_to_erl([Method, Args]) ->
    {Method, Args, undefined};
jmap_to_erl([Method, Args, ClientID]) ->
    {Method, Args, ClientID}.


%% @private
%% @doc Convert from the internal JMAP representation to a jsx encodable form.
-spec erl_to_jmap(jmap()) ->
    [_].
erl_to_jmap({Method, Args, undefined}) ->
    [Method, Args];
erl_to_jmap({Method, Args, ClientID}) ->
    [Method, Args, ClientID].



%% @private
%% @doc Execute a JMAP command. In the case of errors which the client
%% should be notified of, return an error response.
%% @see err/2.
-spec call(#state{}, jmap()) ->
    {#state{}, jmap()}.
call({<<"connect">>, Args, ClientID} = Cmd, #state{account=none} = State) ->
    case connect(Args) of
        {ok, Account, ConnSpec, Owner} ->
            {{<<"connected">>, [{}], ClientID},
             State#state{connspec=ConnSpec, account=Account, owner=Owner}};
        {error, Reason} ->
            {err(Reason, Cmd), State}
    end;
call({<<"connect">>, _, _} = Cmd, #state{account=Account} = State)
  when Account =/= none ->
    {err(<<"alreadyConnected">>, Cmd), State};

call({<<"watchMailboxes">>, Args, ClientID} = Cmd, #state{account=Account} = State)
  when Account =/= none ->
    case watch_mailboxes(Account, Args) of
        {ok, MailboxesSet} ->
            {{<<"watchingMailboxes">>,
              [{<<"list">>, sets:to_list(MailboxesSet)}],
              ClientID},
             State#state{watched_mailboxes=MailboxesSet}};
        {error, Reason} ->
            {err(Reason, Cmd), State}
    end;

call({<<"getMailboxes">>, Args, ClientID} = Cmd, #state{account=Account} = State)
  when Account =/= none ->
    case get_mailboxes(Account, Args) of
        {ok, CurrentState} ->
            {{<<"mailboxes">>, [{<<"state">>, CurrentState}], ClientID}, State};
        {ok, CurrentState, MailboxIds} ->
            {{<<"mailboxes">>,
              [{<<"state">>, CurrentState}, {<<"list">>, MailboxIds}], ClientID},
             State};
        {error, Reason} ->
            {err(Reason, Cmd), State}
    end;

call({<<"getMessageList">>, Args, ClientID} = Cmd, #state{account=Account} = State)
  when Account =/= none ->
    case get_message_list(Account, Args) of
        {ok, MessageIds} ->
            {{<<"messageList">>, [{<<"messageIds">>, MessageIds}], ClientID}, State};
        {error, no_mailbox} ->
            {err(<<"mailboxDoesNotExist">>, Cmd), State};
        {error, no_mailboxid} ->
            {err(<<"missingMailboxId">>, Cmd), State}
    end;

call({<<"getMessages">>, Args, ClientID} = Cmd, #state{account=Account} = State)
  when Account =/= none ->
    case get_messages(Account, Args) of
        {ok, Messages} ->
            {{<<"messages">>, [{state, <<"TODO">>}, {list, Messages}], ClientID},
             State};
        {error, Reason} ->
            err(Reason, Cmd)
    end;

call(Cmd, State) ->
    {err(unknownMethod, Cmd), State}.


%% @private
%% @doc Start watching for new messages, returns the list of mailboxes
%% which are being watched.
%% @todo this function should use mailboxIds
-spec watch_mailboxes(imap:account(), [proplists:property()]) ->
    {ok, sets:set()} | {error, _}.
watch_mailboxes(Account, Args) ->
    case proplists:get_value(<<"list">>, Args) of
        undefined ->
            {error, noList};
        [] ->
            {error, emptyList};
        Mailboxes when is_list(Mailboxes) ->
            %% @todo - race condition here, depending on if name registration
            %% occurs immediately
            MailboxesSet = sets:from_list(Mailboxes),
            CurrentlyMonitored = sets:from_list(switchboard:mailbox_monitors(Account)),
            MonitorSet = sets:subtract(MailboxesSet, CurrentlyMonitored),
            %% @todo better way to do this without the catch?
            catch switchboard:subscribe(new),
            case sets:fold(
                   fun(Mailbox, Acc) ->
                           switchboard:add_mailbox_monitor(Account, Mailbox),
                           Acc
                   end, [], MonitorSet) of
                [] ->
                    {ok, MailboxesSet};
                Failed ->
                    {error, {failed, Failed}}
            end;
        _ ->
            {error, badList}
    end.


%% @private
%% @doc Returns the UID validity for the given mailbox name (not id).
%% @todo this overlaps heavily with get_mailbox/2 -- I need to figure
%% out a better API for this. HAXL?
-spec mailbox_to_uidvalidity(binary() | pid(), binary()) ->
    {ok, integer()} | {error, no_uidvalidity | _}.
mailbox_to_uidvalidity(Account, Name) when is_binary(Account) ->
    switchboard:with_imap(Account, fun(IMAP) -> mailbox_to_uidvalidity(IMAP, Name) end);
mailbox_to_uidvalidity(IMAP, Name) ->
    case imap:call(IMAP, {examine, Name}) of
        {ok, {_, Resps}} ->
            case proplists:get_value(uidvalidity, imap:clean(Resps)) of
                undefined ->
                    {error, no_uidvalidity};
                UIDValidity ->
                    {ok, UIDValidity}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% @private
%% @doc Implements JMAP's getMessageList.
%%
%% <a href="http://jmap.io/#getmessagelist">`getMailboxes'</a>
-spec get_message_list(imap:account(), [proplists:property()]) ->
    {ok, [binary()]} | {error, _}.
get_message_list(Account, Args) when is_binary(Account) ->
    switchboard:with_imap(Account, fun(IMAP) -> get_message_list(IMAP, Args) end);
get_message_list(IMAP, Args) when is_pid(IMAP) ->
    get_message_list(IMAP, Args, proplists:get_value(<<"mailboxId">>, Args)).

get_message_list(_IMAP, _Args, undefined) ->
    {error, nomailboxid};
get_message_list(IMAP, _args, MailboxId) ->
    case select_by_id(IMAP, MailboxId) of
        ok ->
            {ok, {_, Resps}} = imap:call(IMAP, {uid, {search, [<<"ALL">>]}}),
            case imap:clean(Resps) of
                [{search, Uids}] ->
                    {ok, [message_id(MailboxId, U) || U <- Uids]};
                Cleaned when length(Cleaned) > 1 ->
                    case proplists:get_value(search, Cleaned) of
                        undefined ->
                            {error, search_error};
                        Uids ->
                            {ok, [message_id(MailboxId, U) || U <- Uids]}
                    end
            end;
        {error, no_mailbox} ->
            {error, no_mailbox}
    end.


%% @private
%% @doc Returns a list of messages from the server.
get_messages(Account, Args) when is_binary(Account)->
    switchboard:with_imap(Account, fun(IMAP) -> get_messages(IMAP, Args) end);
get_messages(IMAP, Args) when is_pid(IMAP) ->
    get_messages(IMAP,
                 proplists:get_value(<<"ids">>, Args),
                 proplists:get_value(<<"properties">>, Args)).

%% @private
%% @todo must do validation of message_ids
get_messages(_IMAP, undefined, _) ->
    {error, badArgs};
get_messages(IMAP, ReqIds, Properties) when is_list(ReqIds) ->
    DecodedIds = [decode_message_id(Id) || Id <- ReqIds],
    %% @todo support multiple mailboxes
    Messages =
        lists:foldr(
          fun(MailboxId, Acc) ->
                  Ids = proplists:get_all_values(MailboxId, DecodedIds),
                  ok = select_by_id(IMAP, MailboxId),
                  {ok, Attrs} = fetch_attributes(Properties),
                  {ok, {_, Resps}} =
                      imap:call(IMAP, {uid, {fetch, Ids, Attrs}}),
                  lager:debug("Resps: ~p", [Resps]),
                  Acc ++ [case fetch_properties(MailboxId, M, Properties) of
                              {ok, Props} ->
                                  Props
                          end || M <- imap:clean(Resps)]
          end, [], proplists:get_keys(DecodedIds)),
    {ok, Messages}.


%% <b>`getMessages' Properties:</b>
%%
%% <dl>
%%   <dt>`conversationId'</dt>
%%     <dd>Not implemented.</dd>
%%   <dt>`mailboxId'</dt>
%%     <dd>Implemented. Dependencies: none</dd>
%%   <dt>`messageId'</dt>
%%     <dd>Implemented. Dependencies: enveloep</dd>
%%   <dt>`rawUrl'</dt>
%%     <dd>Not implemented. Dependencies: BODY.PEEK[]</dd>
%%   <dt>`isUnread'</dt>
%%     <dd>Not implemented. Dependencies: FLAGS -- ! (\Seen flag present)</dd>
%%   <dt>`isFlagged'</dt>
%%     <dd>Implemented, I think</dd>
%%   <dt>`isDraft'</dt>
%%     <dd>Not implemented. Dependencies: FLAGS?</dd>
%%   <dt>`hasAttachment'</dt>
%%     <dd>Not implemented. Dependencies: BODYSTRUCTURE</dd>
%%   <dt>`labels'</dt>
%%     <dd>Not implemented. http://tools.ietf.org/html/rfc5788</dd>
%%   <dt>`from'</dt>
%%     <dd>Implemented. Dependencies: ENVELOPE</dd>
%%   <dt>`to'</dt>
%%     <dd>Implemented. Dependencies: ENVELOPE</dd>
%%   <dt>`cc'</dt>
%%     <dd>Implemented. Dependencies: ENVELOPE</dd>
%%   <dt>`bcc'</dt>
%%     <dd>Implemented. Dependencies: ENVELOPE</dd>
%%   <dt>`replyTo'</dt>
%%     <dd>Implemented. Dependencies: ENVELOPE</dd>
%%   <dt>`subject'</dt>
%%     <dd>Implemented. Dependencies: ENVELOPE</dd>
%%   <dt>`date'</dt>
%%     <dd>Implemented. Dependencies: ENVELOPE</dd>
%%   <dt>`size'</dt>
%%     <dd>Implemented. Dependencies: ENVELOPE</dd>
%%   <dt>`preview'</dt>
%%     <dd>Not implemented. Dependencies: BODY[TEXT]</dd>
%%   <dt>`textBody'</dt>
%%     <dd>Implemented. Dependencies: BODY[TEXT]</dd>
%%   <dt>`htmlBody'</dt>
%%     <dd>Not implemented. Dependencies: BODY[] ? </dd>
%%   <dt>`body'</dt>
%%     <dd>Not implemented. Dependencies: ?</dd>
%%   <dt>`attachments'</dt>
%%     <dd>Not implemented. Dependencies: BODY[] ? AND it operates recursively</dd>
%%   <dt>`attachmentMessages'</dt>
%%     <dd>Not implemented. Dependencies: BODY[] ? AND it operates recursively</dd>
%% </dl>
%% @todo switch `envelope' fields to use rfc2822 header? More flexible, not necessary?


%% @private
%% @doc Returns the minimal `FETCH' attributes necessary to get all properties.
-spec fetch_attributes([proplists:property()]) ->
    {ok, [binary()]} | {error, _}.
fetch_attributes(Properties) ->
    fetch_attributes(Properties, sets:new()).

fetch_attributes([], Acc) ->
    {ok, sets:to_list(Acc)};
fetch_attributes([Prop | Rest], Acc) when Prop =:= <<"mailboxId">>
  orelse Prop =:= <<"messageId">> ->
    fetch_attributes(Rest, Acc);
fetch_attributes([<<"rawUrl">> | _], _) ->
    {error, rawUrlNotSupported};
fetch_attributes([Prop | Rest], Acc) when Prop =:= <<"unread">>
  orelse Prop =:= <<"isFlagged">> ->
    fetch_attributes(Rest, sets:add_element(<<"FLAGS">>, Acc));
fetch_attributes([<<"isDraft">> | _], _) ->
    {error, isDraftNotSupported};
fetch_attributes([<<"hasAttachment">> | Rest], Acc) ->
    fetch_attributes(Rest, sets:add_element(<<"BODYSTRUCTURE">>, Acc));
fetch_attributes([<<"labels">> | _], _) ->
    {error, labelsNotSupported};
fetch_attributes([Prop | Rest], Acc) when Prop =:= <<"from">> orelse
  Prop =:= <<"to">> orelse Prop =:= <<"cc">> orelse Prop =:= <<"bcc">> orelse
  Prop =:= <<"replyTo">> orelse Prop =:= <<"subject">> orelse Prop =:= <<"date">> ->
    fetch_attributes(Rest, sets:add_element(<<"ENVELOPE">>, Acc));
fetch_attributes([<<"size">> | Rest], Acc) ->
    fetch_attributes(Rest, sets:add_element(<<"RFC822.SIZE">>, Acc));
fetch_attributes([<<"textBody">> | Rest], Acc) ->
    fetch_attributes(Rest, sets:add_element(<<"BODY.PEEK[TEXT]">>, Acc));
fetch_attributes([<<"body">> | Rest], Acc) ->
    %% @todo add html part fetch
    %% @todo with this system, how can the text part be only fetched as necessary
    fetch_attributes(Rest, sets:add_element(<<"BODY.PEEK[TEXT]">>, Acc));
fetch_attributes([<<"raw">> | Rest], Acc) ->
    fetch_attributes(Rest, sets:add_element(<<"BODY.PEEK[]">>, Acc));
fetch_attributes(_, _) ->
    {error, unknownProp}.


%% @private
%% @doc Extract the provided properties from the fetched data.
fetch_properties(MailboxId, {fetch, Fetched}, Props) ->
    fetch_properties(message_id(MailboxId, proplists:get_value(uid, Fetched)),
                     Fetched, Props, []).

fetch_properties(_, _, [], Acc) ->
    {ok, Acc};
fetch_properties(MessageId, Fetched, [<<"mailboxId">> | Rest], Acc) ->
    {MailboxId, _} = decode_message_id(MessageId),
    fetch_properties(MailboxId, Fetched, Rest, [{mailboxId, MailboxId} | Acc]);
fetch_properties(MessageId, Fetched, [<<"messageId">> | Rest], Acc) ->
    fetch_properties(MessageId, Fetched, Rest, [{messageId, MessageId} | Acc]);
fetch_properties(MessageId, Fetched, [<<"isFlagged">> | Rest], Acc) ->
    throw(unimplemented),
    fetch_properties(MessageId, Fetched, Rest, [{messageId, MessageId} | Acc]);
fetch_properties(MessageId, Fetched, [<<"from">> | Rest], Acc) ->
    Envelope = proplists:get_value(envelope, Fetched),
    Prop = {from, addresses_to_jmap(proplists:get_value(from, Envelope))},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]);
fetch_properties(MessageId, Fetched, [<<"to">> | Rest], Acc) ->
    Envelope = proplists:get_value(envelope, Fetched),
    Prop = {to, addresses_to_jmap(proplists:get_value(to, Envelope))},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]);
fetch_properties(MessageId, Fetched, [<<"cc">> | Rest], Acc) ->
    Envelope = proplists:get_value(envelope, Fetched),
    Prop = {cc, addresses_to_jmap(proplists:get_value(cc, Envelope))},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]);
fetch_properties(MessageId, Fetched, [<<"bcc">> | Rest], Acc) ->
    Envelope = proplists:get_value(envelope, Fetched),
    Prop = {bcc, addresses_to_jmap(proplists:get_value(bcc, Envelope))},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]);
fetch_properties(MessageId, Fetched, [<<"replyTo">> | Rest], Acc) ->
    Envelope = proplists:get_value(envelope, Fetched),
    Prop = {replyTo, addresses_to_jmap(proplists:get_value(replyto, Envelope))},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]);
fetch_properties(MessageId, Fetched, [<<"subject">> | Rest], Acc) ->
    Envelope = proplists:get_value(envelope, Fetched),
    Prop = {subject, proplists:get_value(subject, Envelope)},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]);
fetch_properties(MessageId, Fetched, [<<"date">> | Rest], Acc) ->
    Envelope = proplists:get_value(envelope, Fetched),
    Prop = {date, proplists:get_value(date, Envelope)},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]);
fetch_properties(MessageId, Fetched, [<<"size">> | Rest], Acc) ->
    Prop = {size, proplists:get_value(rfc822size, Fetched)},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]);
fetch_properties(MessageId, Fetched, [<<"textBody">> | Rest], Acc) ->
    Prop = {textBody, proplists:get_value(textbody, Fetched)},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]);
fetch_properties(MessageId, Fetched, [<<"raw">> | Rest], Acc) ->
    Prop = {raw, proplists:get_value(body, Fetched)},
    fetch_properties(MessageId, Fetched, Rest, [Prop | Acc]).


-spec addresses_to_jmap([{address, proplists:property()}]) ->
    [[proplists:property()]] | null.
addresses_to_jmap(undefined) ->
    null;
addresses_to_jmap([]) ->
    null;
addresses_to_jmap(Addresses) ->
    [Address || {address, Address} <- Addresses].



%% @private
%% @doc Select a mailbox using the provided jmap mailbox id.
%% Returns an error if the UIDValidity doesn't match.
-spec select_by_id(binary() | pid(), binary()) ->
    ok | {error, _}.
select_by_id(Account, MailboxId) when is_binary(Account) ->
    switchboard:with_imap(Account, fun(IMAP) -> select_by_id(IMAP, MailboxId) end);
select_by_id(IMAP, MailboxId) when is_pid(IMAP) ->
    case catch decode_mailbox_id(MailboxId) of
        {'EXIT', Reason} ->
            lager:warning("Unable to decode mailbox id: ~p, (~p)", [MailboxId, Reason]),
            {error, no_mailbox};
        {Mailbox, UIDValidity} ->
            case imap:call(IMAP, {select, Mailbox}) of
                {ok, {_, RawResps}} ->
                    case proplists:get_value(uidvalidity, imap:clean(RawResps)) of
                        UIDValidity ->
                            ok;
                        _ ->
                            {error, no_mailbox}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.


%% @private
-spec decode_message_id(binary()) ->
    {binary(), non_neg_integer()}.
decode_message_id(MessageId) ->
    [MailboxId, Uid] = binary:split(MessageId, <<?MESSAGE_JOINER>>),
    {MailboxId, binary_to_integer(Uid)}.


%% private
%% @doc Decode a mailboxId into the mailbox and Uid.
-spec decode_mailbox_id(binary()) ->
    {binary(), non_neg_integer()}.
decode_mailbox_id(MailboxId) ->
    [Mailbox, UIDValidityBin] = binary:split(MailboxId, <<?MAILBOX_JOINER>>),
    {Mailbox, binary_to_integer(UIDValidityBin)}.


%% @private
%% @doc Parse the jmap connect args into internal data structure.
-spec jmap_connect_parse_args([proplists:property()]) ->
    {imap:connspec(), imap:auth()}.
jmap_connect_parse_args(Args) ->
    Type = ssl,
    case switchboard_util:get_values([<<"host">>, <<"port">>, <<"auth">>], Args) of
        {[Host, Port, Auth], []} ->
            {ok, {{Type, Host, Port}, auth_for_switchboard(Auth)}};
        {_, Undefineds} when Undefineds =/= [] ->
            {error, {missing, Undefineds}}
    end.


%% @doc Convert an auth proplist to the imap.erl data format.
auth_for_switchboard(Auth) ->
    auth_for_switchboard(proplists:get_value(<<"type">>, Auth), Auth).

auth_for_switchboard(<<"plain">>, Auth) ->
    {plain,
     proplists:get_value(<<"username">>, Auth),
     proplists:get_value(<<"password">>, Auth)};
auth_for_switchboard(<<"xoauth2">>, Auth) ->
    Username = proplists:get_value(<<"username">>, Auth),
    TokenProps = proplists:get_value(<<"token">>, Auth),
    Token = case proplists:get_value(<<"type">>, TokenProps) of
                <<"access">> ->
                    proplists:get_value(<<"token">>, TokenProps);
                <<"refresh">> ->
                    {proplists:get_value(<<"token">>, TokenProps),
                     proplists:get_value(<<"provider">>, TokenProps)}
                end,
    {xoauth2, Username, Token}.


%%==============================================================================
%% EUnit tests.
%%==============================================================================

-ifdef(LIVE_TEST).

suite_test_() ->
    [decode_encode_assertions(),
     jmap_connect_assertions(),
     mailbox_id_assertions(),
     {foreach,
      fun imap_setup/0,
      fun imap_teardown/1,
      [fun get_mailboxes_assertions/1,
       fun watch_mailboxes_assertions/1,
       fun select_by_id_assertions/1,
       fun get_message_list_assertions/1,
       fun get_messages_assertions/1]}].

%% @private
%% @doc Assertions for `encode/1' and `decode/1'.
decode_encode_assertions() ->
    EncodedCall = <<"[[\"method1\",{\"arg1.1\":\"val1.1\"},\"#1\"]"
                    ",[\"method2\",{\"arg2.1\":\"val2.1\"}]]">>,
    DecodedCall = [{<<"method1">>, [{<<"arg1.1">>, <<"val1.1">>}], <<"#1">>},
                   {<<"method2">>, [{<<"arg2.1">>, <<"val2.1">>}], undefined}],
    [?_assertEqual([], decode(<<"[]">>)),
     ?_assertEqual(DecodedCall, decode(EncodedCall)),
     ?_assertEqual(<<"[]">>, encode([])),
     ?_assertEqual(EncodedCall, encode(DecodedCall))].


%% @private
%% @doc Assertions for the jmap (well, my interpretation at least) connect command
jmap_connect_assertions() ->
    {{ssl, Host, Port} = ConnSpec, Auth} = {?DISPATCH_CONN_SPEC, ?DISPATCH_AUTH},
    Account = imap:auth_to_account(Auth),
    case lists:member(Account, switchboard:accounts()) of
        true ->
            OldIMAP = switchboard:where(Account, active),
            switchboard:stop(Account),
            switchboard_util:await_death(OldIMAP);
        false ->
            ok
    end,
    Connect = call({<<"connect">>,
                    [{<<"host">>, Host},
                     {<<"port">>, Port},
                     {<<"auth">>, imap:auth_to_props(Auth)}],
                    <<"1">>}, #state{}),
    {IMAP, _} = gproc:await(switchboard:key_for(Account, active), 5000),
    [?_assertMatch({{<<"connected">>, _, <<"1">>},
                    #state{connspec=ConnSpec, account=Account}}, Connect),
     ?_assertEqual(ok, switchboard:stop(Account)),
     ?_assertEqual(ok, switchboard_util:await_death(IMAP))].


%% @private
%% @doc Assertions for `mailbox_id/2'.
mailbox_id_assertions() ->
    [?_assertEqual(<<"Mailbox!1">>, mailbox_id(<<"Mailbox">>, 1))].


%% @private
%% @doc Setup the test imap connection.
imap_setup() ->
    {ConnSpec, Auth} = {?DISPATCH_CONN_SPEC, ?DISPATCH_AUTH},
    {ok, _} = switchboard:add(?DISPATCH_CONN_SPEC, Auth),
    #state{connspec=ConnSpec, account=imap:auth_to_account(Auth)}.


%% @private
%% @doc Exit the test imap connection.
imap_teardown(#state{account=Account}) ->
    IMAP = switchboard:where(Account, active),
    switchboard:stop(Account),
    switchboard_util:await_death(IMAP).


%% @private
%% @doc Assertions for the JMAP getMailboxes command. This test
%% calls getMailboxes twice, using the state returned by the first
%% call in the second.
get_mailboxes_assertions(#state{account=Account}) ->
    %% This test depends on the state not actually changing.
    {ok, MailboxesState, Mailboxes} = get_mailboxes(Account, []),
    {ok, MailboxesState} = get_mailboxes(Account, [{<<"state">>, MailboxesState}]),
    [{"Because `state' wasn't in the 1st call's args, mailboxes should be present.",
      ?_assert(is_list(Mailboxes))}].


%% @private
%% @doc Assertions for the select_by_id command.
%% @todo Write tests for failure conditions.
select_by_id_assertions(#state{account=Account}) ->
    {ok, InboxID} = mailbox_name_to_id(Account, <<"INBOX">>),
    [?_assertNotEqual(InboxID, undefined),
     ?_assertEqual(ok, select_by_id(Account, InboxID))].


%% @private
%% @doc Assertions for the `watch_mailboxes' command.
%% @todo Write tests for `failed' mailboxes.
watch_mailboxes_assertions(#state{account=Account}) ->
    %% XXX - these config vals should be passed through the opts...
    WatchMailbox = <<"INBOX">>,
    {ok, MailboxesSet} = watch_mailboxes(Account, [{<<"list">>, [WatchMailbox]}]),
    [?_assert(sets:is_set(MailboxesSet)),
     ?_assertMatch(P when is_pid(P), switchboard:await(Account,
                                                       {idler, WatchMailbox})),
     ?_assertEqual([WatchMailbox], switchboard:mailbox_monitors(Account))].

%% @private
get_message_list_assertions(#state{account=Account}) ->
    {ok, InboxID} = mailbox_name_to_id(Account, <<"INBOX">>),
    {ok, MessageIds} = get_message_list(Account, [{<<"mailboxId">>, InboxID}]),
    [?_assert(is_list(MessageIds))].


%% @private
get_messages_assertions(#state{account=Account}) ->
    {ok, InboxID} = mailbox_name_to_id(Account, <<"INBOX">>),
    {ok, MessageIds} = get_message_list(Account, [{<<"mailboxId">>, InboxID}]),
    {ok, Messages} =
        get_messages(Account, [{<<"ids">>, lists:sublist(MessageIds, 3)},
                               {<<"properties">>, [<<"mailboxId">>,
                                                   <<"messageId">>,
                                                   <<"subject">>,
                                                   <<"date">>,
                                                   <<"size">>,
                                                   <<"textBody">>]}]),
    [{"get_message_list should return message ids in its Resp.",
      ?_assertNotEqual(undefined, MessageIds)},
     {"get_message_list shouldn't return an empty list.",
      ?_assertNotEqual([], MessageIds)},
     {"There should be a list messages in MsgArgs",
      ?_assert(is_list(Messages))},
     {"Test the return values.",
      lists:foldr(
        fun(Message, Acc) ->
                [?_assertNotEqual(undefined, proplists:get_value(Key, Message))
                 || Key <- [messageId, mailboxId, subject, date, size]] ++ Acc
        end, [], Messages)}].
     %% ?_assertNotEqual(undefined, MsgArgs)].

-endif.
