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

%% @doc JMAP Command Handler

-module(switchboard_jmap).
-behaviour(cowboy_websocket_handler).
-include("switchboard.hrl").

-export([message_id/2]).
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).


-record(state, {connspec = none :: imap:connspec() | none,
                auth = none :: imap:auth() | none,
                owner = true :: boolean(),
                idle_mailboxes = [] :: [binary()]}).

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
    {State2, Resps} = execute(State, Calls),
    {reply, {text, encode(Resps)}, Req, State2};
websocket_handle(Data, Req, State) ->
    lager:info("Unexpected data: ~p", [Data]),
    {ok, Req, State}.


%% @private
websocket_info({new, {Account, Mailbox}, Item}, Req, State) ->
    Reply = {<<"newMessage">>,
             [{item, Item}, {account, Account}, {mailbox, Mailbox}],
             undefined},
    lager:info("Received new msg: ~p", [Item]),
    {reply, {text, encode([Reply])}, Req, State};
websocket_info(Info, Req, State) ->
    lager:info("Unexpected info: ~p", [Info]),
    {ok, Req, State}.


%% @private
websocket_terminate(_Reason, _Req, #state{auth=Auth, owner=true}) when Auth =/= none ->
    Username = imap:auth_to_username(Auth),
    switchboard:stop(Username);
websocket_terminate(_Reason, _Req, _State) ->
    ok.


%%==============================================================================
%% Internal Functions
%%==============================================================================

%% Swap out the JMAP active connections with a pool -- when running a JMAP
%% connection clients can checkout/lock an IMAP process.

%% Steps:
%% 1. Receive JMAP cmd
%% 2. jmap_command_to_erl -- convert a jmap data str


-type jmap_method() :: binary().
-type jmap_arg() :: {binary(), binary() | integer()}.
-type jmap() :: {Method   :: jmap_method(),
                 Args     :: [jmap_arg()],
                 ClientID :: undefined | binary()}.

%% @private
%% @equiv execute(State, Calls, [])
-spec execute(binary(), [jmap()]) ->
    {#state{}, [jmap()]}.
execute(State, Calls) ->
    execute(State, Calls, []).

%% @private
execute(State, [], Resps) ->
    {State, lists:reverse(Resps)};
execute(State, [Call | Rest], Resps) ->
    {State2, Resp} = jmap(State, Call),
    execute(State2, Rest, [Resp | Resps]).


%% @private
%% @doc Decode a JMAP call binary into a proplist data structure.
-spec decode(binary()) ->
    [jmap()].
decode(JSON) ->
    [jmap_to_erl(JMAPCall) || JMAPCall <- jsx:decode(JSON)].


%% @private
%% @doc Encode JMAP call proplists as JSON.
-spec encode([jmap()]) ->
    binary().
encode(JMAPs) ->
    jsx:encode([erl_to_jmap(JMAP) || JMAP <- JMAPs]).


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
-spec jmap(#state{}, jmap()) ->
    {#state{}, jmap()}.
%% connect
jmap(State, {<<"connect">>, Args, ClientID} = JMAPCall) ->
    case jmap_connect_parse_args(Args) of
        {ok, {ConnSpec, Auth}} ->
            Username = imap:auth_to_username(Auth),
            case lists:member(Username, switchboard:accounts()) of
                false ->
                    {ok, _} = switchboard:add(ConnSpec, Auth),
                    {State#state{connspec=ConnSpec, auth=Auth, owner=true},
                     {<<"connected">>, [{}], ClientID}};
                true ->
                    {State#state{connspec=ConnSpec, auth=Auth, owner=false},
                     {<<"connected">>, [{}], ClientID}}
            end;
        {error, _} ->
            {State, err(<<"badArgs">>, JMAPCall)}
    end;

%% idle
jmap(#state{auth=Auth} = State,
     {<<"idle">>, Args, ClientID} = JMAPCall) when Auth =/= none ->
    case proplists:get_value(<<"list">>, Args) of
        undefined ->
            {State, err(<<"noList">>, JMAPCall)};
        [] ->
            {State, err(<<"emptyList">>, JMAPCall)};
        Mailboxes when is_list(Mailboxes) ->
            %% XXX - race condition here, depending on if name registration
            %% occurs immediately
            Username = imap:auth_to_username(Auth),
            MonitoredSet = sets:from_list(switchboard:mailbox_monitors(Username)),
            MailboxesSet = sets:from_list(Mailboxes),
            RespArgs =
                case sets:fold(
                       fun(Mailbox, Acc) ->
                               switchboard:add_mailbox_monitor(Username, Mailbox),
                               Acc
                       end, [], sets:subtract(MailboxesSet, MonitoredSet)) of
                    [] ->
                        [{}];
                    Failed ->
                        [{failed, Failed}]
                end,
            %% XXX better way to do this without the catch?
            catch switchboard:subscribe(new),
            {State#state{idle_mailboxes=Mailboxes}, {<<"idling">>, RespArgs, ClientID}}
    end;

jmap(#state{auth=Auth} = State, {<<"getMailboxes">>, _, _} = JMAPCall)
  when Auth =/= none ->
    get_mailboxes(State, JMAPCall);
jmap(#state{auth=Auth} = State, {<<"getMessageList">>, _, _} = JMAPCall)
  when Auth =/= none ->
    get_message_list(State, JMAPCall);
jmap(State, JMAPCall) ->
    {State, err(<<"unknownMethod">>, JMAPCall)}.



%% @private
%% @doc Implements JMAP's getMailboxes.
%% @reference <a href="http://jmap.io/#getmailboxes">`getMailboxes'</a>
get_mailboxes(#state{auth=Auth} = State, JMAPCall) ->
    switchboard:with_imap(imap:auth_to_username(Auth),
                          fun(IMAP) -> get_mailboxes(IMAP, State, JMAPCall) end).

get_mailboxes(IMAP, State, JMAPCall) ->
    %% XXX - LIST "" "*" is aggressive, but so is getMailboxes
    {ok, ListResps} = imap:clean_list(imap:call(IMAP, {list, <<"">>, <<"*">>})),
    get_mailboxes(IMAP, State, JMAPCall, ListResps).

%% @private
%% @doc Before making Examine calls, check the state.
get_mailboxes(IMAP, State, JMAPCall, ListResps) ->
    UIDValidities = [case proplists:get_value(uidvalidity, R) of
                         undefined ->
                             <<"u">>;
                         UIDValidity ->
                             integer_to_binary(UIDValidity)
                     end || R <- ListResps],
    <<CurrentState:160/integer>> = crypto:hash(sha, UIDValidities),
    get_mailboxes(IMAP, State, JMAPCall, ListResps, CurrentState).

%% @private
get_mailboxes(IMAP, State, {_, Args, ClientID} = JMAPCall, ListResps, CurrentState) ->
    case CurrentState =:= proplists:get_value(<<"state">>, Args) of
        true ->
            {State, {<<"mailboxes">>, [{<<"state">>, CurrentState}], ClientID}};
        false ->
            get_mailboxes(IMAP, State, JMAPCall, CurrentState, ListResps, [])
    end.

%% @private
get_mailboxes(_IMAP, State, {_, _, ClientID}, ReplyState, [], Acc) ->
    {State, {<<"mailboxes">>,
             [{<<"state">>, ReplyState}, {<<"list">>, Acc}],
             ClientID}};
get_mailboxes(IMAP, State, JMAPCall, ReplyState, [ListResp | Rest], Acc) ->
    NameAttrs = proplists:get_value(name_attrs, ListResp),
    case lists:member(<<"\\Noselect">>, NameAttrs) of
        false ->
            Name = proplists:get_value(name, ListResp),
            {ok, JMAPMailbox} = get_mailbox(IMAP, Name),
            get_mailboxes(IMAP, State, JMAPCall, ReplyState, Rest,
                               [JMAPMailbox | Acc]);
        true ->
            get_mailboxes(IMAP, State, JMAPCall, ReplyState, Rest, Acc)
    end.


%% @private
%% @doc Create the JMAP mailbox response for the provided Mailbox and
%% IMAP connection.
-spec get_mailbox(pid(), binary()) ->
    {ok, [proplists:property()]} | {error, _}.
get_mailbox(IMAP, Mailbox) ->
    case imap:call(IMAP, {examine, Mailbox}) of
        {ok, {_, ExamineResps}} ->
            Resps = [imap:clean(Resp) || Resp <- ExamineResps],
            UIDValidity = proplists:get_value(uidvalidity, Resps),
            {ok, [{name, Mailbox}, {id, mailbox_id(Mailbox, UIDValidity)}]};
        {error, Reason} ->
            {error, Reason}
    end.


%% @private
%% @doc Implements JMAP's getMessageList.
%% @reference <a href="http://jmap.io/#getmessagelist">`getMailboxes'</a>
-spec get_message_list(#state{}, jmap()) ->
    {#state{}, jmap()}.
get_message_list(#state{auth=Auth} = State, JMAPCall) ->
    switchboard:with_imap(imap:auth_to_username(Auth),
                          fun(IMAP) -> get_message_list(IMAP, State, JMAPCall) end).

get_message_list(IMAP, State, {_, Args, _} = JMAPCall) ->
    get_message_list(IMAP, State, JMAPCall,
                     proplists:get_value(<<"mailboxId">>, Args)).

get_message_list(_IMAP, State, JMAPCall, undefined) ->
    {State, err(<<"missingMailboxId">>, JMAPCall)};
get_message_list(IMAP, State, {_, _, ClientID} = JMAPCall, MailboxId) ->
    case select_by_id(IMAP, MailboxId) of
        ok ->
            {ok, {_, Resps}} = imap:call(IMAP, {uid, {search, [<<"ALL">>]}}),
            [{search, Uids}] = imap:clean(Resps),
            {State, {<<"messageList">>,
                     [{<<"messageIds">>, [message_id(MailboxId, U) || U <- Uids]}],
                     ClientID}};
        {error, nomailbox} ->
            {State, err(<<"mailboxDoesNotExist">>, JMAPCall)}
    end.


%% @doc Select a mailbox using the provided jmap mailbox id.
%% Returns an error if the UIDValidity doesn't match.
-spec select_by_id(pid(), binary()) ->
    ok | {error, _}.
select_by_id(#state{auth=Auth}, MailboxId) ->
    switchboard:with_imap(imap:auth_to_username(Auth),
                          fun(IMAP) -> select_by_id(IMAP, MailboxId) end);

select_by_id(IMAP, MailboxId) when is_pid(IMAP) ->
    case catch decode_mailbox_id(MailboxId) of
        {'EXIT', Reason} ->
            {error, nomailbox};
        {Mailbox, UIDValidity} ->
            case imap:call(IMAP, {select, Mailbox}) of
                {ok, {_, RawResps}} ->
                    case proplists:get_value(uidvalidity, imap:clean(RawResps)) of
                        UIDValidity ->
                            ok;
                        _ ->
                            {error, nomailbox}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.


-spec mailbox_name_to_id(#state{}, binary()) ->
    binary().
mailbox_name_to_id(State, Name) ->
    {State, {<<"mailboxes">>, Args, <<"1">>}} =
        get_mailboxes(State, {<<"getMailboxes">>, [], <<"1">>}),
    InboxObject = case switchboard_util:take_first(
                   fun(Mailbox) ->
                           proplists:get_value(name, Mailbox) =:= Name
                   end, proplists:get_value(<<"list">>, Args, [])) of
                      undefined ->
                          [];
                      Else ->
                          Else
                  end,
    proplists:get_value(id, InboxObject).


%% @doc Encode a messageId from a mailboxId and UID.
-spec message_id(binary(), binary() | integer()) ->
    binary().
message_id(MailboxId, Uid) when is_integer(Uid) ->
    message_id(MailboxId, integer_to_binary(Uid));
message_id(MailboxId, Uid) when is_binary(Uid) ->
    <<MailboxId/binary, $u, Uid/binary>>.


%% @doc Encode a mailboxId from the mailbox name and UID validity number.
-spec mailbox_id(binary(), binary() | integer()) ->
    binary().
mailbox_id(MailboxName, UIDValidity) when is_integer(UIDValidity) ->
    mailbox_id(MailboxName, integer_to_binary(UIDValidity));
mailbox_id(MailboxName, UIDValidity) when is_binary(UIDValidity)  ->
    <<MailboxName/binary, $!, UIDValidity/binary>>.


-spec decode_mailbox_id(binary()) ->
    {binary(), non_neg_integer()}.
decode_mailbox_id(MailboxId) ->
    [Mailbox, UIDValidityBin] = binary:split(MailboxId, <<$!>>),
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

%% @private
%% @doc Create an error message.
-spec err(binary(), jmap()) ->
    jmap().
err(Type, {Method, Args, ClientID}) ->
    {<<"error">>,
     [{<<"type">>, Type},
      {<<"method">>, Method},
      {<<"arguments">>, Args}],
     ClientID}.


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
                     proplists:get_value(<<"url">>, TokenProps)}
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
       fun idle_assertions/1,
       fun select_by_id_assertions/1,
       fun get_message_list_assertions/1]}].

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
    {ssl, Host, Port} = ?DISPATCH_CONN_SPEC,
    Auth = ?DISPATCH_AUTH,
    Account = imap:auth_to_username(Auth),
    case lists:member(Account, switchboard:accounts()) of
        true ->
            OldIMAP = switchboard:where(Account, active),
            switchboard:stop(Account),
            switchboard_util:await_death(OldIMAP);
        false ->
            ok
    end,
    Connect = jmap(#state{}, {<<"connect">>,
                              [{<<"host">>, Host},
                               {<<"port">>, Port},
                               {<<"auth">>, imap:auth_to_props(Auth)}],
                              <<"1">>}),
    {IMAP, _} = gproc:await(switchboard:key_for(Account, active), 5000),
    [?_assertMatch({#state{connspec=?DISPATCH_CONN_SPEC, auth=Auth},
                    {<<"connected">>, _, <<"1">>}}, Connect),
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
    #state{connspec=ConnSpec, auth=Auth}.


%% @private
%% @doc Exit the test imap connection.
imap_teardown(#state{auth=Auth}) ->
    Username = imap:auth_to_username(Auth),
    IMAP = switchboard:where(Username, active),
    switchboard:stop(Username),
    switchboard_util:await_death(IMAP).


%% @private
%% @doc Assertions for the JMAP getMailboxes command. This test
%% calls getMailboxes twice, using the state returned by the first
%% call in the second.
get_mailboxes_assertions(State) ->
    %% This test depends on the state not actually changing.
    {State, {<<"mailboxes">>, Args, <<"1">>}} =
        get_mailboxes(State, {<<"getMailboxes">>, [], <<"1">>}),
    {State, {<<"mailboxes">>, Args2, <<"2">>}} =
        get_mailboxes(State, {<<"getMailboxes">>,
                              [{<<"state">>,
                                proplists:get_value(<<"state">>, Args)}],
                              <<"2">>}),
    [{"Assert that `state' didn't change between calls. Which could happen.",
     ?_assertEqual(proplists:get_value(<<"state">>, Args),
                   proplists:get_value(<<"state">>, Args2))},
     {"Assert that the `state' was returned in Args.",
      ?_assertNotEqual(undefined, proplists:get_value(<<"state">>, Args))},
     {"Because `state' wasn't in the first call's args, `list' should be returned.",
      ?_assertMatch(List when is_list(List), proplists:get_value(<<"list">>, Args))},
     {"Because `state' didn't change, `list' should not have been returned.",
      ?_assertEqual(undefined, proplists:get_value(<<"list">>, Args2))}].


%% @private
%% @doc Assertions for the select_by_id command.
%% @todo Write tests for failure conditions.
select_by_id_assertions(State) ->
    InboxID = mailbox_name_to_id(State, <<"INBOX">>),
    [?_assertNotEqual(InboxID, undefined),
     ?_assertEqual(ok, select_by_id(State, InboxID))].


%% @private
%% @doc Assertions for the idle command.
%% @todo Write tests for `failed` mailboxes.
idle_assertions(#state{auth=Auth} = State) ->
    %% XXX - these config vals should be passed through the opts...
    Account = imap:auth_to_username(Auth),
    IdleMailboxes = [<<"INBOX">>],
    [IdleMailbox] = IdleMailboxes,
    State2 = State#state{idle_mailboxes = IdleMailboxes},
    [?_assertMatch({State2, {<<"idling">>, [{}], <<"1">>}},
                   jmap(State, {<<"idle">>, [{<<"list">>, IdleMailboxes}], <<"1">>})),
     ?_assertMatch(P when is_pid(P), switchboard:await(Account, {idler, IdleMailbox})),
     ?_assertEqual(IdleMailboxes, switchboard:mailbox_monitors(Account))].

%% @private
get_message_list_assertions(State) ->
    InboxID = mailbox_name_to_id(State, <<"INBOX">>),
    {State, {<<"messageList">>, Args, <<"1">>}} =
     get_message_list(State,
                      {<<"getMessageList">>,
                       [{<<"mailboxId">>, InboxID}],
                       <<"1">>}),
    [?_assertMatch(L when is_list(L), proplists:get_value(<<"messageIds">>, Args))].

-endif.
