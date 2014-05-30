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

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).


-record(state, {connspec = none :: imap:connspec() | none,
                auth = none :: imap:auth() | none}).

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
websocket_handle(Data, Req, State) when is_binary(Data) ->
    Calls = decode(Data),
    {State2, Resps} = execute(State, Calls),
    {reply, {text, encode(Resps)}, Req, State2};
websocket_handle(Data, Req, State) ->
    lager:info("Unexpected data: ~p", [Data]),
    {ok, Req, State}.


%% @private
websocket_info(Info, Req, State) ->
    lager:info("Unexpected info: ~p", [Info]),
    {ok, Req, State}.


%% @private
websocket_terminate(_Reason, _Req, #state{auth=Auth}) when Auth =/= none ->
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

-spec execute(binary(), [jmap()]) ->
    [jmap()].
execute(State, Calls) ->
    [jmap(State, Call) || Call <- Calls].


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
    jmap().
jmap(State, {<<"connect">>, Args, ClientID} = JMAPCall) ->
    case jmap_connect_parse_args(Args) of
        {ok, {ConnSpec, Auth}} ->
            lager:info("Accounts: ~p", [switchboard:accounts()]),
            {ok, _} = switchboard:add(ConnSpec, Auth),
            {State#state{connspec=ConnSpec, auth=Auth},
             {<<"connected">>, [], ClientID}};
        {error, _} ->
            {State, err(<<"badArgs">>, JMAPCall)}
    end;
jmap(#state{auth=Auth} = State, JMAP) ->
    %% @todo for bad cmds, an imap connection is checked out. Bad.
    Username = imap:auth_to_username(Auth),
    switchboard:with_imap(Username, fun(IMAP) -> jmap(IMAP, State, JMAP) end).


%% @private
%% @doc Execute a JMAP command using the provided account.
-spec jmap(pid(), #state{}, jmap()) ->
    jmap().
jmap(IMAP, State, {<<"getMailboxes">>, [], ClientID}) ->
    lager:info("~p", [imap:call(IMAP, list)]),
    {ok, ListResps} = imap:clean_list(imap:call(IMAP, list)),
    Mailboxes = [jmap_get_mailbox(IMAP, proplists:get_value(name, ListResp)) ||
                    ListResp <- ListResps],
    lager:info("getMailboxes results: ~p", [Mailboxes]),
    CmdState = <<"state">>,
    {State,
     {<<"mailboxes">>, [{<<"state">>, CmdState}, {<<"list">>, Mailboxes}], ClientID}};
jmap(_IMAP, _State, JMAP) ->
    err(<<"unknownMethod">>, JMAP).


%% @private
%% @doc Create the JMAP mailbox response for the provided Mailbox and
%% IMAP connection.
-spec jmap_get_mailbox(pid(), binary()) ->
    {ok, [proplists:property()]} | {error, _}.
jmap_get_mailbox(IMAP, Mailbox) ->
    case imap:call(IMAP, {examine, Mailbox}) of
        {ok, {_, ExamineResps}} ->
            Resps = [imap:clean(Resp) || Resp <- ExamineResps],
            UIDValidity = proplists:get_value(uid_validity, Resps),
            {ok, [{name, Mailbox}, {id, jmap_mailbox_id(Mailbox, UIDValidity)}]};
        {error, Reason} ->
            {error, Reason}
    end.


jmap_mailbox_id(MailboxName, UIDValidity) ->
    <<MailboxName/binary, "-", UIDValidity/binary>>.


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
    {error,
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
    [decode_encode_asserts(),
     jmap_connect_asserts(),
     {foreach,
      fun imap_setup/0,
      fun imap_teardown/1,
      [fun get_mailboxes_asserts/1]}].

%% @private
%% @doc Assertions for `encode/1' and `decode/1'.
decode_encode_asserts() ->
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
jmap_connect_asserts() ->
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
%% @doc Setup the test imap connection.
imap_setup() ->
    Auth = ?DISPATCH_AUTH,
    Username = imap:auth_to_username(?DISPATCH_AUTH),
    {ok, _} = switchboard:add(?DISPATCH_CONN_SPEC, Auth),
    {IMAP, _} = gproc:await(switchboard:key_for(Username, active), 5000),
    {Username, IMAP}.


%% @private
%% @doc Exit the test imap connection.
imap_teardown({Username, IMAP}) ->
    switchboard:stop(Username),
    switchboard_util:await_death(IMAP).


%% @private
%% @doc Assertions for the JMAP getMailboxes command.
get_mailboxes_asserts({_, IMAP}) ->
    [?_assertMatch({#state{}, {<<"mailboxes">>, Args, <<"1">>}} when is_list(Args),
                   jmap(IMAP, #state{}, {<<"getMailboxes">>, [], <<"1">>}))].

-endif.
