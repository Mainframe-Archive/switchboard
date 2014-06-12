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

%% @doc This module provides a websocket worker interface.
%%
%% At the moment, their are two different interfaces for accessing
%% Switchboard:
%%
%% <ul>
%%   <li>
%%     The <b>client interface</b> acts as a normal proxy. When a
%%     newly connected client sends the `connect' command Switchboard
%%     adds the account to the supervision tree. The While the client is
%%     connected it cannot connect any more accounts, and it can only
%%     issue commands for the collected account. When the client disconnects,
%%     Switchboard removes the account, retaining no information other
%%     than what is in the logs by default.
%%   </li>
%%
%%   <li>
%%     The <b>worker interface</b> allows emails to be processed across
%%     all accounts.
%%   </li>
%% </ul>
%%
%% This module implements the worker interface similarly to the client's
%% JMAP implementation. The differences are:
%%
%% <ul>
%%   <li>The worker can connect many accounts.</li>
%%   <li>The worker can use the `idleAll' command to idle across all accounts,
%%       even those connected over the client interface.</li>
%%   <li>Other than for the connect command, command arguments include an account,
%%       specifying what account to run the command against.</li>
%% </ul>
%% @end

-module(switchboard_workers).
-behaviour(cowboy_websocket_handler).
-include("switchboard.hrl").

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-record(state, {}).


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
websocket_handle({text, JSON}, Req, State) ->
    {Resps, State2} = call_all(switchboard_jmap:decode(JSON), State),
    lager:info("Resps: ~p", [Resps]),
    {reply, {text, switchboard_jmap:encode(Resps)}, Req, State2};
websocket_handle(Data, Req, State) ->
    lager:warning("Unexpected data: ~p", [Data]),
    {ok, Req, State}.


%% @private
websocket_info({new, {Account, Mailbox}, Item}, Req, State) ->
    MailboxId = switchboard_jmap:mailbox_name_to_id(Account, Mailbox),
    Resp = [{mailboxId, MailboxId},
            {messageId,
             switchboard_jmap:message_id(MailboxId, proplists:get_value(uid, Item))}],
    Reply = {<<"newMessage">>, Resp, undefined},
    lager:debug("Worker: Received new msg: ~p", [Item]),
    {reply, {text, switchboard_jmap:encode([Reply])}, Req, State};
websocket_info(Info, Req, State) ->
    lager:warning("Unexpected info: ~p", [Info]),
    {ok, Req, State}.


%% @private
websocket_terminate(_Reason, _Req, _State) ->
    ok.


%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private
%% @doc Execute a list of worker commands.
-spec call_all([switchboard_jmap:json()], #state{}) ->
    {[switchboard_jmap:json()], #state{}}.
call_all(Cmds, State) ->
    call_all(Cmds, State, []).

call_all([], State, Acc) ->
    {lists:reverse(Acc), State};
call_all([{<<"watchAll">>, [{}], ClientID} | Rest], State, Acc) ->
    %% @todo catch is dirty -- handle errors?
    catch switchboard:subscribe(new),
    Response = {<<"watchingAll">>, [{}], ClientID},
    call_all(Rest, State, [Response | Acc]);
call_all([{<<"watchAll">>, _, _} = Cmd | Rest], State, Acc) ->
    Response = switchboard_jmap:err(badArgs, Cmd),
    call_all(Rest, State, [Response | Acc]);
call_all([{<<"connect">>, Args, ClientID} = Cmd | Rest], State, Acc) ->
    case switchboard_jmap:connect(Args) of
        {ok, _Account, _ConnSpec, _Owner} ->
            call_all(Rest, State, [{<<"connected">>, [{}], ClientID} | Acc]);
        {error, Reason} ->
            call_all(Rest, State, [switchboard_jmap:err(Reason, Cmd) | Acc])
    end;
call_all([{Method, Args, ClientID} = Cmd | Rest], State, Acc) ->
    case proplists:get_value(<<"account">>, Args) of
        undefined ->
            call_all(Rest, State, [switchboard_jmap:err(noAccount, Cmd) | Acc]);
        Account ->
            case lists:member(Account, switchboard:accounts()) of
                true ->
                    JMAPState = switchboard_jmap:state_by_account(Account),
                    Accountless = proplists:delete(<<"account">>, Args),
                    {Response, _} =
                        switchboard_jmap:call({Method, Accountless, ClientID},
                                              JMAPState),
                    call_all(Rest, State, [Response | Acc]);
                false ->
                    Response = switchboard_jmap:err(inactiveAccount, Cmd),
                    call_all(Rest, State, [Response | Acc])
            end
    end.


%%==============================================================================
%% EUnit tests.
%%==============================================================================

-ifdef(TEST).

-define(TEST_CMDS, [[<<"getMessages">>, [{<<"account">>, ?DISPATCH}]]]).

suite_test_() ->
    [].

-endif.
