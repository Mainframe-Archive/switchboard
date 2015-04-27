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
%%
%% @author Thomas Moulia <jtmoulia@pocketknife.io>
%% @copyright Copyright (c) 2014, ThusFresh, Inc.
%% @end
%%------------------------------------------------------------------------------

%% @private
%% @doc An operator process is paired with a single IMAP `idler' for
%% each mailbox to be monitored. The idler sends its low-level IMAP
%% idle messages to the operator, which checks whether a new email has
%% arrived, and, if so, fetches and publishes a notification to the
%% `new' channel.


-module(switchboard_operator).
-include("switchboard.hrl").
-behaviour(gen_server).

%% Interface exports
-export([start_link/2,
         stop/1,
         dispatch_fun/2,
         update_uid/1,
         get_last_uid/1]).

%% Callback exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).


-ifdef(TEST).
-export([update_uid_internal/1,
         current_uid/2]).
-endif.


%% Records
-record(state, {account :: imap:account(),
                mailbox :: imap:mailbox(),
                last_uid = none :: none | integer(),
                last_uid_validity = none :: none | integer()}).

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc Start the operator as part of the supervision tree.
-spec start_link(imap:account(), imap:mailbox()) ->
    supervisor:startlink_ret().
start_link(Account, Mailbox) ->
    gen_server:start_link(?MODULE, {Account, Mailbox}, []).


%% @doc Stop the operator.
-spec stop(pid()) ->
    ok.
stop(Oper) ->
    gen_server:cast(Oper, stop).


%% @doc Commands for the operator to update its state using the latest email uid
%% in the operator's mailbox.

-spec update_uid(pid()) ->
    ok.
update_uid(Oper) ->
    gen_server:cast(Oper, {uid, update}).


%% @doc Returns the operator's last known UID.

-spec get_last_uid(pid()) ->
    integer().
get_last_uid(Oper) ->
    gen_server:call(Oper, {get, last_uid}).


%% @private
%% @doc Returns the dispatch function that will send the idle results to the
%% proper operator.

-spec dispatch_fun(imap:account(), imap:mailbox()) ->
    fun((imap:response()) -> ok).
dispatch_fun(Account, Mailbox) ->
    Key = pubsub_key(Account, Mailbox),
    fun(Msg) ->
            gproc:send(Key, {idle, Msg}),
            ok
    end.


%%==============================================================================
%% Callback exports
%%==============================================================================

%% @private
init({Account, Mailbox}) ->
    true = gproc:reg(pubsub_key(Account, Mailbox)),
    true = gproc:reg(switchboard:key_for(Account, {operator, Mailbox})),
    {ok, #state{account=Account, mailbox=Mailbox}, 0}.


%% @private
handle_call({get, last_uid}, _From, #state{last_uid=LastUid} = State) ->
    {reply, LastUid, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% @private
handle_cast({uid, update}, State) ->
    {noreply, update_uid_internal(State)};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


%% @private
handle_info(timeout, State) ->
    ok = update_uid(self()),
    {noreply, State};
handle_info({idle, {'*', [_, <<"EXISTS">>]}}, State) ->
    {noreply, update_uid_internal(State)};
handle_info({idle, {'*', [_, <<"EXPUNGE">>]}}, State) ->
    {noreply, update_uid_internal(State)};
handle_info({idle, {'+', [<<"idling">>]}}, State) ->
    {noreply, State};
handle_info({idle, Msg}, State) ->
    lager:info("Unrecognized msg: ~p", [Msg]),
    {noreply, State};

%% Catchall
handle_info(_Info, State) ->
    {noreply, State}.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
terminate(_Reason, _State) ->
    ok.


%%==============================================================================
%% Internal functions.
%%==============================================================================

%% @private
%% @doc Updates the State's uid, publishing messages as necessary.
-spec update_uid_internal(#state{}) ->
    #state{}.
update_uid_internal(#state{account=Account,
                           mailbox=Mailbox,
                           last_uid=LastUid} = State) ->
    {ok, Uid} = current_uid(Account, Mailbox),
    %% lager:debug("UID: ~p, LastUid: ~p", [Uid, LastUid]),
    if LastUid =/= none ->
            {ok, {_, Emails}} = imap:call(switchboard:where(Account, active),
                                     {uid, {fetch, {LastUid + 1, Uid}, <<"ALL">>}}),
            lists:foreach(
             fun({fetch, Data}) ->
                     switchboard:publish(new, {new, {Account, Mailbox}, Data})
             end, lists:map(fun imap:clean/1, Emails));
       %% lager:info("New Emails: ~p", [Emails]);
       true -> ok
    end,
    State#state{last_uid=Uid}.


%% @private
%% @doc Helper function to get the fetch command's result's UID.
get_fetch_uid([]) ->
    none;
get_fetch_uid([{'*', [_, <<"FETCH">>, [<<"UID">>, BinUid]]} | _]) ->
    BinUid;
get_fetch_uid([_ | Rest]) ->
    get_fetch_uid(Rest).


%% @private
%% @doc Returns the current max uid for the given Account and Mailbox.
-spec current_uid(binary(), imap:mailbox()) ->
    {ok, integer()}.
current_uid(Account, Mailbox) ->
    {Active, _} = gproc:await(switchboard:key_for(Account, active), 5000),
    {ok, _} = imap:call(Active, {select, Mailbox}),
    {ok, {_, Resps}} = imap:call(Active, {uid, {fetch, '*', <<"UID">>}}),
    BinUid = get_fetch_uid(Resps),
    {ok, if is_integer(BinUid) -> BinUid;
            is_binary(BinUid)  -> binary_to_integer(BinUid)
         end}.


pubsub_key(Account, Mailbox) ->
    {p, l, {idler, Account, Mailbox}}.


%%==============================================================================
%% EUnit tests.
%%==============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% XXX - this is all a bit weird since duplicate switchboard_operators are started.

switchboard_operator_test_() ->
    [{foreach,
      fun() ->
              {ok, _} = switchboard:add(?DISPATCH_CONN_SPEC, ?DISPATCH_AUTH),
              [{?DISPATCH, ?DISPATCH_MAILBOX}]
      end,
      fun([{Account, _} | _]) -> ok = switchboard:stop(Account) end,
      [fun update_uid_internal_asserts/1,
       fun current_uid_asserts/1]}].

update_uid_internal_asserts(Accounts) ->
    [?_assertMatch({state, _, _, LastUid, _} when is_integer(LastUid),
                   switchboard_operator:update_uid_internal(
                    {state, A, M, none, none})) %% This is sketchy....
     || {A, M} <- Accounts].

current_uid_asserts(Accounts) ->
    [?_assertMatch({ok, _},
                   switchboard_operator:current_uid(A, M)) || {A, M} <- Accounts].

-endif.
