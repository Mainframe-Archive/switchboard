%% @doc the switchboard operator takes low level imap responses, and translates them...
-module(switchboard_operator).
-behaviour(gen_server).

%% Interface exports
-export([start_link/2,
         stop/1,
         dispatch_fun/2,
         update_uid/1]).

%% Callback exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% Records
-record(state, {account :: imap:account(),
                mailbox :: imap:mailbox(),
                last_uid = none :: none | integer(),
                last_uid_validity = none :: none | integer()}).

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc Start the switchboard_operator supervisor as part of the supervision tree.
-spec start_link(imap:account(), imap:mailbox()) ->
    supervisor:startlink_ret().
start_link(Account, Mailbox) ->
    gen_server:start_link(?MODULE, {Account, Mailbox}, []).


stop(Pid) ->
    gen_server:cast(Pid, stop).


%% @doc Returns the dispatch function that will send the idle results to the proper key.
-spec dispatch_fun(imap:account(), imap:mailbox()) ->
    fun((imap:response()) -> ok).
dispatch_fun(Account, Mailbox) ->
    Key = pubsub_key(Account, Mailbox),
    fun(Msg) ->
            gproc:send(Key, {idle, Msg}),
            ok
    end.


%% @doc Non-blocking command for the operator to update its LastUid.
-spec update_uid(pid()) ->
    ok.
update_uid(Oper) ->
    gen_server:cast(Oper, {uid, update}).



%%==============================================================================
%% Callback exports
%%==============================================================================

init({Account, Mailbox}) ->
    true = gproc:reg(pubsub_key(Account, Mailbox)),
    true = gproc:reg(imapswitchboard:key_for(Account, operator)),
    {ok, #state{account=Account, mailbox=Mailbox}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({uid, update}, State) ->
    {noreply, update_uid_internal(State)};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({idle, {'*', [_, <<"EXISTS">>]}}, State) ->
    {noreply, update_uid_internal(State)};
handle_info({idle, {'+', [<<"idling">>]}}, State) ->
    {noreply, State};
handle_info({idle, Msg}, State) ->
    lager:info("Unrecognized msg: ~p", [Msg]),
    {noreply, State};

%% Catchall
handle_info(_Info, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%==============================================================================
%% Internal functions
%%==============================================================================

%% @doc updates the State's uid, publishing messages as necessary
-spec update_uid_internal(#state{}) ->
    #state{}.
update_uid_internal(#state{account=Account,
                           mailbox=Mailbox,
                           last_uid=LastUid} = State) ->
    {ok, Uid} = current_uid(Account, Mailbox),
    % lager:info("UID: ~p, LastUid: ~p", [Uid, LastUid]),
    if LastUid =/= none ->
            {ok, Emails} = imap:call(imapswitchboard:where(Account, active),
                                     {uid, {fetch, {LastUid + 1, Uid}, <<"ALL">>}}),
            lists:foreach(
             fun({fetch, Data}) ->
                     imapswitchboard:publish(new, {new, Account, Data});
                (_) ->
                     ok
             end, lists:map(fun imap:clean/1, Emails));
            % lager:info("New Emails: ~p", [Emails]);
       true -> ok
    end,
    State#state{last_uid=Uid}.


%% @doc returns the current max uid for the given Account and Mailbox
-spec current_uid(binary(), imap:mailbox()) ->
    {ok, integer()}.
current_uid(Account, Mailbox) ->
    Active = imapswitchboard:where(Account, active),
    {ok, _} = imap:call(Active, {select, Mailbox}),
    {ok, [{'*', [BinUid, <<"FETCH">>, [<<"UID">>, BinUid]]},
          {'OK', [<<"Success">>]}]} = imap:call(Active, {uid, {fetch, '*', <<"UID">>}}),
    {ok, if is_integer(BinUid) -> BinUid;
            is_binary(BinUid)  -> binary_to_integer(BinUid)
         end}.


pubsub_key(Account, Mailbox) ->
    {p, l, {idler, Account, Mailbox}}.


-define(TEST, true).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

swichboard_operator_test_() ->
    [{foreach,
      fun() -> case lists:member(<<"dispatchonme@gmail.com">>,
                                 imapswitchboard:which()) of
                   true  -> [{<<"dispatchonme@gmail.com">>, <<"INBOX">>}];
                   false -> []
               end
      end,
      [fun update_uid_internal_assertions/1,
       fun current_uid_assertions/1]}].

update_uid_internal_assertions(Accounts) ->
    [?_assertMatch(#state{last_uid=Uid} when is_integer(Uid),
                   update_uid_internal(#state{account=A, mailbox=M}))
     || {A, M} <- Accounts].

current_uid_assertions(Accounts) ->
    [?_assertMatch({ok, _}, current_uid(A, M)) || {A, M} <- Accounts].

-endif.
