%% @doc the switchboard operator takes low level imap responses, and translates them...
-module(switchboard_operator).
-behaviour(gen_server).

%% Interface exports
-export([start_link/3,
         stop/1,
         dispatch_fun/3]).

%% Callback exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% Records
-record(state, {connspec :: imap:connspec(),
                auth :: imap:auth(),
                mailbox :: imap:mailbox(),
                last_uid = none :: none | integer(),
                last_uid_validity = none :: none | integer()}).

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc start the switchboard_operator supervisor as part of the supervision tree
-spec start_link(imap:connspec(), imap:auth(), imap:mailbox()) ->
    supervisor:startlink_ret().
start_link(ConnSpec, Auth, Mailbox) ->
    gen_server:start_link(?MODULE, {ConnSpec, Auth, Mailbox}, []).


stop(Pid) ->
    gen_server:cast(Pid, stop).


%% @doc returns the dispatch function that will send the idle results to the proper key
-spec dispatch_fun(imap:connspec(), imap:auth(), imap:mailbox()) ->
    fun((imap:response()) -> ok).
dispatch_fun(ConnSpec, Auth, Mailbox) ->
    Key = pubsub_key(ConnSpec, Auth, Mailbox),
    fun(Msg) ->
            gproc:send(Key, {idle, Msg}),
            ok
    end.


%%==============================================================================
%% Callback exports
%%==============================================================================

init({ConnSpec, Auth, Mailbox}) ->
    true = gproc:reg(pubsub_key(ConnSpec, Auth, Mailbox)),
    {ok, #state{connspec=ConnSpec, auth=Auth, mailbox=Mailbox}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({idle, {'*', [_, <<"EXISTS">>]}},
            #state{connspec=ConnSpec,
                   auth=Auth,
                   mailbox=Mailbox,
                   last_uid=LastUid} = State) ->
    Active = imapswitchboard:where(ConnSpec, Auth, active),
    {ok, _} = imap:call(Active, {select, Mailbox}),
    {ok, [{'*', [BinUid, <<"FETCH">>, [<<"UID">>, BinUid]]},
          {'OK', [<<"Success">>]}]} = imap:call(Active, {uid, {fetch, '*', <<"UID">>}}),
    Uid = if is_integer(BinUid) -> BinUid;
             is_binary(BinUid)  -> binary_to_integer(BinUid)
          end,
    % lager:info("UID: ~p, LastUid: ~p", [Uid, LastUid]),
    if LastUid =/= none ->
            {ok, Emails} = imap:call(Active,
                                     {uid, {fetch, {LastUid + 1, Uid}, <<"ALL">>}}),
            lists:foreach(
             fun({fetch, Data}) ->
                     imapswitchboard:publish(new, {new, {ConnSpec, Auth}, Data});
                (_) ->
                     ok
             end, lists:map(fun imap:clean/1, Emails));
            % lager:info("New Emails: ~p", [Emails]);
       true -> ok
    end,
    {noreply, State#state{last_uid=Uid}};
handle_info({idle, {'+', [<<"idling">>]}}, State) ->
    {noreply, State};
handle_info({idle, _Msg}, State) ->
    % lager:info("Unrecognized msg: ~p", [Msg]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%==============================================================================
%% Internal functions
%%==============================================================================


pubsub_key(ConnSpec, Auth, Mailbox) ->
    {p, l, {idler, ConnSpec, Auth, Mailbox}}.
