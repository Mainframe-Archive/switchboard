%% @doc
-module(imapswitchboard).

-export([start/0,
         add/2, add/3,
         stop/1,
         key_for/2,
         register_callback/2,
         where/2,
         which/0,
         subscribe/1,
         unsubscribe/1,
         publish/2]).


-define(TEST, true).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([add_dispatch/0,
         where_dispatch/0,
         dispatch/0]).
-endif.

% -type process() :: account | active.
-type keytype() :: account | active | {idler, imap:mailbox()}.


%%==============================================================================
%% External API
%%==============================================================================

%% @doc start the imapswitchboard application
start() ->
    start_app(?MODULE).


%% @equiv add(ConnSpec, Auth, [])
-spec add(imap:connspec(), imap:auth()) ->
    supervisor:startchild_ret().
add(ConnSpec, Auth) ->
    add(ConnSpec, Auth, []).

%% @doc add a new account to be monitored
%% @todo return type?
-spec add(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startchild_ret().
add(ConnSpec, Auth, Mailboxes) ->
    switchboard_sup:start_child(ConnSpec, Auth, Mailboxes).


%% @doc stop the account from being monitored, wraps switchboard_sup fun
-spec stop(binary()) ->
    ok | {error, not_found | simple_one_for_one}.
stop(Account) ->
    switchboard_sup:stop_child(Account).



%% @doc returns the key for the given Username and Type
-spec key_for(Account, Type) ->
    {n, l, {imapswitchboard, {Type, Account}}} when Account :: imap:account(),
                                                    Type :: keytype().
key_for(Account, Type) ->
    {n, l, {imapswitchboard, {Type, Account}}}.


%% @doc An imap InitCallback fun to register the process with gproc.
-spec register_callback(imap:account(), keytype()) ->
    fun((State) -> State) when State :: any().
register_callback(Account, Type) ->
    fun(State) ->
            true = gproc:reg(imapswitchboard:key_for(Account, Type)),
            State
    end.


%% @doc returns the process registered with the given properties
-spec where(any(), keytype()) ->
    pid().
where(Account, Type) ->
    gproc:where(key_for(Account, Type)).


%% @doc subscribe to Type
-spec subscribe(any()) ->
    true.
subscribe(new) ->
    gproc:reg(pubsub_key_for(new)).


%% @doc unsubscribe from Type
-spec unsubscribe(any()) ->
    true.
unsubscribe(new) ->
    gproc:unreg(pubsub_key_for(new)).


%% @doc publish a message to Type
-spec publish(any(), Msg) ->
    Msg when Msg :: any().
publish(new, Msg) ->
    gproc:send(pubsub_key_for(new), Msg).


%% @doc returns the list of active accounts
-spec which() ->
    [binary()].
which() ->
    Key = {imapswitchboard, {account, '$1'}},
    GProcKey = {'_', '_', Key},
    MatchHead = {GProcKey, '_', '_'},
    gproc:select([{MatchHead, [], ['$1']}]).


%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @doc return the pubsub key for the given type
pubsub_key_for(Type) ->
    {p, l, {imapswitchboard, Type}}.


%% @doc start an app or list of apps
-spec start_app(atom() | [atom()]) ->
    ok.
start_app([]) ->
    ok;
start_app([App | Rest] = Apps) ->
    case application:start(App) of
        {error, {not_started, Unstarted}} ->
            start_app([Unstarted | Apps]);
        ok ->
            start_app(Rest);
        {error, {already_started, App}} ->
            start_app(Rest)
    end;
start_app(App) ->
    start_app([App]).


-ifdef(TEST).

-define(DISPATCH, <<"dispatchonme@gmail.com">>).
-define(DISPATCH_MAILBOX, <<"INBOX">>).

%% @doc get the dispatch user
where_dispatch() ->
    where(?DISPATCH, active).


%% Testing
add_dispatch() ->
    {ConnSpec, Auth} = dispatch(),
    add(ConnSpec, Auth, [?DISPATCH_MAILBOX]).

dispatch() ->
    {{ssl, <<"imap.gmail.com">>, 993},
     {plain, ?DISPATCH, <<"jives48_cars">>}}.


interface_test_() ->
    [add_stop_assertions(),
     pubsub_assertions(),
     {foreach,
      fun() -> add_dispatch(), ?DISPATCH end,
      fun(Account) -> ok = stop(Account) end,
      [fun where_assertions/1,
       fun which_assertions/1]}].

%% @private
add_stop_assertions() ->
    [?_assertMatch({ok, _}, add_dispatch()),
     ?_assertEqual(ok, stop(?DISPATCH))].

%% @private
pubsub_assertions() ->
    PubRecv = fun(Msg) ->
                      Msg = publish(new, Msg),
                      receive R -> R after 100 -> timeout end
              end,
    [?_assertEqual(true, subscribe(new)),
     ?_assertEqual(msg, PubRecv(msg)),
     ?_assertEqual(true, unsubscribe(new))].

%% @private
where_assertions(Account) ->
    [?_assert(is_pid(where(Account, account))),
     ?_assert(is_pid(where(Account, active))),
     ?_assert(is_pid(where(Account, {idler, ?DISPATCH_MAILBOX})))].

%% @private
which_assertions(Account) ->
    [?_assertEqual(which(), [Account])].

-endif.
