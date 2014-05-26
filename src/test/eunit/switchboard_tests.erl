%% @doc Tests for the switchboard module, and some for the app as a whole.
-module(switchboard_tests).
-include("switchboard.hrl").


-export([add_dispatch/0,
         where_dispatch/0,
         dispatch/0]).


%% @private Get the dispatch user's active Imap connection.
where_dispatch() ->
    switchboard:where(?DISPATCH, active).


%% @private Add the dispatch user.
add_dispatch() ->
    {ConnSpec, Auth} = dispatch(),
    switchboard:add(ConnSpec, Auth, [?DISPATCH_MAILBOX]).


%% @private Useful for the console
dispatch() ->
    %{?DISPATCH_CONN_SPEC, ?DISPATCH_AUTH}.
    {?DISPATCH_CONN_SPEC, {plain, <<"dispatchonme@gmail.com">>, <<"jives48_cars">>}}.



%% @private Run the suite of tests.
suite_test_() ->
    [add_stop_assertions(),
     pubsub_assertions(),
     {foreach,
      fun() -> add_dispatch(), ?DISPATCH end,
      fun(Account) -> ok = switchboard:stop(Account) end,
      [fun where_assertions/1,
       fun accounts_assertions/1,
       fun query_assertions/1]}].


%% @private
add_stop_assertions() ->
    [?_assertMatch({ok, _}, add_dispatch()),
     ?_assertEqual(ok, switchboard:stop(?DISPATCH))].


%% @private
pubsub_assertions() ->
    PubRecv = fun(Msg) ->
                      Msg = switchboard:publish(new, Msg),
                      receive R -> R after 100 -> timeout end
              end,
    [?_assertEqual(true, switchboard:subscribe(new)),
     ?_assertEqual(msg, PubRecv(msg)),
     ?_assertEqual(true, switchboard:unsubscribe(new))].


%% @private
where_assertions(Account) ->
    [?_assertMatch({P, _} when is_pid(P),
                   gproc:await(switchboard:key_for(Account, account))),
     ?_assertMatch({P, _} when is_pid(P),
                   gproc:await(switchboard:key_for(Account, active))),
     ?_assertMatch({P, _} when is_pid(P),
                   gproc:await(switchboard:key_for(Account,
                                                   {idler, ?DISPATCH_MAILBOX})))].


%% @private
accounts_assertions(Account) ->
    [?_assertEqual(switchboard:accounts(), [Account])].


%% @private test that the imap server can be queried
query_assertions(Account) ->
    {Active, _} = gproc:await(switchboard:key_for(Account, active)),
    [?_assertMatch({ok, _}, imap:call(Active, {select, <<"INBOX">>}))].
