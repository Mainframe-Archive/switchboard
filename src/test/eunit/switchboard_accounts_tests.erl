-module(switchboard_accounts_tests).
-include("switchboard.hrl").

accounts_test_() ->
    {foreach,
     fun accounts_setup/0,
     fun accounts_teardown/1,
     [fun accounts_reg_asserts/1,
      fun active_asserts/1,
      fun operator_asserts/1]}.

-spec accounts_setup() ->
    {{imap:connspec(), imap:auth()}, [imap:mailbox()], pid()}.
accounts_setup() ->
    {ConnSpec, Auth} = switchboard_tests:dispatch(),
    Mailboxes = [<<"INBOX">>],
    {ok, Pid} = switchboard_accounts:start_link(ConnSpec, Auth, Mailboxes),
    {{ConnSpec, Auth}, Mailboxes, Pid}.

-spec accounts_teardown({{imap:connspec(), imap:auth()}, [imap:mailbox()], pid()}) ->
    ok.
accounts_teardown({_, _, Pid}) ->
    true = exit(Pid, normal),
    timer:sleep(200),
    ok.

%% @hidden assert that the the processes have registered
-spec accounts_reg_asserts({{imap:connspec(), imap:auth()}, [imap:mailbox()], pid()}) ->
    [any()].
accounts_reg_asserts({{_ConnSpec, Auth}, Mailboxes, _}) ->
    Account = imap:auth_to_username(Auth),
    [[?_assertMatch({Pid, _} when is_pid(Pid),
                    gproc:await(switchboard:key_for(Account, {Type, Mailbox})))
      || Type <- [idler, operator], Mailbox <- Mailboxes],
     [?_assertMatch({Pid, _} when is_pid(Pid),
                    gproc:await(switchboard:key_for(Account, Type)))
      || Type <- [active, account]]].

-spec active_asserts({{imap:connspec(), imap:auth()}, [imap:mailbox()], pid()}) ->
    [any()].
active_asserts({{_, Auth}, [Mailbox | _], _}) ->
    Account = imap:auth_to_username(Auth),
    {Active, _} = gproc:await(switchboard:key_for(Account, active)),
    [?_assertMatch({ok, _}, imap:call(Active, {select, Mailbox})),
     ?_assertMatch({ok, _}, imap:call(Active, {fetch, 1, [<<"UID">>]}))].

%% @hidden Assert that the operator is behaving as expected.
-spec operator_asserts({{imap:connspec(), imap:auth()}, [imap:mailbox()], pid()}) ->
    [any()].
operator_asserts({{_Connspec, Auth}, Mailboxes, _}) ->
    Account = imap:auth_to_username(Auth),
    lists:map(fun(Mailbox) ->
                      Operator = switchboard:where(Account, {operator, Mailbox}),
                      ?_assert(is_integer(switchboard_operator:get_last_uid(Operator)))
              end, Mailboxes).
