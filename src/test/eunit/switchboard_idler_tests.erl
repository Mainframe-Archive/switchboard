-module(switchboard_idler_tests).

-include("switchboard.hrl").

idler_test_() ->
    {foreach,
     fun idler_setup/0,
     fun idler_teardown/1,
     [fun reg_asserts/1]}.

-spec idler_setup() ->
    {{imap:connspec(), imap:auth()}, imap:mailbox(), pid()}.
idler_setup() ->
    {ConnSpec, Auth} = {?DISPATCH_CONN_SPEC, ?DISPATCH_AUTH},
    Mailbox = <<"INBOX">>,
    {ok, Pid} = switchboard_idler:start_link(ConnSpec, Auth, Mailbox),
    {{ConnSpec, Auth}, Mailbox, Pid}.

-spec idler_teardown({{imap:connspec(), imap:auth()}, imap:mailbox(), pid()}) ->
    ok.
idler_teardown({_, _, Pid}) ->
    true = exit(Pid, normal),
    ok.

%% @hidden Assert that the the processes have registered.
-spec reg_asserts({{imap:connspec(), imap:auth()}, imap:mailbox(), pid()}) ->
    [any()].
reg_asserts({{_ConnSpec, Auth}, Mailbox, _}) ->
    Account = imap:auth_to_username(Auth),
    [?_assertMatch({Pid, _} when is_pid(Pid),
                   gproc:await(switchboard:key_for(Account, {Type, Mailbox})))
     || Type <- [idler, operator]].
