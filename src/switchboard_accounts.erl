%% @doc The top level supervisor for an account.
-module(switchboard_accounts).
-behaviour(supervisor).

%% Interface exports.
-export([start_link/3]).

%% Callback exports.
-export([init/1]).


%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc Start the switchboard_account_sup as part of a supervision tree.
-spec start_link(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startlink_ret().
start_link(ConnSpec, Auth, Mailboxes) ->
    supervisor:start_link(?MODULE, {ConnSpec, Auth, Mailboxes}).


%%==============================================================================
%% Callback exports
%%==============================================================================

init({ConnSpec, Auth, Mailboxes}) ->
    Account = imap:auth_to_username(Auth),
    true = gproc:reg(imapswitchboard:key_for(Account, account)),
    RestartStrategy = one_for_all,
    MaxR = MaxT = 5,
    ActiveChildSpec = {active,
                       {imap, start_link,
                        [ConnSpec,
                         [{cmds, [{cmd, {call, {login, Auth}}}]},
                          {post_init_callback,
                           imapswitchboard:register_callback(Account, active)}]]},
                       permanent,
                       5000,
                       worker,
                       [imap]},
    IdlersChildSpec = {idlers,
                       {switchboard_idlers, start_link, [ConnSpec, Auth, Mailboxes]},
                       permanent,
                       infinity,
                       supervisor,
                       [switchboard_idlers]},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ActiveChildSpec,
                                          IdlersChildSpec]}}.



%%==============================================================================
%% Eunit.
%%==============================================================================

-define(TEST, true).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accounts_test_() ->
    {foreach,
     fun accounts_setup/0,
     fun accounts_teardown/1,
     [fun accounts_reg_asserts/1]}.

-spec accounts_setup() ->
    {{imap:connspec(), imap:auth()}, [imap:mailbox()], pid()}.
accounts_setup() ->
    {ConnSpec, Auth} = imapswitchboard:dispatch(),
    Mailboxes = [<<"INBOX">>],
    {ok, Pid} = start_link(ConnSpec, Auth, Mailboxes),
    {{ConnSpec, Auth}, Mailboxes, Pid}.

-spec accounts_teardown({{imap:connspec(), imap:auth()}, [imap:mailbox()], pid()}) ->
    ok.
accounts_teardown({_, _, Pid}) ->
    true = exit(Pid, normal),
    ok.

%% @hidden assert that the the processes have registered
-spec accounts_reg_asserts({{imap:connspec(), imap:auth()}, [imap:mailbox()], pid()}) ->
    [any()].
accounts_reg_asserts({{_ConnSpec, Auth}, Mailboxes, _}) ->
    Account = imap:auth_to_username(Auth),
    [[?_assertMatch({Pid, _} when is_pid(Pid),
                    gproc:await(imapswitchboard:key_for(Account, {Type, Mailbox})))
      || Type <- [idler, operator], Mailbox <- Mailboxes],
     [?_assertMatch({Pid, _} when is_pid(Pid),
                    gproc:await(imapswitchboard:key_for(Account, Type)))
      || Type <- [active, account]]].

-endif.
