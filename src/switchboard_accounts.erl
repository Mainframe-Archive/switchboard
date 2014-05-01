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
    true = gproc:reg(switchboard:key_for(Account, account)),
    RestartStrategy = one_for_all,
    MaxR = MaxT = 5,
    ActiveChildSpec = {active,
                       {imap, start_link,
                        [ConnSpec,
                         [{cmds, [{cmd, {call, {login, Auth}}}]},
                          {post_init_callback,
                           switchboard:register_callback(Account, active)}]]},
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
