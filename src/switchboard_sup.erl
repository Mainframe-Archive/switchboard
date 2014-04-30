%% @doc Top level supervisor for the switchboard application
-module(switchboard_sup).
-behaviour(supervisor).

%% Interface exports
-export([start_link/0,
         start_child/3,
         stop_child/1]).

%% Callback exports
-export([init/1]).


%%==============================================================================
%% Interface exports
%%==============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).


%% @doc add a new account to be monitored
-spec start_child(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startchild_ret().
start_child(ConnSpec, Auth, Mailboxes) ->
    case supervisor:start_child(?MODULE, [ConnSpec, Auth, Mailboxes]) of
        {ok, Child} ->
            {ok, Child};
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc stop the child given the provided identifiers
-spec stop_child(binary()) ->
    ok | {error, not_found | simple_one_for_one}.
stop_child(Account) ->
    ChildPid = switchboard:where(Account, account),
    supervisor:terminate_child(?MODULE, ChildPid).


%%==============================================================================
%% Callback exports
%%==============================================================================

init(no_args) ->
    RestartStrategy = simple_one_for_one,
    MaxR = MaxT = 5,
    AccountsSpec = {switchboard_accounts,
                    {switchboard_accounts, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [switchboard_account_sup]},
    {ok, {{RestartStrategy, MaxR, MaxT}, [AccountsSpec]}}.
