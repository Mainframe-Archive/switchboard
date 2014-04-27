%% @doc The top level supervisor for an account.
-module(switchboard_accounts).
-behaviour(supervisor).

%% Interface exports.
-export([start_link/3,
         which/2]).

%% Callback exports.
-export([init/1]).

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc Start the switchboard_account_sup as part of a supervision tree.
-spec start_link(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startlink_ret().
start_link(ConnSpec, Auth, Mailboxes) ->
    case supervisor:start_link(?MODULE, {ConnSpec, Auth, Mailboxes}) of
        {ok, Pid} ->
            %% Auth the active connection.
            {ok, Active} = which(Pid, active),
            {ok, _} = imap:call(Active, {login, Auth}),
            %% The Operator can now update its PID.
            Oper = imapswitchboard:where(imap:auth_to_username(Auth), operator),
            ok = switchboard_operator:update_uid(Oper),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Get the child with the given Id.
-spec which(supervisor:sup(), supervisor:child_id()) ->
    supervisor:child().
which(Sup, Id) ->
    case [C || {I, C, _, _} <- supervisor:which_children(Sup), I =:= Id] of
        [Child] ->
            {ok, Child};
        [] ->
            {error, undefined}
    end.



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
                         [{init_callback,
                           fun() ->
                                   gproc:reg_or_locate(
                                     imapswitchboard:key_for(Account, active))
                           end}]]},
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
%% Internal functions
%%==============================================================================
