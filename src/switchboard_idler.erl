%% @doc supervise the idling IMAP process and the operator

-module(switchboard_idler).
-behaviour(supervisor).

%% Interface exports
-export([start_link/3]).

%% Callback exports
-export([init/1]).

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc start the switchboard_idler supervisor as part of the supervision tree
-spec start_link(imap:connspec(), imap:auth(), imap:mailbox()) ->
    supervisor:startlink_ret().
start_link(ConnSpec, Auth, Mailbox) ->
    supervisor:start_link(?MODULE, {ConnSpec, Auth, Mailbox}).


%%==============================================================================
%% Callback exports
%%==============================================================================

init({ConnSpec, Auth, Mailbox}) ->
    RestartStrategy = one_for_one,
    MaxR = MaxT = 5,
    Account = imap:auth_to_username(Auth),
    DispatchFun = switchboard_operator:dispatch_fun(Account, Mailbox),
    ImapSpec = {imap,
                {imap, start_link,
                 [ConnSpec,
                  [{cmds, [{cmd, {call, {login, Auth}}},
                           {cmd, {call, {select, <<"INBOX">>}}},
                           {cmd, {cast, idle}, [{dispatch, DispatchFun}]}]},
                   {post_init_callback,
                    switchboard:register_callback(Account, {idler, Mailbox})}]]},
                permanent,
                5000, %% TODO switch these out with an application var
                worker,
                [imap]},
    OperatorSpec = {switchboard_operator,
                    {switchboard_operator, start_link, [Account, Mailbox]},
                    permanent,
                    5000, %% TODO switch these out with an application var
                    worker,
                    [imap]},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ImapSpec, OperatorSpec]}}.
