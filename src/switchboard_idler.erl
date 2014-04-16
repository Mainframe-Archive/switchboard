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
    {ok, IdlerSup} = supervisor:start_link(?MODULE, {ConnSpec, Auth, Mailbox}),
    ImapIdler = gproc:where(imapswitchboard:key_for(ConnSpec, Auth, {idler, Mailbox})),
    {ok, _} = imap:call(ImapIdler, {login, Auth}),
    {ok, _} = imap:call(ImapIdler, {select, Mailbox}),
    DispatchFun = switchboard_operator:dispatch_fun(ConnSpec, Auth, Mailbox),
    ok = imap:cast(ImapIdler, idle, [{dispatch, DispatchFun}]),
    {ok, IdlerSup}.


%%==============================================================================
%% Callback exports
%%==============================================================================

init({ConnSpec, Auth, Mailbox}) ->
    RestartStrategy = one_for_one,
    MaxR = MaxT = 5,
    ImapSpec = {imap,
                {imap, start_link,
                 [ConnSpec,
                  [{init_callback,
                           fun() ->
                                   gproc:reg_or_locate(
                                     imapswitchboard:key_for(ConnSpec, Auth,
                                                             {idler, Mailbox}))
                           end}]]},
                permanent,
                5000, %% TODO switch these out with an application var
                worker,
                [imap]},
    OperatorSpec = {switchboard_operator,
                    {switchboard_operator, start_link, [ConnSpec, Auth, Mailbox]},
                    permanent,
                    5000, %% TODO switch these out with an application var
                    worker,
                    [imap]},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ImapSpec, OperatorSpec]}}.
