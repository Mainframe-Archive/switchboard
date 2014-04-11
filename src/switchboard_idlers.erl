%% @doc simple_one_for_one supervisor for imap idling processes
-module(switchboard_idlers).
-behaviour(supervisor).

%% Interface exports
-export([start_link/3,
         start_child/3]).

%% Callback exports
-export([init/1]).


%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc start the switchboard_idlers supervisor as part of the supervision tree
-spec start_link(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startlink_ret().
start_link(ConnSpec, Auth, Mailboxes) ->
    case supervisor:start_link(?MODULE, ConnSpec) of
        {ok, Pid} ->
            lists:foreach(
              fun(M) -> switchboard_idlers:start_child(Pid, Auth, M) end, Mailboxes),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc start an idler using the provided auth and mailbox
-spec start_child(supervisor:sup(), imap:auth(), imap:mailbox()) ->
    supervisor:startchild_ret().
start_child(Sup, Auth, Mailbox) ->
    case supervisor:start_child(Sup, []) of
        {ok, Child} ->
            %% Auth and listen on the given mailbox
            %% DO THIS WITH ACTUAL RETURNS
            ok = imap:cmd(Child, {login, Auth}),
            ok = imap:cmd(Child, {select, Mailbox}),
            ok = imap:cmd(Child, idle),
            {ok, Child};
        {error, Reason} ->
            {error, Reason}
    end.


%%==============================================================================
%% Callback exports
%%==============================================================================

init(ConnSpec) ->
    RestartStrategy =  simple_one_for_one,
    MaxR = MaxT = 5,
    ChildSpec = {idler,
                 {imap, start_link, [ConnSpec]},
                 transient,
                 5000, %% TODO switch these out with an application var
                 worker,
                 [imap]},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}.
