%% @doc simple_one_for_one supervisor for imap idling processes
-module(switchboard_idlers).
-behaviour(supervisor).

%% Interface exports
-export([start_link/3,
         start_child/2]).

%% Callback exports
-export([init/1]).


%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc start the switchboard_idlers supervisor as part of the supervision tree
-spec start_link(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startlink_ret().
start_link(ConnSpec, Auth, Mailboxes) ->
    case supervisor:start_link(?MODULE, {ConnSpec, Auth}) of
        {ok, Pid} ->
            lists:foreach(
              fun(M) -> start_child(Pid, M) end, Mailboxes),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc start an idler using the provided auth and mailbox
-spec start_child(supervisor:sup(), imap:mailbox()) ->
    supervisor:startchild_ret().
start_child(Sup, Mailbox) ->
    supervisor:start_child(Sup, [Mailbox]).


%%==============================================================================
%% Callback exports
%%==============================================================================

init({ConnSpec, Auth}) ->
    RestartStrategy =  simple_one_for_one,
    MaxR = MaxT = 5,
    ChildSpec = {idler,
                 {switchboard_idler, start_link, [ConnSpec, Auth]},
                 transient,
                 infinity,
                 supervisor,
                 [switchboard_idler]},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}.



%%==============================================================================
%% Internal functions
%%==============================================================================

