%% @doc
-module(imapswitchboard).

-export([start/0,
         add/2, add/3]).

%%==============================================================================
%% External API
%%==============================================================================

%% @doc start the imapswitchboard application
start() ->
    application:start(?MODULE).


%% @equiv add(ConnSpec, Auth, [])
-spec add(imap:connspec(), imap:auth()) ->
    supervisor:startchild_ret().
add(ConnSpec, Auth) ->
    add(ConnSpec, Auth, []).

%% @doc add a new account to be monitored
%% TODO return type?
-spec add(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startchild_ret().
add(ConnSpec, Auth, Mailboxes) ->
    switchboard_sup:start_child(ConnSpec, Auth, Mailboxes).
