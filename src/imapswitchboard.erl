%% @doc
-module(imapswitchboard).

-export([start/0,
         add/2, add/3,
         get/3,
         key_for/3]).

-export([add_dispatch/0]).

-type process() :: account | active.

%%==============================================================================
%% External API
%%==============================================================================

%% @doc start the imapswitchboard application
start() ->
    start_app(?MODULE).


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


add_dispatch() ->
    add({ssl, <<"imap.gmail.com">>, 993},
        {plain, <<"dispatchonme@gmail.com">>, <<"jives48_cars">>},
        [<<"INBOX">>]).


get(ConnSpec, Auth, Process) ->
    gproc:where(switchboard_accounts:key_for(ConnSpec, Auth, Process)).


%% TODO -- this will be used
-spec key_for(imap:connspec(), imap:auth(), account | active) ->
    {n, l, {switchboard, {process(), imap:connspec()}}}.
key_for(ConnSpec, Auth, account) ->
    {n, l, {switchboard, {account, {ConnSpec, Auth}}}};
key_for(ConnSpec, Auth, active) ->
    {n, l, {switchboard, {active, {ConnSpec, Auth}}}}.



%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @doc start an app or list of apps
-spec start_app(atom() | [atom()]) ->
    ok.
start_app([]) ->
    ok;
start_app([App | Rest] = Apps) ->
    case application:start(App) of
        {error, {not_started, Unstarted}} ->
            start_app([Unstarted | Apps]);
        ok ->
            start_app(Rest);
        {error, {already_started, App}} ->
            start_app(Rest)
    end;
start_app(App) ->
    start_app([App]).
