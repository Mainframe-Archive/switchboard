%% @doc
-module(imapswitchboard).

-export([start/0,
         add/2, add/3,
         stop/2,
         key_for/3,
         where/3,
         subscribe/1,
         publish/2]).

%% TODO clear this off
-export([add_dispatch/0,
         where_dispatch/0,
         dispatch/0]).

-type process() :: account | active.
-type keytype() :: account | active | {idler, imap:mailbox()}.


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
%% @todo return type?
-spec add(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startchild_ret().
add(ConnSpec, Auth, Mailboxes) ->
    switchboard_sup:start_child(ConnSpec, Auth, Mailboxes).


%% @doc stop the account from being monitored
-spec stop(imap:connspec(), imap:auth()) ->
    ok | {error, not_found | simple_one_for_one}.
stop(ConnSpec, Auth) ->
    switchboard_sup:stop_child(ConnSpec, Auth).


%% Testing
add_dispatch() ->
    add({ssl, <<"imap.gmail.com">>, 993},
        {plain, <<"dispatchonme@gmail.com">>, <<"jives48_cars">>},
        [<<"INBOX">>]).

dispatch() ->
    {{ssl, <<"imap.gmail.com">>, 993},
     {plain, <<"dispatchonme@gmail.com">>, <<"jives48_cars">>}}.


%% TODO -- this will be used to generate gproc keys
-spec key_for(imap:connspec(), imap:auth(), Type) ->
    {n, l, {imapswitchboard, {Type, {process(), imap:connspec()}}}}
        when Type :: keytype().
key_for(ConnSpec, Auth, Type) ->
    {n, l, {imapswitchboard, {Type, {ConnSpec, Auth}}}}.


%% TODO -- this will be used to generate gproc keys
-spec where(imap:connspec(), imap:auth(), keytype()) ->
    pid().
where(ConnSpec, Auth, Type) ->
    gproc:where(key_for(ConnSpec, Auth, Type)).


where_dispatch() ->
    where({ssl, <<"imap.gmail.com">>, 993},
          {plain, <<"dispatchonme@gmail.com">>, <<"jives48_cars">>},
          active).


subscribe(new) ->
    gproc:reg({p, l, {imapswitchboard, new}}).


publish(new, Msg) ->
    gproc:send({p, l, {imapswitchboard, new}}, Msg).


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
