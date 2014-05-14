%%------------------------------------------------------------------------------
%% @author Thomas Moulia <jtmoulia@pocketknife.io>
%%
%% @copyright Copyright (c) 2014, Spatch
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% 1. Redistributions of source code must retain the above copyright notice, this
%% list of conditions and the following disclaimer.
%%
%% 2. Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%%
%% 3. Neither the name of the copyright holder nor the names of its contributors
%% may be used to endorse or promote products derived from this software without
%% specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
%% THE POSSIBILITY OF SUCH DAMAGE.
%% @end
%%
%% @doc API for the switchboard email application.
%% @end
%%------------------------------------------------------------------------------
-module(switchboard).

-export([start/0,
         add/2, add/3,
         stop/1,
         key_for/2,
         register_callback/2,
         where/2,
         which/0,
         subscribe/1,
         unsubscribe/1,
         publish/2]).

% -type process() :: account | active.
-type keytype() :: account | active | {idler, imap:mailbox()}.


%%==============================================================================
%% External API
%%==============================================================================

%% @doc start the switchboard application
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


%% @doc stop the account from being monitored, wraps switchboard_sup fun
-spec stop(binary()) ->
    ok | {error, not_found | simple_one_for_one}.
stop(Account) ->
    switchboard_sup:stop_child(Account).


%% @doc returns the key for the given Username and Type
-spec key_for(Account, Type) ->
    {n, l, {switchboard, {Type, Account}}} when Account :: imap:account(),
                                                Type :: keytype().
key_for(Account, Type) ->
    {n, l, {switchboard, {Type, Account}}}.


%% @doc An imap InitCallback fun to register the process its called on  with gproc.
-spec register_callback(imap:account(), keytype()) ->
    fun((State) -> State) when State :: any().
register_callback(Account, Type) ->
    fun(State) ->
            true = gproc:reg(switchboard:key_for(Account, Type)),
            State
    end.


%% @doc returns the process registered with the given properties
-spec where(any(), keytype()) ->
    pid().
where(Account, Type) ->
    gproc:where(key_for(Account, Type)).


%% @doc subscribe to Type
-spec subscribe(any()) ->
    true.
subscribe(new) ->
    gproc:reg(pubsub_key_for(new)).


%% @doc unsubscribe from Type
-spec unsubscribe(any()) ->
    true.
unsubscribe(new) ->
    gproc:unreg(pubsub_key_for(new)).


%% @doc publish a message to Type
-spec publish(any(), Msg) ->
    Msg when Msg :: any().
publish(new, Msg) ->
    gproc:send(pubsub_key_for(new), Msg).


%% @doc returns the list of active accounts
-spec which() ->
    [binary()].
which() ->
    Key = {switchboard, {account, '$1'}},
    GProcKey = {'_', '_', Key},
    MatchHead = {GProcKey, '_', '_'},
    gproc:select([{MatchHead, [], ['$1']}]).


%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @doc return the pubsub key for the given type
pubsub_key_for(Type) ->
    {p, l, {switchboard, Type}}.


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
