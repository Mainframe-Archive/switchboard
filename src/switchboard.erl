%%------------------------------------------------------------------------------
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
%%
%% @author Thomas Moulia <jtmoulia@pocketknife.io>
%% @copyright Copyright (c) 2014, ThusFresh, Inc.
%% @end
%%------------------------------------------------------------------------------


%% @doc This module provides the main interface to Switchboard.

-module(switchboard).

-export([start/0,
         add/2, add/3,
         stop/1,
         key_for/2,
         register_callback/2,
         where/2,
         checkout/1,
         return/2,
         with_imap/2,
         accounts/0,
         subscribe/1,
         unsubscribe/1,
         publish/2]).

% -type process() :: account | active.
-type keytype() :: account | active | {idler | operator, imap:mailbox()}.
-type pubsub_channel() :: new.


%%==============================================================================
%% External API
%%==============================================================================

%% @doc Start the Switchboard application.
start() ->
    start_app(?MODULE).


%% @equiv add(ConnSpec, Auth, [])
-spec add(imap:connspec(), imap:auth()) ->
    supervisor:startchild_ret().
add(ConnSpec, Auth) ->
    add(ConnSpec, Auth, []).


%% @doc Add the specified account to be monitored by the switchboard application.
%% Each mailbox provided will be monitored for new emails, and notifications
%% of new email arrivals will be published to the <tt>new</tt> channel.
%%
%% Note: This will start an `active' IMAP connection which can be used for queries,
%% as well as one `idler' IMAP connection per mailbox given. Some email providers
%% <a href="https://support.google.com/mail/answer/97150?hl=en">limit</a> the number
%% of open imap connections.
%%
%% Once started, the IMAP client processes can be accessed using `where'.

-spec add(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startchild_ret().
add(ConnSpec, Auth, Mailboxes) ->
    switchboard_sup:start_child(ConnSpec, Auth, Mailboxes).


%% @doc Stop the account from being monitored. Unlike add, this only
%% requires the account name, e.g. <tt>dispatchonme@gmail.com</tt>

-spec stop(binary()) ->
    ok | {error, not_found | simple_one_for_one}.
stop(Account) ->
    switchboard_sup:stop_child(Account).


%% @doc Returns the key for the given account and process type.
%%
%% Switchboard processes are registered using descriptive erlang terms via
%% <a href="https://github.com/uwiger/gproc">gproc</a>. This function
%% wraps the term with gproc properties and a switchboard namespace.
%%
%% <dl>
%%   <dt>`active'</dt>
%%     <dd>The active IMAP client. While used internally by Switchboard,
%%         this can also be used externally to make requests.
%%     </dd>
%%   <dt>`{idler, Mailbox}'</dt>
%%     <dd>The IMAP client monitoring `Mailbox' via the `idle' command.
%%     </dd>
%%   <dt>... others</dt>
%%     <dd>Other processes are intended to be opaque.
%%     </dd>
%% </dl>

-spec key_for(Account, Type) ->
    {n, l, {switchboard, {Type, Account}}} when Account :: imap:account(),
                                                Type :: keytype().
key_for(Account, Type) ->
    {n, l, {switchboard, {Type, Account}}}.


%% @doc An IMAP InitCallback function used to register the process its called upon
%% with gproc.
%%
%% This is useful since processes can only be registered with gproc from within
%% themselves. It is currently used as an imap `post_init_callback'.

-spec register_callback(imap:account(), keytype()) ->
    fun((State) -> State) when State :: any().
register_callback(Account, Type) ->
    fun(State) ->
            true = gproc:reg(switchboard:key_for(Account, Type)),
            State
    end.


%% @doc Returns the process of type for the provided account.
%%
%% For example, `where(<<"dispatchonme@gmail.com">>, active)' would return
%% the `active' IMAP client for `dispatchonme@gmail.com'.
%%
%% @see key_for

%-spec where(any(), keytype()) ->
%    pid().
where(Account, Type) ->
    gproc:where(key_for(Account, Type)).


%% @todo implement a pool around active connections, checkout the active process
%% from this.
checkout(Account) ->
    {ok, where(Account, active)}.


%% @todo return a checked out IMAP connection.
return(_Account, _IMAP) ->
    ok.


%% @todo rethrow errors instead of eating them?
with_imap(Account, Fun) ->
    {ok, IMAP} = checkout(Account),
    Result = case catch Fun(IMAP) of
                 {'EXIT', Reason} ->
                     {error, Reason};
                 R ->
                     R
             end,
    ok = return(Account, IMAP),
    Result.


%% @doc Subscribe to Switchboard messages using the provided channel.
%%
%% Channels:
%% <dl>
%%   <dt><tt>new</tt></dt>
%%     <dd>This channel provides email arrival notifications. The messages should
%%         take the form of <tt>{new, {Account, Mailbox}, Item}</tt> where Item is
%%         the result of a cleaned ALL fetch (@see imap:call).
%%     </dd>
%% </dl>

-spec subscribe(pubsub_channel()) ->
    true.
subscribe(new) ->
    gproc:reg(pubsub_key_for(new)).


%% @doc Unsubscribe from the provided Switchboard channel.
%% @see subscribe

-spec unsubscribe(pubsub_channel()) ->
    true.
unsubscribe(new) ->
    gproc:unreg(pubsub_key_for(new)).


%% @doc Publish a message to the Switchboard channel.
%% @see subscribe

-spec publish(pubsub_channel(), Msg) ->
    Msg when Msg :: any().
publish(new, Msg) ->
    gproc:send(pubsub_key_for(new), Msg).


%% @doc Returns the list of accounts currently being managed by Switchboard.

-spec accounts() ->
    [binary()].
accounts() ->
    Key = {switchboard, {account, '$1'}},
    GProcKey = {'_', '_', Key},
    MatchHead = {GProcKey, '_', '_'},
    gproc:select([{MatchHead, [], ['$1']}]).


%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private
%% @doc Return the pubsub key for the given type.
pubsub_key_for(Type) ->
    {p, l, {switchboard, Type}}.


%% @private
%% @doc Start an app or list of apps.
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
