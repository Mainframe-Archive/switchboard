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

%% @private
%% @doc The top level supervisor for an account.


-module(switchboard_accounts).
-behaviour(supervisor).

%% Interface exports.
-export([start_link/3]).

%% Callback exports.
-export([init/1]).


%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc Start the switchboard_account_sup as part of a supervision tree.
-spec start_link(imap:connspec(), imap:auth(), [imap:mailbox()]) ->
    supervisor:startlink_ret().
start_link(ConnSpec, Auth, Mailboxes) ->
    supervisor:start_link(?MODULE, {ConnSpec, Auth, Mailboxes}).


%%==============================================================================
%% Callback exports
%%==============================================================================

%% @private
init({ConnSpec, Auth, Mailboxes}) ->
    Account = imap:auth_to_account(Auth),
    true = gproc:reg(switchboard:key_for(Account, account)),
    RestartStrategy = one_for_all,
    MaxR = MaxT = 5,
    ActiveChildSpec = {active,
                       {imap, start_link,
                        [ConnSpec,
                         [{cmds, [{cmd, {call, {login, Auth}}}]},
                          {post_init_callback,
                           switchboard:register_callback(Account, active)}]]},
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
