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
%% @copyright Copyright (c) 2014, ThusFresh Inc.
%% @end
%%------------------------------------------------------------------------------

%% @private
%% @doc Supervisor for an idling IMAP process and the `imap_operator'.


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
    Account = imap:auth_to_account(Auth),
    Key = switchboard:key_for(Account, {idler_sup, Mailbox}),
    supervisor:start_link({via, gproc, Key}, ?MODULE, {ConnSpec, Auth, Mailbox}).


%%==============================================================================
%% Callback exports
%%==============================================================================

%% @private
init({ConnSpec, Auth, Mailbox}) ->
    RestartStrategy = one_for_one,
    MaxR = MaxT = 5,
    Account = imap:auth_to_account(Auth),
    DispatchFun = switchboard_operator:dispatch_fun(Account, Mailbox),
    ImapSpec = {imap,
                {imap, start_link,
                 [ConnSpec,
                  [{cmds, [{cmd, {call, {login, Auth}}},
                           {cmd, {call, {select, Mailbox}}},
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
