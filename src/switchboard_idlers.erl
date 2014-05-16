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
%% @copyright Copyright (c) 2014, Spatch
%% @end
%%------------------------------------------------------------------------------

%% @doc `simple_one_for_one supervisor' for imap idling processes.


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

%% @private
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

