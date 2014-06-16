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
%% @doc Top level supervisor for the Switchboard application.


-module(switchboard_sup).
-behaviour(supervisor).

%% Interface exports
-export([start_link/0]).

%% Callback exports
-export([init/1]).

%% Some worker processes, e.g. `reloader', are only started when DEBUG is set.
-include("switchboard.hrl").
-define(WORKER(M, F, A), {M, {M, F, A}, permanent, 5000, worker, [M]}).
-ifdef(DEBUG).
-define(WORKERS, [?WORKER(reloader, start_link, [])]).
-else.
-define(WORKERS, []).
-endif.


%%==============================================================================
%% Public interface.
%%==============================================================================

%% @doc Start the supervisor as part of the supervision tree.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).


%%==============================================================================
%% Supervisor callbacks.
%%==============================================================================

%% @private
init(no_args) ->
    RestartStrategy = one_for_one,
    MaxR = MaxT = 5,
    AccountsSupSpec = {switchboard_accounts_sup,
                       {switchboard_accounts_sup, start_link, []},
                       transient,
                       infinity,
                       supervisor,
                       [switchboard_accounts_sup]},
    {ok, {{RestartStrategy, MaxR, MaxT}, [AccountsSupSpec | ?WORKERS]}}.

