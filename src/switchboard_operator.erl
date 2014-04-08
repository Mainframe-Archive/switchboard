%% @doc The operator for a given account
-module(switchboard_operator).
-behaviour(supervisor).

%% Interface exports
-export([start_link/0]).

%% Callback exports
-export([init/1]).

%%==============================================================================
%% Interface exports
%%==============================================================================

start_link() ->
    supervisor:start_link(?MODULE, no_args).

%%==============================================================================
%% Callback exports
%%==============================================================================

init({ConnType, Auth}) ->
    RestartStrategy = one_for_all,
    MaxR = MaxT = 5,
    ConnType = ssl,
    % ActiveChildSpec = {active_imap,
    %                    {imap, start_link, [{ConnType, }]},
    %                    Restart = transient, % permanent | temporary
    %                    Shutdown = infinity, % brutal_kill | int() % ms
    %                    Type = worker, % supervisor
    %                    Modules = [child]}, % dynamic
    {ok, {{RestartStrategy, MaxR, MaxT}, []}}.

%%==============================================================================
%% Internal functions
%%==============================================================================
