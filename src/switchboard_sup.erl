%% @doc Top level supervisor for the imapswitchboard application
-module(switchboard_sup).
-behaviour(supervisor).

%% Interface exports
-export([start_link/0]).

%% Callback exports
-export([init/1]).

%%==============================================================================
%% Interface exports
%%==============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

%%==============================================================================
%% Callback exports
%%==============================================================================

init(no_args) ->
    RestartStrategy = simple_one_for_one,
    MaxR = MaxT = 5,
    OpersSpec = {switchboard_operator,
                 {switchboard_operator, start_link, []},
                 transient,
                 infinity,
                 worker,
                 [switchboard_operator]},
    {ok, {{RestartStrategy, MaxR, MaxT}, [OpersSpec]}}.

%%==============================================================================
%% Internal functions
%%==============================================================================
