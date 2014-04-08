%% @doc
-module(imapswitchboard).

-export([start/0]).

%%==============================================================================
%% External API
%%==============================================================================

%% @doc start the imapswitchboard application
start() ->
    application:start(?MODULE).
