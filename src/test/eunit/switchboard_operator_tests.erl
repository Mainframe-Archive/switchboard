-module(switchboard_operator_tests).
-include("switchboard.hrl").

%% XXX - this is all a bit weird since duplicate switchboard_operators are started.

swichboard_operator_test_() ->
    [{foreach,
      fun() ->
              {ok, _} = switchboard:add(?DISPATCH_CONN_SPEC, ?DISPATCH_AUTH),
              [{?DISPATCH, ?DISPATCH_MAILBOX}]
      end,
      fun([{Account, _} | _]) -> ok = switchboard:stop(Account) end,
      [fun update_uid_internal_assertions/1,
       fun current_uid_assertions/1]}].

update_uid_internal_assertions(Accounts) ->
    [?_assertMatch({state, _, _, LastUid, _} when is_integer(LastUid),
                   switchboard_operator:update_uid_internal(
                    {state, A, M, none, none})) %% This is sketchy....
     || {A, M} <- Accounts].

current_uid_assertions(Accounts) ->
    [?_assertMatch({ok, _},
                   switchboard_operator:current_uid(A, M)) || {A, M} <- Accounts].
