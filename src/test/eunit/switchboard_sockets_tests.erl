%% @doc Tests for the dispatch_sockets module.
-module(switchboard_sockets_tests).
-include("switchboard.hrl").

suite_test_() ->
    [parse_seqset_asserts(),
     key_for_account_asserts(),
     {setup,
      fun handle_setup/0,
      fun handle_teardown/1,
      fun handle_asserts/1}].


%% @hidden Test parsing seqsets with dispatch_sockets:parse_seqset.
parse_seqset_asserts() ->
    [?_assertEqual(1, dispatch_sockets:parse_seqset(1)),
     ?_assertEqual([1, 2], dispatch_sockets:parse_seqset([1, 2])),
     ?_assertEqual({uid, 1}, dispatch_sockets:parse_seqset([{<<"uid">>, 1}])),
     ?_assertEqual({uid, [1, 2]},
                   dispatch_sockets:parse_seqset([{<<"uid">>, [1, 2]}]))].


%% @hidden Asserts for account_to_active helper.
key_for_account_asserts() ->
    %% @todo -- an example which does exist
    [?_assertEqual(undefined, dispatch_sockets:account_to_active(<<"idontexist">>))].

%%==============================================================================
%% Handle testing.
%%==============================================================================

-spec handle_setup() ->
    {imap:account(), pid()}.
handle_setup() ->
    {ConnSpec, Auth} = switchboard_tests:dispatch(),
    Account = imap:auth_to_username(Auth),
    {ok, _} = switchboard:add(ConnSpec, Auth),
    {Account, switchboard:where(Account, active)}.

handle_teardown({Account, _}) ->
    switchboard:stop(Account).

%% @hidden Test websockets handle fun.
handle_asserts({_, Active}) ->
    {Req, State} = {placeholder, placeholder},
    [% idle assertions
     [?_assertEqual(dispatch_sockets:reply(dispatch_sockets:reply([{idle, ok}]),
                                           Req, State),
                    dispatch_sockets:handle(<<"idle">>, Active, [], Req, State))]].

