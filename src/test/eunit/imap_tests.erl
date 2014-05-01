-module(imap_tests).
-include("switchboard.hrl").
-export([start_dispatch/0]).

-define(FETCH, {'*',[37,<<"FETCH">>,
                     [<<"UID">>,37,<<"BODY">>,
                      [[{string,<<"TEXT">>},
                        {string,<<"PLAIN">>},
                        [{string,<<"CHARSET">>},{string,<<"utf-8">>}],
                        nil,nil,
                        {string,<<"QUOTED-PRINTABLE">>},
                        87,5],
                       [{string,<<"TEXT">>},
                        {string,<<"HTML">>},
                        [{string,<<"CHARSET">>},{string,<<"utf-8">>}],
                        nil,nil,
                        {string,<<"QUOTED-PRINTABLE">>},
                        850,11],
                       {string,<<"ALTERNATIVE">>}],
                      <<"ENVELOPE">>,
                      [{string,<<"Thu, 24 Apr 2014 22:16:08 -0700">>},
                       {string,<<"never">>},
                       [[{string,<<"John Doe">>},
                         nil,
                         {string,<<"john">>},
                         {string,<<"gmail.com">>}]],
                       [[{string,<<"John Doe">>},
                         nil,
                         {string,<<"john">>},
                         {string,<<"gmail.com">>}]],
                       [[{string,<<"John Doe">>},
                         nil,
                         {string,<<"john">>},
                         {string,<<"gmail.com">>}]],
                       [[nil,nil,
                         {string,<<"dispatchonme">>},
                         {string,<<"gmail.com">>}]],
                       nil,nil,nil,
                       {string,<<"<etPan.5359ef98.ded7263.112@Thomass-MacBook-Pro.local>">>}],
                      <<"FLAGS">>,[],<<"INTERNALDATE">>,
                      {string,<<"25-Apr-2014 05:16:11 +0000">>},
                      <<"RFC822.SIZE">>,3856]]}).


-define(ADDRESSES, [[{string,<<"John Doe">>},
                     nil,
                     {string,<<"john">>},
                     {string,<<"gmail.com">>}],
                    [{string,<<"Jane Doe">>},
                     nil,
                     {string,<<"jane">>},
                     {string,<<"gmail.com">>}]]).


%% @private start the dispatch user -- useful for testing
start_dispatch() ->
    {ok, Child} = imap:start({ssl, <<"imap.gmail.com">>, 993}),
    {ok, _} = imap:call(Child, {login, {plain, <<"dispatchonme@gmail.com">>,
                                        <<"jives48_cars">>}}),
    {ok, Child}.


%%==============================================================================
%% Clean tests.
%%==============================================================================

clean_suite_test_() ->
    [clean_address_assertions(),
     clean_assertions()].

clean_address_assertions() ->
    [?_assertEqual([{address, [{name, <<"John Doe">>}, {email, <<"john@gmail.com">>}]},
                    {address, [{name, <<"Jane Doe">>}, {email, <<"jane@gmail.com">>}]}],
                   imap:clean_addresses(?ADDRESSES))].

clean_assertions() ->
    [?_assertMatch({fetch, [{uid, 37},
                            {body, _},
                            {envelope, _},
                            {flags, []},
                            {internaldate, <<"25-Apr-2014 05:16:11 +0000">>},
                            {rfc822size, 3856}]},
                   imap:clean(?FETCH))].


%% Commands
cmd_test_() ->
    [?_assertEqual([<<"UID">>, <<"FETCH">>, <<"1:9">>, <<"full">>],
                   imap:cmd_to_list({uid, {fetch, {1, 9}}})),
     seqset_to_list_assertions()].

seqset_to_list_assertions() ->
    [?_assertEqual(<<"1">>, imap:seqset_to_list(1)),
     ?_assertEqual(<<"1:9">>, imap:seqset_to_list({1, 9}))].


%% Tokenize
pop_token_test_() ->
    [pop_token_atoms(),
     pop_token_numbers(),
     pop_token_quoted()].

pop_token_atoms() ->
    [?_assertEqual({<<"atom">>, <<" ">>, none}, imap:pop_token(<<"atom ">>)),
     ?_assertEqual({<<"atom123">>, <<" ">>, none}, imap:pop_token(<<"atom123 ">>))].

pop_token_numbers() ->
    [?_assertEqual({0, <<" ">>, none}, imap:pop_token(<<"0 ">>)),
     ?_assertEqual({123, <<" ">>, none}, imap:pop_token(<<"123 ">>))].

pop_token_quoted() ->
    [?_assertEqual({{string, <<"quoted">>}, <<>>, none},
                   imap:pop_token(<<"\"quoted\"">>))].


tokenize_test_() ->
    [tokenize_terms()].

tokenize_terms() ->
    [?_assertEqual({[1, 2, <<"a">>], <<>>, none}, imap:tokenize(<<"1 2 a ">>)),
     ?_assertEqual({[1, 2, '(', <<"a">>, ')', {string, <<"s">>}], <<>>, none},
                   imap:tokenize(<<"1 2 (a)\"s\"">>))].


%% Parse
parse_test_() ->
    [parse_atoms(),
     parse_numbers(),
     parse_nils(),
     parse_strings(),
     parse_lists()].

parse_atoms() ->
    [?_assertEqual({none, [], [<<"atom">>]},
                   imap:parse([<<"atom">>]))].

parse_nils() ->
    [?_assertEqual({none, [], [nil]}, imap:parse([nil]))].


parse_numbers() ->
    [?_assertEqual({[0], [], []}, imap:parse([0, crlf])),
     ?_assertEqual({[21], [], []}, imap:parse([21, crlf]))].

parse_strings() ->
    [?_assertEqual({[{string, <<"string">>}], [], []},
                   imap:parse([{string, <<"string">>}, crlf]))].

parse_lists() ->
    [?_assertEqual({[1, [2, <<"c">>], 4], [], []},
                   imap:parse([1, '(', 2, <<"c">>, ')', 4, crlf]))].


%% decode_line testing.
decode_line_test_() ->
    [decode_line_default()].


decode_line_default() ->
    [?_assertEqual({[1, 2, 3], {<<>>, none}, {[], []}},
                   imap:decode_line(<<"1 2 3\r\n">>))].


%%==============================================================================
%% Live tests [dispatchonme@gmail.com]
%%==============================================================================

-ifdef(LIVE_TEST).
-type dispatch_test_spec() :: {{imap:connspec(), imap:auth()}, pid()}.

dispatch_live_suite_test_() ->
    {foreach,
     fun dispatch_setup/0,
     fun dispatch_teardown/1,
     [fun dispatch_select_assertions/1]}.


-spec dispatch_setup() ->
    dispatch_test_spec().
dispatch_setup() ->
    {ConnSpec, Auth} = switchboard:dispatch(),
    TestPid = self(),
    {ok, Imap} = imap:start_link(ConnSpec,
                                 [{cmds, [{cmd, {call, {login, Auth}}}]},
                                  {post_init_callback,
                                   fun(State) ->
                                           TestPid ! {imap, ready},
                                           State
                                   end}]),
    ok = receive {imap, ready} -> ok after 5000 -> timeout end,
    {{ConnSpec, Auth}, Imap}.


-spec dispatch_teardown(dispatch_test_spec()) ->
    ok.
dispatch_teardown({_, Imap}) ->
    imap:stop(Imap).

dispatch_select_assertions({_, Imap}) ->
    [?_assertMatch({ok, _}, imap:call(Imap, {select, <<"INBOX">>}))].

-endif.
