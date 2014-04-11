%% @doc An imap connection
%% http://tools.ietf.org/html/rfc3501
%% In the API, binaries are used for string-like data.

-module(imap).
-behaviour(gen_server).

%% Interface exports
-export([start/1,
         start_link/1,
         stop/1,
         cast/2,
         call/2, call/3,
         recv/0, recv/1]).

%% Callback exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% Macros
% uncomment this if you want to enable debug mode
-define(CALL_TIMEOUT, 5000).
-define(DEBUG, true).

-ifdef(DEBUG).
    -define(LOG_DEBUG(Format, Data),
            ?LOG_INFO("*** DEBUG: " ++ Format ++ " ***", Data)).
    -define(LOG_ERROR(Fun, Format, Data),
            error_logger:error_msg("~p:~p(): " ++ Format ++ "~n",
                                   [?MODULE, Fun] ++ Data)).
    -define(LOG_WARNING(Fun, Format, Data),
            error_logger:warning_msg("~p:~p(): " ++ Format ++ "~n",
                                     [?MODULE, Fun] ++ Data)).
    -define(LOG_INFO(Format, Data), error_logger:info_msg(Format ++ "~n", Data)).
-else.
    -define(LOG_DEBUG(Format, Data), true).
    -define(LOG_ERROR(Fun, Format, Data), true).
    -define(LOG_WARNING(Fun, Format, Data), true).
    -define(LOG_INFO(Format, Data), true).
-endif.

%% Types
-type socket() :: ssl:sslsocket() | gen_tcp:socket().
-type connspec() :: {ssl | plain, Host :: binary(), Port :: integer()} |
                    {ssl | plain, Host :: binary(),
                     Port :: integer(), Options :: list()} |
                    {ssl | plain, Host :: binary(),
                     Port :: integer(), Options :: list(), Timeout :: integer()}.
-type auth_plain() :: {plain, Username :: binary(), Password :: binary()}.
-type auth_xoauth2() :: {xoauth2, Account :: binary(), Token :: binary()}.
-type auth() ::  auth_plain() | auth_xoauth2().

-type cmd() :: {login, auth()}
             | list
             | {select, mailbox()}
             | idle.

-type internal_cmd() :: {cmd, pid() | none, cmd()}.

%% Mailboxes
-type mailbox() :: binary().

%% Parse Types
-type parse_term() :: binary()               % Atom    [A-Za-z0-9+]+
                    | integer()              % Number  [0-9]+
                    | {string, binary()}     % String  "*" | {n}*
                    | [parse_term()]         % List    (parse_term())
                    | nil.                   % NIL     NIL

-type parse_state() :: none
                     | {string, binary()}
                     | {atom, binary()}
                     | {number, binary()}.


-export_type([connspec/0,
              auth_plain/0,
              auth_xoauth2/0,
              auth/0,
              mailbox/0]).


%% Records
-record(state, {socket :: socket(),
                connspec :: connspec(),
                auth = none :: none | auth(),
                tag = 0 :: integer(),
                buffer = <<>> :: binary(),
                parse_state = none :: parse_state(),
                token_acc = [] :: [parse_term()],
                cmds = gb_trees:empty() :: gb_trees:tree()}).

%%==============================================================================
%% Interface exports
%%==============================================================================

-ifdef(DEBUG).
-export([start_dispatch/0, parse/1]).
start_dispatch() ->
    {ok, Child} = start({ssl, <<"imap.gmail.com">>, 993}),
    {ok, _} = call(Child, {login, {plain, <<"dispatchonme@gmail.com">>,
                                   <<"jives48_cars">>}}),
    {ok, Child}.
-endif.

%% @doc start a standalone IMAP connection
-spec start(connspec()) ->
    {ok, pid()} | _.
start(ConnSpec) ->
    gen_server:start(?MODULE, ConnSpec, []).


%% @doc start an IMAP connection as part of the supervision tree
-spec start_link(connspec()) ->
    {ok, pid()} | _.
start_link(ConnSpec) ->
    gen_server:start_link(?MODULE, ConnSpec, []).


%% @doc stop the server
-spec stop(pid()) ->
    ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).


-spec cast(pid(), cmd()) ->
    ok.
cast(Server, Cmd) ->
    gen_server:cast(Server, {cmd, self(), Cmd}).


%% @equiv call(Server, Cmd, ?CALL_TIMEOUT)
-spec call(pid(), cmd()) ->
    {ok, [[parse_term()]]} | {error, _}.
call(Server, Cmd) ->
    call(Server, Cmd, ?CALL_TIMEOUT).

%% @doc call the command, waiting until timeout for all responses
-spec call(pid(), cmd(), integer()) ->
    {ok, [[parse_term()]]} | {error, _}.
call(Server, Cmd, Timeout) ->
    gen_server:cast(Server, {cmd, self(), Cmd}),
    Ref = monitor(process, Server),
    Responses = recv(Timeout, Ref),
    true = demonitor(Ref),
    Responses.


%% @equiv recv(?CALL_TIMEOUT)
-spec recv() ->
    {ok, [[parse_term()]]} | {error, _}.
recv() ->
    recv(?CALL_TIMEOUT).

%% @equiv recv(Timeout, none)
-spec recv(integer()) ->
    {ok, [[parse_term()]]} | {error, _}.
recv(Timeout) ->
    recv(Timeout, none).

%% @doc receive response until completion message, optional monitor used be call
-spec recv(integer(), reference() | none) ->
    {ok, [[parse_term()]]} | {error, _}.
recv(Timeout, MonitorRef) ->
    recv(Timeout, MonitorRef, []).

%% @private helper for recieving responses
-spec recv(integer(), reference() | none, [[parse_term()]]) ->
    {ok, [[parse_term()]]} | {error, _}.
recv(Timeout, MonitorRef, Responses) ->
    receive
        {'*', Response} ->
            recv(Timeout, [{'*', Response} | Responses]);
        {'OK', Response} ->
            {ok, lists:reverse([{'OK', Response} | Responses])};
        {'NO', Response} ->
            {error, lists:reverse([{'NO', Response} | Responses])};
        {'BAD', Response} ->
            {error, lists:reverse([{'BAD', Response} | Responses])};
        {'DOWN', MonitorRef, _, Reason} ->
            {error, {down, Reason, lists:reverse(Responses)}}
        after
            Timeout ->
              {error, timeout}
        end.

%%==============================================================================
%% Callback exports
%%==============================================================================

%% @doc init callback
-spec init(connspec()) ->
    {ok, #state{}} | {stop, _}.
init({SocketType, Host, Port}) ->
    init({SocketType, Host, Port, []});
init({SocketType, Host, Port, Options}) ->
    init({SocketType, Host, Port, Options, 5000});
init({SocketType, Host, Port, Options, Timeout} = ConnSpec) ->
    OptionDefaults = [binary],
    case SocketType:connect(binary_to_list(Host), Port, Options ++ OptionDefaults, Timeout) of
        {ok, Socket} ->
            {ok, #state{socket=Socket, connspec=ConnSpec}};
        {error, ssl_not_started} ->
            %% If ssl app isn't started, attempt to restart and then retry init
            start_app(ssl),
            init(ConnSpec);
        {error, Reason} ->
            {stop, Reason}
    end.

%% @doc handle synchronous calls
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle asynchronous casts
handle_cast({cmd, _, Cmd} = IntCmd, #state{cmds=Cmds, socket=Socket, tag=Tag} = State) ->
    CTag = <<$C, (integer_to_binary(Tag))/binary>>,
    ?LOG_DEBUG("SENDING: ~p", [cmd_to_data(Cmd)]),
    ok = ssl:send(Socket, [CTag, " " | cmd_to_data(Cmd)]),
    {noreply, State#state{cmds=gb_trees:insert(CTag, IntCmd, Cmds), tag=Tag+1}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


%% @doc handle messages
handle_info({ssl, Socket, Data}, #state{socket=Socket, buffer=Buffer} = State) ->
    ?LOG_DEBUG("data: ~p", [parse(Data)]),
    %% XXX ineffecient binary cat, buffer can be wrapped in a queue
    {noreply, churn_buffer(State#state{buffer=(<<Buffer/binary, Data/binary>>)})};
handle_info({ssl_closed, Socket}, #state{socket=Socket} = State) ->
    ?LOG_DEBUG("Socket Closed: ~p", [self()]),
    {stop, normal, State};
handle_info(Info, State) ->
    ?LOG_WARNING(handle_info, "unexpected: ~p", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(Reason, #state{socket=Socket, connspec={SocketType, _, _, _}}) ->
    ?LOG_WARNING(terminate, "TERMINATING ~p", [Reason]),
    SocketType:close(Socket);
terminate(Reason, _State) ->
    ?LOG_DEBUG("~p", [Reason]).


%%==============================================================================
%% Internal functions
%%==============================================================================

%% @doc churn the buffer by parsing responses and sending them to the proper processes
-spec churn_buffer(#state{}) ->
    #state{}.
churn_buffer(#state{buffer=Buffer, parse_state=ParseState, token_acc=TokenAcc} = State) ->
    {Result, {Buffer2, ParseState2, TokenAcc2}} = parse(Buffer, ParseState, TokenAcc),
    churn_buffer(State#state{buffer=Buffer2,
                             parse_state=ParseState2,
                             token_acc=TokenAcc2},
                 Result).


%% @private internal churn_buffer helper
-spec churn_buffer(#state{}, [parse_term()]) ->
    #state{}.
churn_buffer(State, none) ->
    State;
churn_buffer(#state{cmds=Cmds} = State, [<<"*">> | Response]) ->
    ?LOG_DEBUG("UNTAGGED: ~p", [Response]),
    [Pid ! {'*', Response} ||
        {cmd, Pid, Cmd} <- gb_trees:values(Cmds), cmds_response(Cmd, Response)],
    churn_buffer(State);
churn_buffer(State, [<<"+">> | Response]) ->
    ?LOG_DEBUG("Continuation: ~p", [Response]),
    churn_buffer(State);
churn_buffer(#state{cmds=Cmds} = State, [Tag | Response]) ->
    ?LOG_DEBUG("Tag: ~p, Rest: ~p", [Tag, Response]),
    churn_buffer(case gb_trees:lookup(Tag, Cmds) of
                     {value, {cmd, Pid, _}} ->
                         Pid ! case Response of
                                   [<<"OK">> | Rest]  -> {'OK', Rest};
                                   [<<"NO">> | Rest]  -> {'NO', Rest};
                                   [<<"BAD">> | Rest] -> {'BAD', Rest}
                               end,
                         State#state{cmds=gb_trees:delete(Tag, Cmds)};
                     none ->
                         ?LOG_WARNING(churn_buffer, "Unknown Cmd Tag: ~p", [Tag]),
                         State
                 end).


%% @doc returns true if the response is associated with the given cmd
-spec cmds_response(_, _) ->
    boolean().
cmds_response(_Cmd, _Response) ->
    false.

%% @doc return a command as iodata() ready to be sent to the IMAP server
-spec cmd_to_data(internal_cmd()) ->
    iodata().
cmd_to_data(InternalCmd) ->
    %% XXX -- ineffecient end append
    intersperse(" ", cmd_to_list(InternalCmd)) ++ ["\r\n"].


%% @doc return the list of command tokens
-spec cmd_to_list(internal_cmd()) ->
    iodata().
cmd_to_list({login, {plain, Username, Password}}) ->
    ["LOGIN", Username, Password];
cmd_to_list({select, Mailbox}) ->
    ["SELECT", Mailbox];
cmd_to_list(list) ->
    ["LIST"];
cmd_to_list(idle) ->
    ["IDLE"].


%% @doc intersperses the separator between list elements
-spec intersperse(Sep, [Sep | A]) ->
    [A] when Sep :: _, A :: _.
intersperse(_, []) ->
  [];
intersperse(_, [X]) ->
  [X];
intersperse(Sep, [X | Xs]) ->
  [X, Sep | intersperse(Sep, Xs)].


%% @doc start an app or list of apps
-spec start_app(atom() | [atom()]) ->
    ok.
start_app([]) ->
    ok;
start_app([App | Rest] = Apps) ->
    case application:start(App) of
        {error, {not_started, Unstarted}} ->
            start_app([Unstarted | Apps]);
        ok ->
            start_app(Rest);
        {error, {already_started, App}} ->
            start_app(Rest)
    end;
start_app(App) ->
    start_app([App]).


%%==============================================================================
%% Response Parsing
%%==============================================================================

-type parse_return() :: {[parse_term()] | none,
                         {binary(), parse_state(), [parse_term()]}}.

-spec parse(binary()) ->
    parse_return().
parse(Data) ->
    parse(Data, none, []).
%% XXX Relies on space after terms -- will drop on \r\n. TOKENIZE + LEX

-spec parse(binary(), parse_state(), [parse_term()]) ->
    parse_return().
parse(<<>>, ParseState, ParseAcc) ->
    {none, {<<>>, ParseState, ParseAcc}};

parse(<<$\r, $\n, Rest/binary>>, _, TokenAcc) ->
    {lists:reverse(TokenAcc), {Rest, none, []}};

parse(<<" ", Rest/binary>>, none, TokenAcc) ->
    parse(Rest, none, TokenAcc);

%% NIL
parse(<<"NIL", Rest/binary>>, none, TokenAcc) ->
    parse(Rest, none, [nil | TokenAcc]);

%% List
parse(<<$(, Rest/binary>>, none, TokenAcc) ->
    {List, {InnerRest, none, []}} = parse(Rest, none, []),
    parse(InnerRest, none, [List | TokenAcc]);
parse(<<$), Rest/binary>>, _, TokenAcc) ->
    {lists:reverse(TokenAcc), {Rest, none, []}};

%% String
parse(<<$", Rest/binary>>, none, TokenAcc) ->
    parse(Rest, {string, <<>>}, TokenAcc);
parse(<<$", Rest/binary>>, {string, StringAcc}, TokenAcc) ->
    parse(Rest, none, [{string, StringAcc} | TokenAcc]);
parse(<<$\\, $", Rest/binary>>, {string, StringAcc}, TokenAcc) ->
    parse(Rest, {string, <<StringAcc/binary, $">>}, TokenAcc);
parse(<<C, Rest/binary>>, {string, StringAcc}, TokenAcc) when C /= $"->
    parse(Rest, {string, <<StringAcc/binary, C>>}, TokenAcc);

%% Number
parse(<<" ", Rest/binary>>, {number, NumberAcc}, TokenAcc) ->
    parse(Rest, none, [binary_to_integer(NumberAcc) | TokenAcc]);
    %parse(Rest, none, [NumberAcc | TokenAcc]);
parse(<<D, Rest/binary>>, {number, NumberAcc}, TokenAcc) when D >= 48, D < 57 ->
    parse(Rest, {number, <<NumberAcc/binary, D>>}, TokenAcc);
parse(<<D, Rest/binary>>, none, TokenAcc) when D >= 48, D < 57 ->
    parse(Rest, {number, <<D>>}, TokenAcc);
parse(<<C, Rest/binary>>, {number, NumberAcc}, TokenAcc) when C >= 35, C < 123 ->
    parse(Rest, {atom, <<NumberAcc/binary, C>>}, TokenAcc);

%% Atom
%% XXX -- atoms as atoms +: simpler syntax, quicker matching, -: could blow the atom table
parse(<<" ", Rest/binary>>, {atom, AtomAcc}, TokenAcc) ->
    %% otherwise, {atom, <<"atomname">>}
    parse(Rest, none, [AtomAcc | TokenAcc]);
parse(<<C, Rest/binary>>, none, TokenAcc) when C >= 35, C < 123 ->
    parse(Rest, {atom, <<C>>}, TokenAcc);
parse(<<C, Rest/binary>>, {atom, AtomAcc}, TokenAcc) when C >= 35, C < 123 ->
    parse(Rest, {atom, <<AtomAcc/binary, C>>}, TokenAcc).


%%==============================================================================
%% Tests
%%==============================================================================

-define(TEST, true).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    [parse_atoms(),
     parse_numbers(),
     parse_nils(),
     parse_strings(),
     parse_lists()].


parse_atoms() ->
    [?_assertEqual({none, {<<>>, none, [<<"atom">>]}},
                   parse(<<"atom ">>))].

parse_nils() ->
    [?_assertEqual({none, {<<>>, none, [nil]}}, parse(<<"NIL">>)),
     ?_assertEqual({none, {<<>>, none, [nil]}}, parse(<<"NIL ">>))].


parse_numbers() ->
    [?_assertEqual({[0], {<<>>, none, []}}, parse(<<"0 \r\n">>)),
     ?_assertEqual({[21], {<<>>, none, []}}, parse(<<"21 \r\n">>))].

parse_strings() ->
    [?_assertEqual({none, {<<>>, none, [{string, <<"string">>}]}},
                   parse(<<"\"string\"">>))].

parse_lists() ->
    [?_assertEqual({none, {<<>>, none, [[{string, <<"a">>}, {string, <<"b">>}]]}},
                   parse(<<"(\"a\" \"b\")">>)),
     ?_assertEqual({[[{string, <<"a">>}, 1,
                      [{string, <<"c">>}, {string, <<"d">>}]],
                     {string, <<"e">>}, {string, <<"f">>}],
                    {<<>>, none, []}},
                   parse(<<"(\"a\" 1 (\"c\" \"d\")) \"e\" \"f\"\r\n">>))].

-endif.
