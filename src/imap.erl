%% @doc An imap connection
%% http://tools.ietf.org/html/rfc3501
%% In the API, binaries are used for string-like data.

-module(imap).
-behaviour(gen_server).
-export([parse/1]).

%% Interface exports
-export([start/1,
         start_link/1,
         stop/1,
         cmd/2]).

%% Callback exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% Macros
% uncomment this if you want to enable debug mode
-define(DEBUG, true).

-ifdef(DEBUG).
    -define(LOG_DEBUG(Format, Data), ?LOG_INFO("*** DEBUG: " ++ Format ++ " ***", Data)).
    -define(LOG_ERROR(Fun, Format, Data), error_logger:error_msg("~p:~p(): " ++ Format ++ "~n", [?MODULE, Fun] ++ Data)).
    -define(LOG_WARNING(Fun, Format, Data), error_logger:warning_msg("~p:~p(): " ++ Format ++ "~n", [?MODULE, Fun] ++ Data)).
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
-type parse_result() :: {atom, binary()}         % Atom    [A-Za-z0-9+]+
                        | integer()              % Number  [0-9]+
                        | {string, binary()}     % String  "*" | {n}*
                        | [parse_result()]       % List    (parse_result())
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
                %% XXX - buffer as binary means I have to cat binaries -- it would likely be more efficient to use a queue of binaries
                buffer = <<>> :: binary(),
                parse_state = none :: parse_state(),
                token_acc = [] :: [parse_result()],
                cmds = queue:new() :: queue(),
                active = none :: none | {Tag :: binary(), internal_cmd()}}).

%%==============================================================================
%% Interface exports
%%==============================================================================

-ifdef(DEBUG).
-export([start_dispatch/0]).
start_dispatch() ->
    {ok, Child} = start({ssl, <<"imap.gmail.com">>, 993}),
    ok = cmd(Child, {login, {plain, <<"dispatchonme@gmail.com">>, <<"jives48_cars">>}}),
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


-spec cmd(pid(), cmd()) ->
    ok.
cmd(IMAP, Cmd) ->
    gen_server:cast(IMAP, {cmd, self(), Cmd}).

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
handle_cast({cmd, _, _} = Cmd, #state{cmds=Cmds} = State) ->
    {noreply, step(State#state{cmds=queue:in(Cmd, Cmds)})};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


%% @doc handle messages
handle_info({ssl, Socket, Data}, #state{socket=Socket, buffer=Buffer} = State) ->
    ?LOG_WARNING(handle_info, "unexpected data: ~p", [Data]),
    ?LOG_DEBUG("data: ~p", [parse(Data)]),
    %% TODO
    {noreply, step(State#state{})};
handle_info({ssl_closed, Socket}, #state{socket=Socket} = State) ->
    ?LOG_DEBUG("Socket Closed: ~p", [self()]),
    {stop, normal, State};
handle_info(Info, State) ->
    ?LOG_WARNING(handle_info, "unexpected: ~p", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, #state{socket=Socket, connspec={SocketType, _, _, _}}) ->
    SocketType:close(Socket);
terminate(Reason, _State) ->
    ?LOG_DEBUG("~p", [Reason]).


%%==============================================================================
%% Internal functions
%%==============================================================================

%% @doc step the state by flushing the buffer, then attempting to run the next cmd
-spec step(#state{}) ->
    #state{}.
step(#state{active=none, cmds=Cmds, socket=Socket, tag=Tag} = State) ->
    case queue:out(Cmds) of
        {empty, Cmds} ->
            State;
        {{value, {cmd, _, InternalCmd} = Cmd}, Cmds2} ->
            CTag = <<$C, (integer_to_binary(Tag))/binary>>,
            ?LOG_DEBUG("SENDING: ~p", [cmd_to_data(InternalCmd)]),
            ok = ssl:send(Socket, [CTag, " " | cmd_to_data(InternalCmd)]),
            State#state{cmds=Cmds2, tag=Tag+1, active={CTag, Cmd}}
    end;
step(State) ->
    State.


flush()


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

-spec parse(binary() | #state{}) ->
    {binary(), parse_state(), [parse_result()], [[parse_result()]]} |
        {[parse_result()] | none, #state{}}.
parse(Data) when is_binary(Data) ->
    parse(Data, none, []);
parse(#state{buffer=Buffer, parse_state=ParseState, token_acc=TokenAcc} = State) ->
    {Result, {Buffer2, ParseState2, TokenAcc2}} = parse(Buffer, ParseState, TokenAcc),
    {Result, State#state{buffer=Buffer2, parse_state=ParseState2, token_acc=TokenAcc2}}.

-spec parse(binary(), parse_state(), [parse_result()]) ->
    {[parse_result()] | none, {binary(), parse_state(), [parse_result()]}}.
parse(<<>>, ParseState, ParseAcc) ->
    {none, {<<>>, ParseState, ParseAcc}};

parse(<<$\r, $\n, Rest/binary>>, _, TokenAcc) ->
    {lists:reverse(TokenAcc), {Rest, none, []}};

parse(<<" ", Rest/binary>>, none, TokenAcc) ->
    parse(Rest, none, TokenAcc);

%% NIL
parse(<<"NIL", Rest/binary>>, none, TokenAcc) ->
    parse(Rest, none, [nil | TokenAcc]);

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
parse(<<D, Rest/binary>>, none, TokenAcc) when D >= 30, D < 40 ->
    parse(Rest, {number, <<>>}, TokenAcc);

%% Atom
parse(<<" ", Rest/binary>>, {atom, AtomAcc}, TokenAcc) ->
    parse(Rest, none, [{atom, AtomAcc} | TokenAcc]);
parse(<<C, Rest/binary>>, none, TokenAcc) when C >= 42, C < 123 ->
    parse(Rest, {atom, <<C>>}, TokenAcc);
parse(<<C, Rest/binary>>, {atom, AtomAcc}, TokenAcc) when C >= 42, C < 123 ->
    parse(Rest, {atom, <<AtomAcc/binary, C>>}, TokenAcc);

%% List
parse(<<$(, Rest/binary>>, none, TokenAcc) ->
    {List, {InnerRest, none, []}} = parse(Rest, none, []),
    parse(InnerRest, none, [List | TokenAcc]);
parse(<<$), Rest/binary>>, _, TokenAcc) ->
    {lists:reverse(TokenAcc), {Rest, none, []}}.


%%==============================================================================
%% Tests
%%==============================================================================

-define(TEST, true).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    [parse_atoms(),
     parse_nils(),
     parse_strings(),
     parse_lists()].


parse_atoms() ->
    [?_assertEqual({none, {<<>>, none, [{atom, <<"atom">>}]}},
                   parse(<<"atom ">>))].

parse_nils() ->
    [?_assertEqual({none, {<<>>, none, [nil]}}, parse(<<"NIL">>)),
     ?_assertEqual({none, {<<>>, none, [nil]}}, parse(<<"NIL ">>))].

parse_strings() ->
    [?_assertEqual({none, {<<>>, none, [{string, <<"string">>}]}},
                   parse(<<"\"string\"">>))].

parse_lists() ->
    [?_assertEqual({none, {<<>>, none, [[{string, <<"a">>}, {string, <<"b">>}]]}},
                   parse(<<"(\"a\" \"b\")">>)),
     ?_assertEqual({[[{string, <<"a">>}, {string, <<"b">>},
                      [{string, <<"c">>}, {string, <<"d">>}]],
                     {string, <<"e">>}, {string, <<"f">>}],
                    {<<>>, none, []}},
                   parse(<<"(\"a\" \"b\" (\"c\" \"d\")) \"e\" \"f\"\r\n">>))].

-endif.
