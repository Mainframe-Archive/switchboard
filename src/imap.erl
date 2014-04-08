%% @doc An imap connection
-module(imap).
-behaviour(gen_server).

-export([parse/1]).
%% Interface exports
-export([start/1, start/2,
         start_link/1, start_link/2,
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

-type cmd() :: {login, auth()}.
-type internal_cmd() :: {cmd, pid() | none, cmd()}.

%% Records
-record(state, {socket :: socket(),
                connspec :: connspec(),
                auth = none :: none | auth(),
                tag = 0 :: integer(),
                buffer = <<>> :: binary(),
                cmds = queue:new() :: queue(),
                active = none :: none | {Tag :: binary(), internal_cmd()}}).
-export_type([connspec/0,
              auth_plain/0,
              auth_xoauth2/0,
              auth/0]).

%%==============================================================================
%% Interface exports
%%==============================================================================

-ifdef(DEBUG).
-export([start_test/0]).
start_test() ->
    start({ssl, <<"imap.gmail.com">>, 993},
          {xoauth2, <<"dispatchonme@gmail.com">>,
           <<"1/kif0yuTDHWu7UKtTCNtgDWTeoj_IYZM-SPmyxNiDCjc">>}).
-endif.

%% @equiv start(ConnSpec, none)
-spec start(connspec()) ->
    {ok, pid()} | _.
start(ConnSpec) ->
    start(ConnSpec, none).


%% @doc start a standalone IMAP connection
-spec start(connspec(), auth() | none) ->
    {ok, pid()} | _.
start(ConnSpec, Auth) ->
    gen_server:start(?MODULE, {ConnSpec, Auth}, []).


%% @equiv start(ConnSpec, none)
-spec start_link(connspec()) ->
    {ok, pid()} | _.
start_link(ConnSpec) ->
    start_link(ConnSpec, none).

%% @doc start an IMAP connection as part of the supervision tree
-spec start_link(connspec(), auth() | none) ->
    {ok, pid()} | _.
start_link(ConnSpec, Auth) ->
    gen_server:start_link(?MODULE, {ConnSpec, Auth}, []).


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
-spec init({connspec()} | {connspec(), auth() | none}) ->
    {ok, #state{}} | {stop, _}.
init({ConnSpec}) ->
    init({ConnSpec, none});
init({{SocketType, Host, Port}, Auth}) ->
    init({{SocketType, Host, Port, []}, Auth});
init({{SocketType, Host, Port, Options}, Auth}) ->
    init({{SocketType, Host, Port, Options, 5000}, Auth});
init({{SocketType, Host, Port, Options, Timeout} = ConnSpec, Auth}) ->
    OptionDefaults = [binary],
    case SocketType:connect(binary_to_list(Host), Port, Options ++ OptionDefaults, Timeout) of
        {ok, Socket} ->
            State = #state{socket=Socket, connspec=ConnSpec},
            case Auth of
                none ->
                    {ok, State};
                _ ->
                    {ok, State#state{auth=Auth,
                                     cmds=queue:in({cmd, none, Auth}, State#state.cmds)}}
            end;
        {error, ssl_not_started} ->
            start_app(ssl);
        {error, Reason} ->
            {stop, Reason}
    end.

%% @doc handle synchronous calls
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle asynchronous casts
handle_cast({cmd, _, _} = Cmd, #state{cmds=Cmds} = State) ->
    {noreply, step(State#state{cmds=queue:in(Cmd, Cmds)})};
%handle_cast({login, {plain, Username, Password}}, State) ->

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


%% @doc handle messages
handle_info({ssl, Socket, Data}, #state{socket=Socket} = State) ->
    ?LOG_WARNING(handle_info, "unexpected data: ~p", [Data]),
    ?LOG_DEBUG("data: ~p", [parse(Data)]),
    %% TODO
    {noreply, step(State)};
handle_info({ssl_closed, Socket}, #state{socket=Socket} = State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    ?LOG_WARNING(handle_info, "unexpected: ~p", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, #state{socket=Socket, connspec={SocketType, _, _, _}}) ->
    SocketType:close(Socket).


%%==============================================================================
%% Internal functions
%%==============================================================================

%% @equiv step(State, Cmd) if there is a command the queue
-spec step(#state{}) ->
    #state{}.
step(#state{active=none, cmds=Cmds} = State) ->
    case queue:out(Cmds) of
        {empty, Cmds} ->
            State;
        {{value, Cmd}, Cmds2} ->
            send_cmd(State#state{cmds=Cmds2}, Cmd)
    end.


%% @doc tag and send the cmd
-spec send_cmd(#state{}, internal_cmd()) ->
    #state{}.
send_cmd(#state{} = State, {cmd, _, {login, {plain, Username, Password}}} = Cmd) ->
    send_cmd(State, Cmd, intersperse(" ", ["LOGIN", Username, Password, "\r\n"])).

-spec send_cmd(#state{}, internal_cmd(), iodata()) ->
    #state{}.
send_cmd(#state{socket=Socket, tag=Tag} = State, Cmd, Data) ->
    CTag = <<$C, (integer_to_binary(Tag))/binary>>,
    ?LOG_DEBUG("SENDING: ~p", [Data]),
    ok = ssl:send(Socket, [CTag, " " | Data]),
    State#state{tag=Tag+1, active={CTag, Cmd}}.


%% @doc intersperses the separator between list elements
-spec intersperse(Sep :: iodata(), Xs :: iodata()) ->
    iodata().
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

-type parse_result() :: {atom, binary()}         % Atom    [A-Za-z0-9+]+
                        | integer()              % Number  [0-9]+
                        | {string, binary()}     % String  "*" | {n}*
                        | [parse_result()]       % List    (parse_result())
                        | nil.                   % NIL     NIL

-type parse_state() :: none
                       | {string, []}
                       | {atom, []}
                       | {number, []}.


-spec parse(binary()) ->
    {binary(), parse_state(), [parse_result()]}.
parse(Data) ->
    parse(Data, none, [], []).

-spec parse(binary(), parse_state(), [parse_result()], [[parse_result()]]) ->
    {binary(), parse_state(), [parse_result()]}.
parse(<<>>, ParseState, ParseAcc, LineAcc) ->
    {<<>>, ParseState, ParseAcc, LineAcc};

parse(<<$\r, $\n, Rest/binary>>, _, ParseAcc, LineAcc) ->
    parse(Rest, none, [], [lists:reverse(ParseAcc) | LineAcc]);

parse(<<" ", Rest/binary>>, none, TokenAcc, LineAcc) ->
    parse(Rest, none, TokenAcc, LineAcc);

%% NIL
parse(<<"NIL", Rest/binary>>, none, TokenAcc, LineAcc) ->
    parse(Rest, none, [nil | TokenAcc], LineAcc);

%% String
parse(<<$", Rest/binary>>, none, TokenAcc, LineAcc) ->
    parse(Rest, {string, <<>>}, TokenAcc, LineAcc);
parse(<<$", Rest/binary>>, {string, StringAcc}, TokenAcc, LineAcc) ->
    parse(Rest, none, [{string, StringAcc} | TokenAcc], LineAcc);
parse(<<$\\, $", Rest/binary>>, {string, StringAcc}, TokenAcc, LineAcc) ->
    parse(Rest, {string, <<StringAcc/binary, $">>}, TokenAcc, LineAcc);
parse(<<C, Rest/binary>>, {string, StringAcc}, TokenAcc, LineAcc) when C /= $"->
    parse(Rest, {string, <<StringAcc/binary, C>>}, TokenAcc, LineAcc);

%% Number
parse(<<D, Rest/binary>>, none, TokenAcc, LineAcc) when D >= 30, D < 40 ->
    parse(Rest, {number, <<>>}, TokenAcc, LineAcc);

%% Atom
parse(<<" ", Rest/binary>>, {atom, AtomAcc}, TokenAcc, LineAcc) ->
    parse(Rest, none, [{atom, AtomAcc} | TokenAcc], LineAcc);
parse(<<C, Rest/binary>>, none, TokenAcc, LineAcc) when C >= 42, C < 123 ->
    parse(Rest, {atom, <<C>>}, TokenAcc, LineAcc);
parse(<<C, Rest/binary>>, {atom, AtomAcc}, TokenAcc, LineAcc) when C >= 42, C < 123 ->
    parse(Rest, {atom, <<AtomAcc/binary, C>>}, TokenAcc, LineAcc);

%% List
parse(<<$(, Rest/binary>>, none, TokenAcc, LineAcc) ->
    {InnerRest, none, List, []} = parse(Rest, none, [], []),
    parse(InnerRest, none, [List | TokenAcc], LineAcc);
parse(<<$), Rest/binary>>, _, TokenAcc, LineAcc) ->
    {Rest, none, lists:reverse(TokenAcc), LineAcc}.


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
    [?_assertEqual({<<>>, none, [{atom, <<"atom">>}], []},
                   parse(<<"atom ">>))].

parse_nils() ->
    [?_assertEqual({<<>>, none, [nil], []}, parse(<<"NIL">>)),
     ?_assertEqual({<<>>, none, [nil], []}, parse(<<"NIL ">>))].

parse_strings() ->
    [?_assertEqual({<<>>, none, [{string, <<"string">>}], []}, parse(<<"\"string\"">>))].

parse_lists() ->
    [?_assertEqual({<<>>, none, [[{string, <<"a">>}, {string, <<"b">>}]], []},
                   parse(<<"(\"a\" \"b\")">>)),
     ?_assertEqual({<<>>, none, [],
                    [[[{string, <<"a">>}, {string, <<"b">>},
                       [{string, <<"c">>}, {string, <<"d">>}]],
                      {string, <<"e">>}, {string, <<"f">>}]]},
                   parse(<<"(\"a\" \"b\" (\"c\" \"d\")) \"e\" \"f\"\r\n">>))].

-endif.
