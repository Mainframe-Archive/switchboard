%% @doc An imap connection
%% http://tools.ietf.org/html/rfc3501
%% In the API, binaries are used for string-like data.

%% To allow the imap connection to be properly setup before usage, there
%% are some lifecycle hooks:
%% a cmds opt specifies a list of commands which are executed in order
%% in a separately spawned thread
%% the post_init_callback opt is a function that is run once all cmds have been completed

-module(imap).
-include("switchboard.hrl").

-behaviour(gen_server).

%% Interface exports
-export([start/1, start/2,
         start_link/1, start_link/2,
         stop/1,
         cast/2, cast/3,
         call/2, call/3, call/4,
         recv/0, recv/1,
         clean/1,
         auth_to_username/1]).


%% Callback exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% Preprocessor
-ifdef(TEST).
%% When testing, export additional functions
-export([clean_addresses/1,
         clean_body/1,
         cmd_to_list/1,
         seqset_to_list/1,
         pop_token/1,
         decode_line/1,
         tokenize/1,
         parse/1]).
-endif.

-define(CALL_TIMEOUT, 5000).


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
%% account() should be your good old <<"address@email.com">>"
-type account() :: binary().
%% connspec() specifies a tcp connection, see gen_tcp:connect args
-type connspec() :: {ssl | plain, Host :: binary(), Port :: integer()} |
                    {ssl | plain, Host :: binary(),
                     Port :: integer(), Options :: list()} |
                    {ssl | plain, Host :: binary(),
                     Port :: integer(), Options :: list(), Timeout :: integer()}.

%% auth() specifies an authorization type and arguments
%% XXX - Avoid operating with user credentials in plaintext
-type auth_plain() :: {plain, Username :: binary(), Password :: binary()}.
-type auth_xoauth2() :: {xoauth2, Account :: binary(), Token :: binary()}.
-type auth() :: auth_plain() | auth_xoauth2().

%% opt() specifies an option that the imap process can be started with
%% init_callback will be called in the init gen_server callback function (gproc reg)
-type opt() :: {init_callback, fun(() -> ok)}.

%% response() is passed into the applicable commands dispatch fun
-type response() :: {'*' | '+' | 'OK' | 'NO' | 'BAD', imap_term()}.
-type address() :: {address, [proplist:property()]}.

%% cmd() specifies a valid command that can be issued via call or cast
-type cmd() :: {login, auth()}
             | list
             | {examine, mailbox()}
             | {select, mailbox()}
             | {fetch, seqset()} | {fetch, seqset(), [binary()]}
             | {uid, {fetch, seqset()}} | {uid, {fetch, seqset(), [binary()]}}
             | noop
             | idle.

%% cmd_opt() specifies an optional setting for a command
%% The dispatch key refers to a dispatch function that is called with
%% the command's responses as they arrive
-type cmd_opt() :: {dispatch, fun((response()) -> ok)}.

%% internal_cmd() is used to wrap a cmd with options inside the gen_server
-type internal_cmd() :: {cmd, cmd(), [cmd_opt()]}.

%% Mailboxes
-type mailbox() :: binary().

%% Sequence Sets
-type seqset() :: '*' | integer() | {integer() | none, integer() | none}.

%% imap_term() a single post-parse imap token
-type imap_term() :: binary()               % Atom
                   | integer()              % Number
                   | {string, binary()}     % String
                   | [imap_term()]          % List
                   | nil                    % NIL
                   | '[' | ']'.             % [ | ]

%% token() is a single post-tokenize imap token
-type token() :: imap_term()
               | crlf                       % CRLF / \r\n
               | '(' | ')'.                 % ( | )

%% tokenize_state() private -- the possible tokenize states
-type tokenize_state() :: none
                        | {quoted, binary()}
                        | {literal, binary()}
                        | {number, binary()}
                        | {atom, binary()}.

-export_type([account/0,
              connspec/0,
              auth_plain/0,
              auth_xoauth2/0,
              auth/0,
              mailbox/0,
              seqset/0]).


%% imap gen_server state
-record(state, {socket :: socket(),
                connspec :: connspec(),
                opts = [] :: [proplist:property()],
                tag = 0 :: integer(),
                tokenize_state = {<<>>, none} :: {binary(), tokenize_state()},
                parse_state = {[], []} :: {[token()], [imap_term()]},
                cmds = gb_trees:empty() :: gb_trees:tree()}).


%%==============================================================================
%% Interface exports
%%==============================================================================

%% @equiv start(ConnSpec, [])
-spec start(connspec()) ->
    {ok, pid()} | _.
start(ConnSpec) ->
    start(ConnSpec, []).

%% @doc start a standalone IMAP connection
-spec start(connspec(), [opt()]) ->
    {ok, pid()} | _.
start(ConnSpec, Opts) ->
    gen_server:start(?MODULE, {ConnSpec, Opts}, []).


%% @equiv start_link(ConnSpec, [])
-spec start_link(connspec()) ->
    {ok, pid()} | _.
start_link(ConnSpec) ->
    start_link(ConnSpec, []).

%% @doc start an IMAP connection as part of the supervision tree
-spec start_link(connspec(), [opt()]) ->
    {ok, pid()} | _.
start_link(ConnSpec, Opts) ->
    gen_server:start_link(?MODULE, {ConnSpec, Opts}, []).


%% @doc stop the server
-spec stop(pid()) ->
    ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).


%% @equiv cast(Server, Cmd, [{dispatch, fun}])
-spec cast(pid(), cmd()) ->
    ok.
cast(Server, Cmd) ->
    cast(Server, Cmd, [{dispatch, dispatch_to_ref(self())}]).

%% @doc asynchronously cast the cmd, return without waiting for a response
-spec cast(pid(), cmd(), [cmd_opt()]) ->
    ok.
cast(Server, Cmd, Opts) ->
    gen_server:cast(Server, {cmd, Cmd, Opts}).


%% @equiv call(Server, Cmd, [{dispatch, fun}])
-spec call(pid(), cmd()) ->
    {ok, _} | {'+', _} | {error, _}.
call(Server, Cmd) ->
    call(Server, Cmd, [{dispatch, dispatch_to_ref(self())}]).

%% @equiv call(Server, Cmd, Opts, ?CALL_TIMEOUT)
-spec call(pid(), cmd(), [cmd_opt()]) ->
    {ok, _} | {'+', _} | {error, _}.
call(Server, Cmd, Opts) ->
    call(Server, Cmd, Opts, ?CALL_TIMEOUT).

%% @doc call the command, waiting until timeout for all responses
-spec call(pid(), cmd(), [cmd_opt()], integer()) ->
    {ok, _} | {'+', _} | {error, _}.
call(Server, Cmd, Opts, Timeout) ->
    gen_server:cast(Server, {cmd, Cmd, Opts}),
    Ref = monitor(process, Server),
    Responses = recv(Timeout, Ref),
    true = demonitor(Ref),
    Responses.


%% @equiv recv(?CALL_TIMEOUT)
-spec recv() ->
    {ok, _} | {'+', _} | {error, _}.
recv() ->
    recv(?CALL_TIMEOUT).

%% @equiv recv(Timeout, none)
-spec recv(integer()) ->
    {ok, _} | {'+', _} | {error, _}.
recv(Timeout) ->
    recv(Timeout, none).

%% @doc receive response until completion message, optional monitor used be call
-spec recv(integer(), reference() | none) ->
    {ok, _} | {'+', _} | {error, _}.
recv(Timeout, MonitorRef) ->
    recv(Timeout, MonitorRef, []).

%% @private helper for recieving responses
-spec recv(integer(), reference() | none, _) ->
    {ok, _} | {'+', _} | {error, _}.
recv(Timeout, MonitorRef, Responses) ->
    receive
        {'+', Response} ->
            {'+', lists:reverse([{'+', Response} | Responses])};
        {'*', Response} ->
            recv(Timeout, MonitorRef, [{'*', Response} | Responses]);
        {'OK', Response} ->
            {ok, lists:reverse([{'OK', Response} | Responses])};
        {'NO', Response} ->
            {error, lists:reverse([{'NO', Response} | Responses])};
        {'BAD', Response} ->
            {error, lists:reverse([{'BAD', Response} | Responses])};
        {'DOWN', MonitorRef, _, _, Reason} ->
            {error, {down, Reason, lists:reverse(Responses)}}
        after
            Timeout ->
              {error, timeout}
        end.


%% @private Clean a response to be JSON serializable via jsx.
clean({'*', [_, <<"FETCH">>, _]} = Fetch) ->
    clean_fetch(Fetch);
clean({'*', [<<"FLAGS">>, Flags]}) ->
    {flags, Flags};
clean({'*', [Exists, <<"EXISTS">>]}) ->
    {exists, Exists};
clean({'*', [Recent, <<"RECENT">>]}) ->
    {recent, Recent};
clean({'OK',[<<"Success">>]}) ->
    {ok, success}.


%% @private returns the username for the given auth
-spec auth_to_username(auth()) ->
    binary().
auth_to_username({plain, Username, _}) ->
    Username;
auth_to_username({xoauth2, Account}) ->
    Account.


%%==============================================================================
%% Callback exports
%%==============================================================================

%% @doc init callback
-spec init({connspec(), [opt()]}) ->
    {ok, #state{}} | {stop, _}.
init({{SocketType, Host, Port}, Opts}) ->
    init({{SocketType, Host, Port, []}, Opts});
init({{SocketType, Host, Port, SocketOpts}, Opts}) ->
    init({{SocketType, Host, Port, SocketOpts, 5000}, Opts});
init({{SocketType, Host, Port, SocketOpts, Timeout} = ConnSpec, Opts}) ->
    %% This lil bit of overengineering is so I can use gproc to reg the process
    case proplists:get_value(cmds, Opts) of
        undefined ->
            cmds_complete(self());
        Cmds ->
            cmds_call(self(), Cmds)
    end,
    InitCallback = case proplists:get_value(init_callback, Opts) of
        undefined ->
            fun(X) -> X end;
        Callback ->
            Callback
    end,
    SocketOptDefaults = [binary],
    case SocketType:connect(binary_to_list(Host), Port, SocketOpts ++ SocketOptDefaults,
                            Timeout) of
        {ok, Socket} ->
            State = InitCallback(#state{connspec=ConnSpec, opts=Opts, socket=Socket}),
            case proplists:get_value(post_init_callback, Opts) of
                undefined ->
                    {ok, State};
                _ ->
                    {ok, State, 0}
            end;
        {error, ssl_not_started} ->
            %% If ssl app isn't started, attempt to restart and then retry init
            start_app(ssl),
            init({ConnSpec, Opts});
        {error, Reason} ->
            {stop, Reason}
    end.

%% @doc handle synchronous calls
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle asynchronous casts
handle_cast({cmd, Cmd, _} = IntCmd,
            #state{cmds=Cmds, socket=Socket, tag=Tag} = State) ->
    % ?LOG_DEBUG("IMAP Being issued cmd: ~p", [Cmd]),
    CTag = <<$C, (integer_to_binary(Tag))/binary>>,
    ok = ssl:send(Socket, [CTag, " " | cmd_to_data(Cmd)]),
    {noreply, State#state{cmds=gb_trees:insert(CTag, IntCmd, Cmds), tag=Tag+1}};
%% Called once all inital commands have been completed
handle_cast({lifecycle, {cmds, complete}}, #state{opts=Opts} = State) ->
    % ?LOG_DEBUG("cmds complete", []),
    case proplists:get_value(post_init_callback, Opts) of
        undefined ->
            {noreply, State};
        Fun ->
            {noreply, Fun(State)}
    end;
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


%% @doc handle messages
handle_info({ssl, Socket, Data},
            #state{socket=Socket, tokenize_state={Buffer, AccState}} = State) ->
    ?LOG_DEBUG("Received: ~p", [Data]),
    Buffer2 = <<Buffer/binary, Data/binary>>,
    {noreply, churn_buffer(State#state{tokenize_state={Buffer2, AccState}})};
handle_info({ssl_closed, Socket}, #state{socket=Socket} = State) ->
    % ?LOG_WARNING(handle_info, "Socket Closed: ~p", [self()]),
    {stop, normal, State};
handle_info(Info, State) ->
    ?LOG_WARNING(handle_info, "unexpected: ~p", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% TODO -- on termination send {error, _} msgs to all open cmds
terminate(Reason, #state{socket=Socket, connspec={SocketType, _, _, _}}) ->
    ?LOG_WARNING(terminate, "TERMINATING ~p", [Reason]),
    SocketType:close(Socket);
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    ?LOG_WARNING(terminate, "IMAP Terminating due to ~p", [Reason]).


%%==============================================================================
%% Internal functions
%%==============================================================================

%% @doc this function casts a lifecycle cmds completion msg to the Imap server
-spec cmds_complete(pid()) ->
    ok.
cmds_complete(Imap) ->
    gen_server:cast(Imap, {lifecycle, {cmds, complete}}).


%% @doc call the list of commands in a separate process. used in startup
-spec cmds_call(pid(), [cmd()]) ->
    pid().
cmds_call(Imap, Cmds) ->
    spawn_link(fun() ->
                       lists:foreach(
                         fun({cmd, {call, Cmd}}) ->
                                 {ok, _} = imap:call(Imap, Cmd);
                            ({cmd, {cast, Cmd}}) ->
                                 {ok, _} = imap:cast(Imap, Cmd);
                            ({cmd, {call, Cmd}, Opts}) ->
                                 {ok, _} = imap:call(Imap, Cmd, Opts);
                            ({cmd, {cast, Cmd}, Opts}) ->
                                 ok = imap:cast(Imap, Cmd, Opts)
                         end, Cmds),
                       cmds_complete(Imap)
               end).


%% @doc returns a dispatch function for sending messages to the provided pid
-spec dispatch_to_ref(pid() | port() | atom()) ->
    fun((response()) -> ok).
dispatch_to_ref(Ref) ->
    fun(M) ->
            Ref ! M,
            ok
    end.


%% @doc churn the buffer by parsing responses and sending them to the proper processes
-spec churn_buffer(#state{}) ->
    #state{}.
churn_buffer(#state{tokenize_state=TokenizeState, parse_state=ParseState} = State) ->
    {Result, TokenizeState2, ParseState2} = decode_line(TokenizeState, ParseState),
    churn_buffer(State#state{tokenize_state=TokenizeState2, parse_state=ParseState2},
                 Result).


%% @private internal churn_buffer helper
-spec churn_buffer(#state{}, [imap_term()] | none) ->
    #state{}.
churn_buffer(State, none) ->
    State;
churn_buffer(#state{cmds=Cmds} = State, [<<"*">> | Response]) ->
    % ?LOG_DEBUG("UNTAGGED: ~p", [Response]),
    ok = lists:foreach(
           fun(Cmd) ->
                   dispatch(Cmd, {'*', Response})
           end,
           lists:filter(fun(C) -> cmds_response(C, Response) end,
                        gb_trees:values(Cmds))),
    churn_buffer(State);
churn_buffer(#state{cmds=Cmds} = State, [<<"+">> | Response]) ->
    % ?LOG_DEBUG("+: ~p", [Response]),
    ok = lists:foreach(
           fun(Cmd) ->
                   dispatch(Cmd, {'+', Response})
           end, gb_trees:values(Cmds)),
    churn_buffer(State);
churn_buffer(#state{cmds=Cmds} = State, [Tag | Response]) ->
    % ?LOG_DEBUG("Tag: ~p, Rest: ~p", [Tag, Response]),
    churn_buffer(case gb_trees:lookup(Tag, Cmds) of
                     {value, Cmd} ->
                         ok = dispatch(Cmd, case Response of
                                           [<<"OK">> | Rest]  -> {'OK', Rest};
                                           [<<"NO">> | Rest]  -> {'NO', Rest};
                                           [<<"BAD">> | Rest] -> {'BAD', Rest}
                                       end),
                         State#state{cmds=gb_trees:delete(Tag, Cmds)};
                     none ->
                         ?LOG_WARNING(churn_buffer, "Unknown Cmd Tag: ~p", [Tag]),
                         State
                 end).


%% @doc helper for dispatching a msg using the cmds dispatch fun[s]
-spec dispatch(internal_cmd(), _) ->
    ok.
dispatch({cmd, _, Opts} = IntCmd, Msg) ->
    dispatch(IntCmd, Msg, proplists:get_all_values(dispatch, Opts)).

-spec dispatch(internal_cmd(), A, [fun((A) -> ok)]) ->
    ok when A :: _.
dispatch(_IntCmd, _Msg, []) ->
    ok;
dispatch(IntCmd, Msg, [Fun | Rest]) ->
    ok = Fun(Msg),
    dispatch(IntCmd, Msg, Rest).


%% @doc returns true if the response is associated with the given cmd
-spec cmds_response(_, _) ->
    boolean().
cmds_response(_Cmd, _Response) ->
    true.

%% @doc return a command as iodata() ready to be sent to the IMAP server
-spec cmd_to_data(cmd()) ->
    iodata().
cmd_to_data(InternalCmd) ->
    %% XXX -- ineffecient end append. Luckily the lists are simple
    intersperse(" ", cmd_to_list(InternalCmd)) ++ ["\r\n"].


%% @doc return the list of command tokens
-spec cmd_to_list(cmd()) ->
    iodata().
cmd_to_list({login, {plain, Username, Password}}) ->
    [<<"LOGIN">>, Username, Password];
cmd_to_list(list) ->
    [<<"LIST">>];
cmd_to_list({select, Mailbox}) ->
    [<<"SELECT">>, Mailbox];
cmd_to_list({examine, Mailbox}) ->
    [<<"EXAMINE">>, Mailbox];

cmd_to_list({uid, {fetch, SeqSet}}) ->
    [<<"UID">> | cmd_to_list({fetch, SeqSet})];
cmd_to_list({uid, {fetch, SeqSet, Items}}) ->
    [<<"UID">> | cmd_to_list({fetch, SeqSet, Items})];
cmd_to_list({fetch, SeqSet}) ->
    cmd_to_list({fetch, SeqSet, <<"full">>});
cmd_to_list({fetch, SeqSet, Data}) ->
    [<<"FETCH">>, seqset_to_list(SeqSet), list_to_imap_list(Data)];

cmd_to_list(noop) ->
    [<<"NOOP">>];
cmd_to_list(idle) ->
    [<<"IDLE">>].


%% @doc returns the list as imap command tokens
%% I don't much like how I'm building commands...
-spec list_to_imap_list(binary() | [binary()]) ->
    binary() | [binary()].
list_to_imap_list(List) when is_list(List) ->
    [<<"(">> | intersperse(" ", List)] ++ [<<")">>];
list_to_imap_list(Term) ->
    Term.


%% @doc returns the list of command tokens associated with the sequence set
-spec seqset_to_list(seqset()) ->
    iodata().
seqset_to_list({none, Stop}) ->
    <<$:, (integer_to_binary(Stop))/binary>>;
seqset_to_list({Start, none}) ->
    <<(integer_to_binary(Start))/binary, $:>>;
seqset_to_list({Start, Stop}) ->
    <<(integer_to_binary(Start))/binary, $:, (integer_to_binary(Stop))/binary>>;
seqset_to_list('*') ->
    <<"*">>;
seqset_to_list(Item) ->
    integer_to_binary(Item).


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


clean_fetch({'*', [_Id, <<"FETCH">>, Params]}) ->
    clean_fetch(Params, []).

clean_fetch([], Acc) ->
    {fetch, lists:reverse(Acc)};
clean_fetch([<<"UID">>, Uid | Rest], Acc) ->
    clean_fetch(Rest, [{uid, Uid} | Acc]);
clean_fetch([<<"FLAGS">>, Flags | Rest], Acc) ->
    clean_fetch(Rest, [{flags, Flags} | Acc]);
clean_fetch([<<"INTERNALDATE">>, {string, InternalDate} | Rest], Acc) ->
    clean_fetch(Rest, [{internaldate, InternalDate} | Acc]);
clean_fetch([<<"RFC822.SIZE">>, Rfc822Size | Rest], Acc) ->
    clean_fetch(Rest, [{rfc822size, Rfc822Size} | Acc]);
clean_fetch([<<"ENVELOPE">>,
             [{string, Date}, {string, Subject},
              From, Sender, ReplyTo, To, Cc, Bcc, InReplyTo,
              {string, MessageId}] | Rest], Acc) ->
    %% @todo parse the date
    Envelope = [{date, Date},
                {subject, Subject},
                {from, clean_addresses(From)},
                {sender, clean_addresses(Sender)},
                {replyto, clean_addresses(ReplyTo)},
                {to, clean_addresses(To)},
                {cc, clean_addresses(Cc)},
                {bcc, clean_addresses(Bcc)},
                {inreplyto, clean_addresses(InReplyTo)},
                {messageid, MessageId}],
    clean_fetch(Rest, [{envelope, Envelope} | Acc]);
clean_fetch([<<"BODY">>, Body | Rest], Acc) ->
    clean_fetch(Rest, [{body, clean_body(Body)} | Acc]).


%% @doc Clean the provided body.
%% @todo this typing is worthless -- actually sit down and define the types
%% @todo currently doesn't support multipart params
-spec clean_body(_) ->
    [_].
clean_body(Body) ->
    clean_body(Body, []).

-spec clean_body(_, [_]) ->
    [_].
clean_body([{string, Type}, {string, SubType}, Params, Id,
            Description, {string, Encoding}, Size, _What], []) ->
    [{type, Type},
     {subtype, SubType},
     {params, clean_imap_props(Params)},
     {id, Id},
     {description, Description},
     {encoding, Encoding},
     {size, Size}];
clean_body([{string, MultiPartType}], Acc) ->
    [{multipart, MultiPartType}, {parts, lists:reverse(Acc)}];

clean_body([Head | Rest], Acc) ->
    clean_body(Rest, [clean_body(Head) | Acc]).


%% @doc Clean an imap props list, e.g. [{string, <<"KEY">>}, {string, <<"VALUE">>}, ...].
-spec clean_imap_props([{string, binary()}]) ->
    [proplists:property()].
clean_imap_props(Props) ->
    clean_imap_props(Props, []).

-spec clean_imap_props([{string, binary()}], [proplists:property()]) ->
    [proplists:property()].
clean_imap_props([], Acc) ->
    lists:reverse(Acc);
clean_imap_props([{string, Key}, {string, Value} | Rest], Acc) ->
    clean_imap_props(Rest, [{Key, Value} | Acc]).


%% @doc clean the provided address
-spec clean_addresses([[{string, binary()}]]) ->
    [address()].
clean_addresses(nil) ->
    [];
clean_addresses(Addresses) ->
    clean_addresses(Addresses, []).

%% @private
-spec clean_addresses([[{string, binary()}]], [address()]) ->
    [address()].
clean_addresses([], Acc) ->
    lists:reverse(Acc);
clean_addresses([[RawName, _, {string, MailBox}, {string, Host}] | Rest], Acc) ->
    Address = [{email, <<MailBox/binary, $@, Host/binary>>}],
    clean_addresses(Rest,
                    [{address, case RawName of
                                   nil -> Address;
                                   {string, Name} -> [{name, Name} | Address]
                               end} |
                     Acc]).

%%==============================================================================
%% Response Tokenizing + Parsing
%%==============================================================================

-type pop_token_ret() :: {token() | none, binary(), tokenize_state()}.


-spec decode_line(binary()) ->
    {[imap_term()] | none,  binary(), tokenize_state(), [imap_term()]}.
decode_line(Data) ->
    decode_line({Data, none}, {[], []}).

-spec decode_line({binary(), tokenize_state()}, {[token()], [imap_term()]}) ->
    {[imap_term()] | none,  {binary(), tokenize_state()}, {[token()], [imap_term()]}}.
decode_line({Data, TokenizeState}, {TokenBuffer, ParseAcc}) ->
    case tokenize(Data, TokenizeState) of
        {none, Data2, TokenizeState2} ->
            {none, {Data2, TokenizeState2}, {TokenBuffer, ParseAcc}};
        {Tokens, Data2, TokenizeState2} ->
            {Line, TokenBuffer2, ParseAcc2} = parse(TokenBuffer ++ Tokens, ParseAcc),
            {Line, {Data2, TokenizeState2}, {TokenBuffer2, ParseAcc2}}
    end.


%% @doc tokenize the imap data
-spec tokenize(binary()) ->
    {[token()], binary(), tokenize_state()}.
tokenize(Data) ->
    tokenize(Data, [], pop_token(Data)).

-spec tokenize(binary(), tokenize_state()) ->
    {[token()], binary(), tokenize_state()}.
tokenize(Data, TokenizeState) ->
    tokenize(Data, [], pop_token(Data, TokenizeState)).

-spec tokenize(binary(), [token()], pop_token_ret()) ->
    {[token()], binary(), tokenize_state()}.
tokenize(_Data, Tokens, {none, Rest, TokenizeState}) ->
    {lists:reverse(Tokens), Rest, TokenizeState};
tokenize(Data, Tokens, {Token, Rest, TokenizeState}) ->
    tokenize(Data, [Token | Tokens], pop_token(Rest, TokenizeState)).


%% @equiv pop_token(Data, none)
-spec pop_token(binary()) ->
    pop_token_ret().
pop_token(Data) ->
    pop_token(Data, none).

%% @doc pop the next token from the imap data, returning the state
-spec pop_token(binary(), tokenize_state()) ->
    {token() | none, binary(), tokenize_state()}.
pop_token(<<>>, State) ->
    {none, <<>>, State};

%% Consume hanging spaces
pop_token(<<" ", Rest/binary>>, none) ->
    pop_token(Rest, none);

%% \r\n
pop_token(<<$\r, $\n, Rest/binary>>, none) ->
    {crlf, Rest, none};

%% NIL
pop_token(<<"NIL", Rest/binary>>, none) ->
    {nil, Rest, none};

%% ( | ) | [ | ]
pop_token(<<$(, Rest/binary>>, none) ->
    {'(', Rest, none};
pop_token(<<$), Rest/binary>>, none) ->
    {')', Rest, none};

pop_token(<<$[, Rest/binary>>, none) ->
    {'[', Rest, none};
pop_token(<<$], Rest/binary>>, none) ->
    {']', Rest, none};

%% Numbers
pop_token(<<C, _/binary>> = Data, {number, NumberAcc}) when
  C =:= 32; C =:= 40; C =:= 41; C =:= 91; C =:= 93 ->
    {binary_to_integer(NumberAcc), Data, none};
pop_token(<<$\r, $\n, _/binary>> = Data, {number, NumberAcc}) ->
    {binary_to_integer(NumberAcc), Data, none};
pop_token(<<" ", Rest/binary>>, {number, NumberAcc}) ->
    {binary_to_integer(NumberAcc), Rest, none};
pop_token(<<D, Rest/binary>>, {number, NumberAcc}) when D >= 48, D < 58 ->
    pop_token(Rest, {number, <<NumberAcc/binary, D>>});
pop_token(<<D, Rest/binary>>, none) when D >= 48, D < 57 ->
    pop_token(Rest, {number, <<D>>});
pop_token(<<C, Rest/binary>>, {number, NumberAcc}) when C >= 35, C < 123 ->
    pop_token(Rest, {atom, <<NumberAcc/binary, C>>});

%% Atom
pop_token(<<C, _/binary>> = Data, {atom, AtomAcc}) when
  C =:= 32; C =:= 40; C =:= 41; C =:= 91; C =:= 93 ->
    {AtomAcc, Data, none};
pop_token(<<$\r, $\n, _/binary>> = Data, {atom, AtomAcc}) ->
    {AtomAcc, Data, none};
pop_token(<<C, Rest/binary>>, none) when C >= 35, C < 123 ->
    pop_token(Rest, {atom, <<C>>});
pop_token(<<C, Rest/binary>>, {atom, AtomAcc}) when C >= 35, C < 123 ->
    pop_token(Rest, {atom, <<AtomAcc/binary, C>>});

%% Quoted Strings
pop_token(<<$", Rest/binary>>, none) ->
    pop_token(Rest, {quoted, <<>>});
pop_token(<<$\\, C, Rest/binary>>, {quoted, Acc}) ->
    pop_token(Rest, {quoted, <<Acc/binary, C>>});
pop_token(<<$", Rest/binary>>, {quoted, Acc}) ->
    {{string, Acc}, Rest, none};
pop_token(<<$\r, $\n, _>>, {quoted, _}) ->
    throw({error, crlf_in_quoted});
pop_token(<<C, Rest/binary>>, {quoted, Acc}) ->
    pop_token(Rest, {quoted, <<Acc/binary, C>>});

pop_token(Binary, _) ->
    {none, Binary, none}.


%% @doc Parse the flat list of tokens into a data structurej
-spec parse([token()]) ->
    {[imap_term()] | none, [token()], [imap_term()]}.
parse(Tokens) ->
    parse(Tokens, []).

-spec parse([token()], [imap_term()]) ->
    {[imap_term()] | none, [token()], [imap_term()]}.
parse([], ParseAcc) ->
    {none, [], ParseAcc};

parse([crlf | Rest], ParseAcc) ->
    {lists:reverse(ParseAcc), Rest, []};

parse(['(' | Rest] = Tokens, ParseAcc) ->
    case parse(Rest) of
        {List, Rest2, []} ->
            parse(Rest2, [List | ParseAcc]);
        {none, _, _} ->
            {none, Tokens, ParseAcc}
    end;
parse([')' | Rest], ParseAcc) ->
    {lists:reverse(ParseAcc), Rest, []};

parse([Token | Rest], ParseAcc) ->
    parse(Rest, [Token | ParseAcc]).
