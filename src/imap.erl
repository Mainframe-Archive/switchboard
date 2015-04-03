%%------------------------------------------------------------------------------
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% 1. Redistributions of source code must retain the above copyright notice, this
%% list of conditions and the following disclaimer.
%%
%% 2. Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%%
%% 3. Neither the name of the copyright holder nor the names of its contributors
%% may be used to endorse or promote products derived from this software without
%% specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
%% THE POSSIBILITY OF SUCH DAMAGE.
%% @end
%%
%% @author Thomas Moulia <jtmoulia@pocketknife.io>
%% @copyright Copyright (c) 2014, ThusFresh, Inc.
%% @end
%%------------------------------------------------------------------------------

%% @doc An RFC 3501 IMAP Client.
%%
%% Binaries are used for string-like data.
%%
%% To allow the imap connection to be properly setup before usage,
%% there are some lifecycle hooks: a cmds opt specifies a list of
%% commands which are executed in order in a separately spawned thread
%%
%% the post_init_callback opt is a function that is run once all cmds
%% have been completed.
%%
%% @reference See <a href="http://tools.ietf.org/html/rfc3501">RFC 3501</a>.

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
         clean_list/1,
         get_parts_by_type/2, get_parts_by_type/3,
         auth_to_account/1,
         auth_to_props/1,
         start_app/1]).


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
%% account() should be your good old "address@email.com"
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
-type auth_xoauth2() :: {xoauth2, Account :: binary(),
                         AccessToken :: binary() |
                         {RefreshToken :: binary(), RefreshUrl :: binary()}}.
-type auth() :: auth_plain() | auth_xoauth2().

%% opt() specifies an option that the imap process can be started with -- see start_link
-type opt() :: {init_callback, fun((term) -> ok)}
    | {post_init_callback, fun((term) -> ok)}
    | {cmds, [{cmd, cmd()}]}.

%% response() is passed into the applicable commands dispatch fun
-type response() :: {'*' | '+' | 'OK' | 'NO' | 'BAD', imap_term()}.
-type address() :: {address, [proplist:property()]}.

%% cmd() specifies a valid command that can be issued via call or cast
-type cmd() :: {login, auth()}
    | list |  {list, binary(), binary()}
    | {status, mailbox()} | {status, mailbox(), [binary()]}
    | {examine, mailbox()}
    | {select, mailbox()}
    | {delete, mailbox()}
    | {search, iodata()}
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
-type seqset() :: '*' | integer() | [integer()]
    | {integer() | none, integer() | none}.

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

%% @doc Start a standalone IMAP connection.
%% @see start_link/2

-spec start(connspec(), [opt()]) ->
    {ok, pid()} | _.
start(ConnSpec, Opts) ->
    gen_server:start(?MODULE, {ConnSpec, Opts}, []).


%% @equiv start_link(ConnSpec, [])
-spec start_link(connspec()) ->
    {ok, pid()} | _.
start_link(ConnSpec) ->
    start_link(ConnSpec, []).

%% @doc Start an IMAP connection as part of the supervision tree.
%%
%% Options:
%% <dl>
%%   <dt>`{cmd, [{cmd, Cmd}]}'</dt>
%%     <dd>A list of valid commands, tagged with `cmd' which will be called against the
%%         IMAP client from a separate process on start. Use this
%%         to bring the IMAP client to the correct initial state.
%%
%%         For example, if starting an IMAP client to run the IDLE command, the
%%         `cmd' list may contain
%%         `[{cmd, {login, ...}}, {cmd, {select, ...}}, {cmd, idle}]'.
%%     </dd>
%%
%%   <dt>`{init_callback, fun((State) -> ok)}'</dt>
%%     <dd>This function will be called during the IMAP client's `init'. It is useful
%%         for more complex registration, e.g. using `gproc'.
%%     </dd>
%%
%%   <dt>`{post_init_callback, fun((State) -> ok)}'</dt>
%%     <dd>This function will be called after `init', and after all commands
%%         specified by the `cmds' option have successfully completed.
%%     </dd>
%% </dl>
-spec start_link(connspec(), [opt()]) ->
    {ok, pid()} | _.
start_link(ConnSpec, Opts) ->
    gen_server:start_link(?MODULE, {ConnSpec, Opts}, []).


%% @doc Stop the IMAP client.
-spec stop(pid()) ->
    ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).


%% @equiv cast(Server, Cmd, [{dispatch, dispatch_to_ref(self())}])
-spec cast(pid(), cmd()) ->
    ok.
cast(Server, Cmd) ->
    cast(Server, Cmd, [{dispatch, dispatch_to_ref(self())}]).

%% @doc Asynchronously cast the cmd.
-spec cast(pid(), cmd(), [cmd_opt()]) ->
    ok.
cast(Server, Cmd, Opts) ->
    gen_server:cast(Server, {cmd, Cmd, Opts}).


%% @equiv call(Server, Cmd, [{dispatch, dispatch_to_ref(self())}])
-spec call(pid(), cmd()) ->
    {ok, _} | {'+', _} | {error, _}.
call(Server, Cmd) ->
    call(Server, Cmd, [{dispatch, dispatch_to_ref(self())}]).

%% @equiv call(Server, Cmd, Opts, 5000)
-spec call(pid(), cmd(), [cmd_opt()]) ->
    {ok, _} | {'+', _} | {error, _}.
call(Server, Cmd, Opts) ->
    call(Server, Cmd, Opts, ?CALL_TIMEOUT).

%% @doc Call the command, waiting until timeout for all responses.
-spec call(pid(), cmd(), [cmd_opt()], integer()) ->
    {ok, _} | {'+', _} | {error, _}.
call(Server, Cmd, Opts, Timeout) ->
    case is_process_alive(Server) of
        true ->
            gen_server:cast(Server, {cmd, Cmd, Opts}),
            Ref = monitor(process, Server),
            Responses = recv(Timeout, Ref),
            true = demonitor(Ref),
            Responses;
        false ->
            {error, noprocess}
    end.


%% @equiv recv(5000)
-spec recv() ->
    {ok, _} | {'+', _} | {error, _}.
recv() ->
    recv(?CALL_TIMEOUT).

%% @doc Receive responses until receiving an IMAP completion message.
%%
%% Used by call.

-spec recv(integer()) ->
    {ok, _} | {'+', _} | {error, _}.
recv(Timeout) ->
    recv(Timeout, none).

%% @private
-spec recv(integer(), reference() | none) ->
    {ok, _} | {'+', _} | {error, _}.
recv(Timeout, MonitorRef) ->
    recv(Timeout, MonitorRef, []).

%% @private Helper for receiving responses.
-spec recv(integer(), reference() | none, _) ->
    {ok, Responses} | {'+', _} | {error, Responses} when Responses :: {_, [_]}.
recv(Timeout, MonitorRef, Responses) ->
    receive
        {'+', Response} ->
            {'+', lists:reverse([{'+', Response} | Responses])};
        {'*', Response} ->
            recv(Timeout, MonitorRef, [{'*', Response} | Responses]);
        {'OK', Response} ->
            {ok, {{'OK', Response}, lists:reverse(Responses)}};
        {'NO', Response} ->
            {error, {{'NO', Response}, lists:reverse(Responses)}};
        {'BAD', Response} ->
            {error, {{'BAD', Response}, lists:reverse(Responses)}};
        {'DOWN', MonitorRef, process, _, Reason} ->
            {error, {{down, Reason}, lists:reverse(Responses)}}
        after
            Timeout ->
              {error, timeout}
        end.


%% @doc Clean a response to be JSON serializable via jsx. In other words,
%% proplists! For convenience, this function is overloaded to also accept
%% a list of responses, in which case it will map itself across the list.
%%
%% NB: Not implemented for all responses -- see function matches.
-spec clean(_) ->
    {atom(), _}.
clean(Resps) when is_list(Resps) ->
    [clean(Resp) || Resp <- Resps];
clean({'*', [_, <<"FETCH">>, _]} = Fetch) ->
    clean_props(Fetch);
clean({'*', [<<"SEARCH">> | SeqIds]}) ->
    {search, SeqIds};
clean({'*', [<<"FLAGS">> | Flags]}) ->
    {flags, Flags};
clean({'*', [Exists, <<"EXISTS">>]}) ->
    {exists, Exists};
clean({'*', [Recent, <<"RECENT">>]}) ->
    {recent, Recent};
clean({'*', [<<"OK">>, '[', <<"PERMANENTFLAGS">>, PermanentFlags, ']' | _]}) ->
    {permanent_flags, PermanentFlags};
clean({'*', [<<"OK">>, '[', <<"UIDVALIDITY">>, UIDValidity, ']' | _]}) ->
    {uidvalidity, UIDValidity};
clean({'*', [<<"OK">>, '[', <<"UIDNEXT">>, UIDNext, ']' | _]}) ->
    {uidnext, UIDNext};
clean({'*', [<<"OK">>, '[', <<"HIGHESTMODSEQ">>, HighestModSeq, ']' | _]}) ->
    {highest_mod_seq, HighestModSeq};
clean({'*', [<<"LIST">>, NameAttrs, {string, Delim}, {string, Name}]}) ->
    {list, [{name_attrs, NameAttrs},
            {delimiter, Delim},
            {name, Name}]}.


%% @doc Returns the fetch parts by type.
get_parts_by_type({fetch, Props}, Type) ->
    case proplists:get_value(body, Props) of
        undefined ->
            {error, no_body};
        Body ->
            case proplists:get_value(parts, Body) of
                undefined ->
                    {error, no_parts};
                Parts ->
                    {ok,
                     lists:filter(
                       fun(Part) ->
                               proplists:get_value(type, Part) =:= Type
                       end, Parts)}
            end
    end.

get_parts_by_type(Fetch, Type, SubType) ->
    case get_parts_by_type(Fetch, Type) of
        {error, Reason} ->
            {error, Reason};
        {ok, Parts} ->
            {ok,
             lists:filter(
               fun(Part) -> proplists:get_value(subtype, Part) =:= SubType end, Parts)}
    end.


%% @doc Clean list command responses, returning an easier to deal with list.
%% NB: this will discard non-`list' responses.
-spec clean_list({ok, {_, [_]}}) ->
    {error, _}.
clean_list({error, Reason}) ->
    {error, Reason};
clean_list({ok, {_, Resps}}) ->
    %% XXX - this forces all responses to be `{list, _}' -- too stringent?
    {ok, lists:foldl(
          fun(Resp, Acc) ->
                 case clean(Resp) of
                     {list, Rest} ->
                         [Rest | Acc];
                     _ ->
                         Acc
                 end
          end, [], Resps)}.


%% @doc Returns the username for the given authorization. This is used
%% to simplify process registration.
-spec auth_to_account(auth()) ->
    account().
auth_to_account({plain, Account, _}) ->
    Account;
auth_to_account({xoauth2, Account, _}) ->
    Account.


%% @doc Returns the auth as a jsx:encodable proplist.
-spec auth_to_props(auth()) ->
    [proplists:property()].
auth_to_props({plain, Username, Password}) ->
    [{<<"type">>, <<"plain">>},
     {<<"username">>, Username},
     {<<"password">>, Password}];
auth_to_props({xoauth2, Username, Token}) ->
    [{<<"type">>, <<"xoauth2">>},
     {<<"username">>, Username},
     {<<"token">>, case Token of
                       AccessToken when is_binary(Token) ->
                           [{<<"type">>, <<"access">>},
                            {<<"token">>, AccessToken}];
                       {RefreshToken, RefreshUrl} ->
                           [{<<"type">>, <<"refresh">>},
                            {<<"token">>, RefreshToken},
                            {<<"url">>, RefreshUrl}]
                   end
     }].


%%==============================================================================
%% Callback exports
%%==============================================================================

%% @private
-spec init({connspec(), [opt()]}) ->
    {ok, #state{}} | {stop, _}.
init({{SocketType, Host, Port}, Opts}) ->
    init({{SocketType, Host, Port, []}, Opts});
init({{SocketType, Host, Port, SocketOpts}, Opts}) ->
    init({{SocketType, Host, Port, SocketOpts, 5000}, Opts});
init({{SocketType, Host, Port, SocketOpts, Timeout} = ConnSpec, Opts}) ->
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
            {ok, InitCallback(#state{connspec=ConnSpec, opts=Opts, socket=Socket})};
        {error, Reason} ->
            {stop, Reason}
    end.


%% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% @private
handle_cast({cmd, {'+', ContCmd}, _}, #state{socket=Socket} = State) ->
    %% Handle continuation commands. Supplies no concurrency guarantees.
    ok = ssl:send(Socket, cmd_to_data(ContCmd)),
    {noreply, State};
handle_cast({cmd, Cmd, _} = IntCmd,
            #state{cmds=Cmds, socket=Socket, tag=Tag} = State) ->
    %% ?LOG_DEBUG("IMAP Being issued cmd: ~p", [Cmd]),
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


%% @private
handle_info({ssl, Socket, Data},
            #state{socket=Socket, tokenize_state={Buffer, AccState}} = State) ->
    %% lager:info("Received: ~p", [Data]),
    Buffer2 = <<Buffer/binary, Data/binary>>,
    {noreply, churn_buffer(State#state{tokenize_state={Buffer2, AccState}})};
handle_info({ssl_closed, Socket}, #state{socket=Socket} = State) ->
    % ?LOG_WARNING(handle_info, "Socket Closed: ~p", [self()]),
    {stop, normal, State}.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
%% @todo -- on termination send {error, _} msgs to all open cmds
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

%% @private
%% @doc Casts a lifecycle cmds completion msg to the IMAP server.
-spec cmds_complete(pid()) ->
    ok.
cmds_complete(Imap) ->
    gen_server:cast(Imap, {lifecycle, {cmds, complete}}).


%% @private
%% @doc Call the list of commands in a separate process. Used in startup.
%% Success responses will be discarded, and errors will take down the imap
%% client
-spec cmds_call(pid(), [cmd()]) ->
    pid().
cmds_call(Imap, Cmds) ->
    spawn_link(fun() ->
                       lists:foreach(
                         fun({cmd, {call, Cmd}}) ->
                                 case imap:call(Imap, Cmd) of
                                     {ok, _}  -> ok;
                                     {'+', _} -> ok
                                 end;
                            ({cmd, {cast, Cmd}}) ->
                                 ok = imap:cast(Imap, Cmd);
                            ({cmd, {call, Cmd}, Opts}) ->
                                 case imap:call(Imap, Cmd, Opts) of
                                     {ok, _}  -> ok;
                                     {'+', _} -> ok
                                 end;
                            ({cmd, {cast, Cmd}, Opts}) ->
                                 ok = imap:cast(Imap, Cmd, Opts)
                         end, Cmds),
                       cmds_complete(Imap)
               end).


%% @private
%% @doc Returns a dispatch function for sending messages to the provided pid.
-spec dispatch_to_ref(pid() | port() | atom()) ->
    fun((response()) -> ok).
dispatch_to_ref(Ref) ->
    fun(M) ->
            Ref ! M,
            ok
    end.


%% @private
%% @doc Churn the buffer by parsing and dispatching responses.
-spec churn_buffer(#state{}) ->
    #state{}.
churn_buffer(#state{tokenize_state=TokenizeState, parse_state=ParseState} = State) ->
    {Result, TokenizeState2, ParseState2} = decode_line(TokenizeState, ParseState),
    %% lager:info("Result: ~p", [Result]),
    churn_buffer(State#state{tokenize_state=TokenizeState2, parse_state=ParseState2},
                 Result).


%% @private
%% @doc Internal churn_buffer helper.
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


%% @private
%% @doc Helper for dispatching a msg using the cmds dispatch fun[s].
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


%% @private
%% @doc Returns true if the response is associated with the given command.
-spec cmds_response(_, _) ->
    boolean().
cmds_response(_Cmd, _Response) ->
    true.

%% @private
%% @doc Return a command as iodata() ready to be sent to the IMAP serverl
-spec cmd_to_data(cmd()) ->
    iodata().
cmd_to_data(InternalCmd) ->
    %% XXX -- ineffecient end append. Luckily the lists are simple
    intersperse(" ", cmd_to_list(InternalCmd)) ++ ["\r\n"].


%% @private
%% @doc Return the list of command tokens.
-spec cmd_to_list(cmd()) ->
    iodata().
cmd_to_list({login, {plain, Username, Password}}) ->
    [<<"LOGIN">>, Username, Password];
cmd_to_list({login, {xoauth2, _, {_, _}} = RefreshAuth}) ->
    %% @todo wrap up the req
    case switchboard_oauth:refresh_to_access_token(RefreshAuth) of
        {ok, AccessAuth} ->
            cmd_to_list({login, AccessAuth})
    end;
cmd_to_list({login, {xoauth2, Account, AccessToken}}) ->
    Encoded = base64:encode(<<"user=", Account/binary,
                             "auth=Bearer ", AccessToken/binary,
                             "">>),
    [<<"AUTHENTICATE">>, <<"XOAUTH2">>, Encoded];
cmd_to_list({uid, Cmd}) ->
    [<<"UID">> | cmd_to_list(Cmd)];
cmd_to_list(list) ->
    [<<"LIST">>, <<"\"\"">>, <<"%">>];
cmd_to_list({list, Reference, Match}) ->
    [<<"LIST">>, Reference, Match];
cmd_to_list({status, Mailbox}) ->
    [<<"STATUS">>, quote_wrap_binary(Mailbox)];
cmd_to_list({status, Mailbox, Items}) ->
    [<<"STATUS">>, quote_wrap_binary(Mailbox), Items];
cmd_to_list({search, Terms}) ->
    [<<"SEARCH">> | Terms];
cmd_to_list({rename, ExistingMailbox, NewMailbox}) ->
    [<<"RENAME">>, quote_wrap_binary(ExistingMailbox), quote_wrap_binary(NewMailbox)];
cmd_to_list({fetch, SeqSet}) ->
    cmd_to_list({fetch, SeqSet, <<"full">>});
cmd_to_list({fetch, SeqSet, Data}) ->
    [<<"FETCH">>, seqset_to_list(SeqSet), list_to_imap_list(Data)];
cmd_to_list({Cmd, Mailbox}) when Cmd =:= select; Cmd =:= examine; Cmd =:= delete;
                                 Cmd =:= subscribe, Cmd =:= unsubscribe ->
    [atom_to_cmd(Cmd), quote_wrap_binary(Mailbox)];
cmd_to_list(Cmd) when Cmd =:= noop; Cmd =:= idle; Cmd =:= done; Cmd =:= close;
                      Cmd =:= expunge ->
    [atom_to_cmd(Cmd)].


%% @private
%% @doc Returns the atom as a command, i.e. as an uppercase binary.
-spec atom_to_cmd(atom()) ->
    binary().
atom_to_cmd(Cmd) ->
    CmdString = string:to_upper(atom_to_list(Cmd)),
    list_to_binary(CmdString).


%% @private
%% @doc Wrap a binary with quotes.
-spec quote_wrap_binary(binary()) ->
    binary().
quote_wrap_binary(Bin) ->
    <<$", Bin/binary, $">>.


%% @private
%% @doc Returns the list as imap command tokens.
-spec list_to_imap_list(binary() | [binary()]) ->
    binary() | [binary()].
list_to_imap_list(List) when is_list(List) ->
    [<<"(">> | intersperse(" ", List)] ++ [<<")">>];
list_to_imap_list(Term) ->
    Term.


%% @private
%% @doc returns the list of command tokens associated with the sequence set.
%% See the type specification of seqset for it's various options.
-spec seqset_to_list(seqset()) ->
    iodata().
seqset_to_list([Head | Rest]) ->
    lists:foldl(
      fun(Id, Acc) ->
              <<Acc/binary, $,, (integer_to_binary(Id))/binary>>
     end, integer_to_binary(Head), Rest);
seqset_to_list({none, Stop}) ->
    <<$:, (integer_to_binary(Stop))/binary>>;
seqset_to_list({'*', Stop}) ->
    <<$*, $:, (integer_to_binary(Stop))/binary>>;
seqset_to_list({Start, none}) ->
    <<(integer_to_binary(Start))/binary, $:>>;
seqset_to_list({Start, '*'}) ->
    <<(integer_to_binary(Start))/binary, $:, $*>>;
seqset_to_list({Start, Stop}) ->
    <<(integer_to_binary(Start))/binary, $:, (integer_to_binary(Stop))/binary>>;
seqset_to_list('*') ->
    <<"*">>;
seqset_to_list(Item) ->
    integer_to_binary(Item).


%% @private
%% @doc Intersperses the separator between list elements.
-spec intersperse(Sep, [Sep | A]) ->
    [A] when Sep :: _, A :: _.
intersperse(_, []) ->
  [];
intersperse(_, [X]) ->
  [X];
intersperse(Sep, [X | Xs]) ->
  [X, Sep | intersperse(Sep, Xs)].


%% @private
%% @doc Start an app or list of apps.
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


%% @private
%% @doc Clean response props.
clean_props({'*', [_Id, <<"FETCH">>, Params]}) ->
    clean_props(Params, []).

clean_props([], Acc) ->
    {fetch, lists:reverse(Acc)};
clean_props([<<"UID">>, Uid | Rest], Acc) ->
    clean_props(Rest, [{uid, Uid} | Acc]);
clean_props([<<"FLAGS">>, Flags | Rest], Acc) ->
    clean_props(Rest, [{flags, Flags} | Acc]);
clean_props([<<"INTERNALDATE">>, {string, InternalDate} | Rest], Acc) ->
    clean_props(Rest, [{internaldate, InternalDate} | Acc]);
clean_props([<<"RFC822.SIZE">>, Rfc822Size | Rest], Acc) ->
    clean_props(Rest, [{rfc822size, Rfc822Size} | Acc]);
clean_props([<<"ENVELOPE">>,
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
    clean_props(Rest, [{envelope, Envelope} | Acc]);
clean_props([<<"BODY">>, '[', ']', {string, Body} | Rest], Acc) ->
    clean_props(Rest, [{body, Body} | Acc]);
clean_props([<<"BODY">>, '[', <<"TEXT">>, ']', {string, Body} | Rest], Acc) ->
    clean_props(Rest, [{textbody, Body} | Acc]);
clean_props([<<"BODY">>, Body | Rest], Acc) ->
    clean_props(Rest, [{body, clean_body(Body)} | Acc]).


%% @private
%% @doc Clean the provided body.
%% @todo this typing is worthless -- actually sit down and define the types
%% @todo currently doesn't support multipart params
%% @todo continue expansion to support mime types

-spec clean_body(_) ->
    [_].
clean_body(Body) ->
    clean_body(Body, []).
-spec clean_body(_, [_]) ->
    [_].
clean_body([{string, Type}, {string, SubType}, Params, Id,
            Description, {string, Encoding}, Size | _What], []) ->
    [{type, Type},
     {subtype, SubType},
     {params, clean_imap_props(Params)},
     {id, Id},
     {description, Description},
     {encoding, Encoding},
     {size, Size}];
%% @todo why multiple formats?
clean_body([{string, MultiPartType}], Acc) ->
    [{multipart, MultiPartType}, {parts, lists:reverse(Acc)}];
clean_body([Head | Rest], Acc) ->
    clean_body(Rest, [clean_body(Head) | Acc]).


%% @private
%% @doc Clean an imap props list, e.g.
%% `[{string, &lt;&lt;"KEY"&gt;&gt;}, {string, &lt;&lt;"VALUE"&gt;&gt;}, ...]'.
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


%% @private
%% @doc Clean the provided address.
-spec clean_addresses([[{string, binary()}]]) ->
    [address()].
clean_addresses(nil) ->
    [];
clean_addresses({string, <<"">>}) ->
    [];
clean_addresses({string, Address}) ->
    [address, {email, strip_address(Address)}];
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
                                   nil -> [{name, <<"">>} | Address];
                                   {string, Name} -> [{name, Name} | Address]
                               end} |
                     Acc]).

%% @private
%% If the address is wrapped by `<...>', strip the angle brackets
strip_address(Address) when is_binary(Address) ->
    case {binary:first(Address), binary:last(Address)} of
        {$<, $>} ->
            Length = byte_size(Address) - 2,
            <<$<, Stripped:Length/binary, $>>> = Address,
            Stripped;
        _ ->
            Address
    end.
%%==============================================================================
%% Response Tokenizing + Parsing
%%==============================================================================

-type pop_token_ret() :: {token() | none, binary(), tokenize_state()}.

%% @private
-spec decode_line(binary()) ->
    {[imap_term()] | none,  binary(), tokenize_state(), [imap_term()]}.
decode_line(Data) ->
    decode_line({Data, none}, {[], []}).

%% @private
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


%% @private
%% @doc Tokenize the imap data.
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


%% @private
%% @equiv pop_token(Data, none)
-spec pop_token(binary()) ->
    pop_token_ret().
pop_token(Data) ->
    pop_token(Data, none).

%% @private
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
pop_token(<<D, Rest/binary>>, none) when D >= 48, D < 58 ->
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

%% Literal Strings
pop_token(<<${, Rest/binary>>, none) ->
    pop_token(Rest, {literal, <<>>});
pop_token(<<$}, $\r, $\n, Rest/binary>>, {literal, ByteAcc}) ->
    pop_token(Rest, {literal, binary_to_integer(ByteAcc), <<>>});
pop_token(<<D, Rest/binary>>, {literal, ByteAcc}) when D >= 48, D < 58 ->
    pop_token(Rest, {literal, <<ByteAcc/binary, D>>});
pop_token(Binary, {literal, Bytes, LiteralAcc}) when is_integer(Bytes) ->
    case Binary of
        <<Literal:Bytes/binary, Rest/binary>> ->
            {{string, <<LiteralAcc/binary, Literal/binary>>}, Rest, none};
        _ ->
            %% If the binary is too short, accumulate it in the state
            pop_token(<<>>, {literal, Bytes - size(Binary),
                             <<LiteralAcc/binary, Binary/binary>>})
    end;

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


%% @private
%% @doc Parse the flat list of tokens into a data structurej
%% @end

%% todo
%% 21:46:48.402 [info] Received: <<"* 12 FETCH (UID 12)\r\n* 13 FETCH (U">>
%% 21:46:48.403 [info] Result: [<<"*">>,12,<<"FETCH">>,[<<"UID">>,12]]
%% 21:46:48.403 [info] Result: none
%% 21:46:48.403 [info] Received: <<"ID 13)\r\n* 14 FETCH (UID 14)\r\n">>
%% 21:46:48.403 [info] Result: [<<"*">>,13,<<"FETCH">>,none,<<"UID">>,13]
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
