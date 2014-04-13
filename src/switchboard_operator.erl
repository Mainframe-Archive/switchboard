%% @doc the switchboard operator takes low level imap responses, and translates them...
-module(switchboard_operator).
-behaviour(gen_server).

%% Interface exports
-export([start_link/1,
         stop/1,
         idle_key/1,
         dispatch_to_key/1]).

%% Callback exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% Records
-record(state, {currentuid = undefined :: undefined | integer()}).

%%==============================================================================
%% Interface exports
%%==============================================================================

start_link(ConnSpec) ->
    gen_server:start_link(?MODULE, ConnSpec, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).


%% Messaging
%% @doc returns the key used by idlers
-spec idle_key(ConnSpec) ->
    {p, l, {?MODULE, idle, ConnSpec}} when ConnSpec :: imap:connspec().
idle_key(ConnSpec) ->
    {p, l, {?MODULE, idle, ConnSpec}}.


-spec dispatch_to_key(_) ->
    fun((imap:response()) -> ok).
dispatch_to_key(Key) ->
    fun(M) ->
            gproc:send(Key, M),
            ok
    end.


%%==============================================================================
%% Callback exports
%%==============================================================================

init({ConnSpec, Auth}) ->
    true = gproc:reg(idle_key(ConnSpec)),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({idle, Mailbox, Msg}, State) ->
    lager:info("Mailbox: ~p, Msg: ~p", [Mailbox, Msg]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%==============================================================================
%% Internal functions
%%==============================================================================

