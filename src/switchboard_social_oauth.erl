%% @auth Thomas Moulia <jtmoulia@pocketknife.io>
%% @doc Hook for handling gmail OAuth requests.

-module(switchboard_social_oauth).
-behavior(cowboy_social_hook).

-export([execute/3]).

-type maybe_binary() :: binary() | undefined.


%%==============================================================================
%% Cowboy Social Hook Callback
%%==============================================================================

%% @doc Start monitoring for the given account.
%% @todo Execute code in new process to not block page render
execute(TokenProps, Req, State) ->
    Options = cowboy_social:get_options(State),
    {ok, Account} = get_account(TokenProps),
    {ok, ConnSpec} = make_connspec(Options),
    {ok, Auth} = make_auth(TokenProps, Options, Account),
    {ok, _} = switchboard:add(ConnSpec, Auth, [<<"INBOX">>]),
    {ok, TokenProps, Req, State}.


%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private
%% @doc Returns the user's account, retrieved from the userinfo endpoint
%% using the access token in TokenProps.
-spec get_account(dispatch_social:token_props()) ->
    {ok, binary()}.
get_account(TokenProps) ->
    AccessToken = proplists:get_value(access_token, TokenProps),
    {ok, UserProfile} = cowboy_social_google:user_profile(AccessToken, []),
    case proplists:get_value(email, UserProfile) of
        undefined ->
            {error, no_account};
        Account ->
            {ok, Account}
    end.


%% @private
%% @doc make a connspec from the provided options
-spec make_connspec([proplists:prop()]) ->
    {ok, imap:connspec()} | {error, _}.
make_connspec(Options) ->
    make_connspec(proplists:get_value(imap_host, Options),
                  proplists:get_value(imap_port, Options)).

-spec make_connspec(maybe_binary(), integer() | undefined) ->
    {ok, imap:connspec()} | {error, _}.
make_connspec(undefined, _Port) ->
    {error, {missing, imap_host}};
make_connspec(_Host, undefined) ->
    {error, {missing, imap_port}};
make_connspec(Host, Port) ->
    {ok, {ssl, Host, Port}}.


%% @private
%% @doc Make an Auth datastructure.
-spec make_auth([proplists:prop()], [proplists:prop()], binary()) ->
    {ok, imap:auth()} | {error, _}.
make_auth(TokenProps, Options, Account) ->
    make_auth(Account,
              proplists:get_value(refresh_token, TokenProps),
              proplists:get_value(token_uri, Options),
              proplists:get_value(access_token, TokenProps)).

%% @private
%% @doc Helper for make_auth.
%% @see make_auth/2
-spec make_auth(binary(), maybe_binary(), maybe_binary(), maybe_binary()) ->
    {ok, imap:auth()} | {error, _}.
make_auth(Account, undefined, _, AccessToken) when AccessToken =/= undefined ->
    {ok, {xoauth2, Account, AccessToken}};
make_auth(Account, RefreshToken, RefreshURI, _) ->
    {ok, {xoauth2, Account, {RefreshToken, RefreshURI}}}.
