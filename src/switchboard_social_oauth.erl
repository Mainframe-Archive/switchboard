%% @auth Thomas Moulia <jtmoulia@pocketknife.io>
%% @doc Hook for handling gmail OAuth requests.

-module(switchboard_social_oauth).
-behavior(cowboy_social_hook).

-export([execute/3]).


-define(REFRESH_TOKEN_URL, <<"https://accounts.google.com/o/oauth2/token">>).
-define(GMAIL_HOST, <<"imap.gmail.com">>).
-define(GMAIL_PORT, 993).


%%==============================================================================
%% Cowboy Social Hook Callback
%%==============================================================================

%% @doc Start monitoring for the given account.
%% @todo Execute code in new process to not block page render
execute(TokenProps, Req, State) ->
    {ok, Account} = get_account(TokenProps),
    {ok, Credential} = make_credential(TokenProps, Account),
    {ok, _} = switchboard:add(Account, Credential),
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
%% @doc Make a credential.
-spec make_credential(dispatch_social_hook:token_props(), binary()) ->
    {ok, dispatch_db:credential()} | {error, _}.
make_credential(TokenProps, Account) ->
    UserID = uuid:uuid_to_string(uuid:get_v4()),
    {ok, Auth} = make_auth(TokenProps, Account),
    {ok, dispatch_db:credential(UserID, ?GMAIL_HOST, ?GMAIL_PORT,
                                imap:auth_to_props(Auth))}.


%% @private
%% @doc Make an Auth datastructure.
-spec make_auth(dispatch_social_hook:token_props(), binary()) ->
    {ok, imap:auth()} | {error, _}.
make_auth(TokenProps, Account) ->
    make_auth(Account,
              proplists:get_value(refresh_token, TokenProps),
              proplists:get_value(access_token, TokenProps)).

%% @private
%% @doc Helper for make_auth.
%% @see make_auth/2
-spec make_auth(binary(), binary() | undefined, binary() | undefined) ->
    {ok, imap:auth()} | {error, _}.
make_auth(Account, undefined, AccessToken) ->
    {ok, {xoauth2, Account, AccessToken}};
make_auth(Account, RefreshToken, _AccessToken) ->
    {ok, {xoauth2, Account, {RefreshToken, ?REFRESH_TOKEN_URL}}}.
