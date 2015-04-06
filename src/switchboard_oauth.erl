%% @auth Thomas Moulia <jtmoulia@pocketknife.io>
%% @doc Switchboard's OAuth functions. Switchboard only handles
%% exchanging refresh tokens for access tokens.

-module(switchboard_oauth).

-export([refresh_to_access_token/1, refresh_to_access_token/3]).


%%==============================================================================
%% Public Interface
%%==============================================================================

%% @doc Exchange the refresh token for an access token using the
%% application's `oauth_providers' configuration.
-spec refresh_to_access_token(imap:auth_xoauth2()) ->
    {ok, imap:auth_xoauth2()} | {error, _}.
refresh_to_access_token({xoauth2, _, {_, _}} = Auth) ->
    %% @todo wrap up the req
    case oauth_for(Auth) of
        {ok, Props} ->
            case get_values([refresh_url,
                             client_id,
                             client_secret], Props) of
                {ok, [RefreshUrl, ClientID, ClientSecret]} ->
                    refresh_to_access_token(Auth, RefreshUrl,
                                            {ClientID, ClientSecret});
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Exchange the refresh token for an access token using the provided
%% configuration.
-spec refresh_to_access_token(imap:auth_xoauth2(),
                              binary(),
                              {binary(), binary()}) ->
    {ok, imap:auth_xoauth2()} | {error, _}.
refresh_to_access_token({xoauth2, Account, {RefreshToken, _Provider}},
                        RefreshUrl,
                        {ClientID, ClientSecret})
  when is_binary(ClientID) andalso is_binary(ClientSecret) ->
    Body = http_uri:encode(to_query_string([{client_id, ClientID},
                       {client_secret, ClientSecret},
                       {refresh_token, RefreshToken},
                       {grant_type, <<"refresh_token">>}])),
    ContentType = "application/x-www-form-urlencoded",
    case httpc:request(post, {binary_to_list(RefreshUrl), [], ContentType, Body}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            Props = jsx:decode(Body),
            AccessToken = proplists:get_value(<<"access_token">>, Props),
            {ok, {xoauth2, Account, AccessToken}}
    end.


%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private
%% @doc Turn a tuple into a query string
-spec to_query_string(tuple) -> {ok, binary()} | {error, _}.
to_query_string(Props) ->
  << <<(atom_to_binary(K, utf8))/binary, "=", V/binary, "&">> || {K, V} <- Props >>.

%% @private
%% @doc Split a domain into an iolist
-spec split_domain(binary) -> {ok, {iolist(), binary()}} | {error, bad_domain}.
split_domain(Domain) when is_binary(Domain) ->
    case binary:split(Domain, <<".">>, [global]) of
        [_] ->
            {error, bad_domain};
        Components when is_list(Components) ->
            [TLD | DomainName] = lists:reverse(Components),
            {ok, {lists:reverse(DomainName), TLD}}
    end.


%% @private
%% @doc Get the domain of the provided account.
-spec split_account(binary()) ->
    {ok, iolist()} | {error, bad_account | bad_domain}.
split_account(Account) ->
    case binary:split(Account, <<"@">>) of
        [Username, Domain] ->
            case split_domain(Domain) of
                {ok, {DomainName, TLD}} ->
                    {ok, {Username, {DomainName, TLD}}};
                _ ->
                    {error, bad_domain}
            end;
         _ ->
            {error, bad_account}
    end.


%% @private
%% @doc Get the provider associated witht the domain name
-spec domain_to_provider(iolist()) -> {ok, atom()} | {error, undefined}.
domain_to_provider([<<"gmail">>]) -> {ok, google};
domain_to_provider(_) -> {error, undefined}.


%% @private
%% @doc Get oauth settings for the provided account.
-spec oauth_for(imap:auth_xoauth2()) ->
    {ok, {ClientID :: string, ClientSecret :: string}} | {error, _}.
oauth_for({xoauth2, Account, {_, Provider}}) ->
    case application:get_env(switchboard, oauth_providers) of
        undefined ->
            {error, no_oauth};
        {ok, Providers} ->
            case oauth_for(provider, Provider, Providers) of
              {ok, OAuthProvider} ->
                {ok, OAuthProvider};
              {error, _} ->
                oauth_for(email, Account, Providers)
            end
    end.

%% @private
-spec oauth_for(atom(), binary(), [proplists:property()]) ->
    {ok, {ClientID :: string, ClientSecret :: string}} | {error, _}.
oauth_for(provider, Provider, Providers) ->
    case proplists:get_value(Provider, Providers) of
      undefined ->
        {error, {no_provider_oauth, Provider}}; 
      OAuthProvider ->
        {ok, OAuthProvider}
    end;
oauth_for(email, Account, Providers) ->
    case split_account(Account) of
        {ok, {_Username, {DomainName, _TLD}}} ->
            case domain_to_provider(DomainName) of
                {ok, Provider} ->
                    case proplists:get_value(Provider, Providers) of
                        undefined ->
                            {error, {no_domain_oauth, DomainName}};
                        OAuthProvider ->
                            {ok, OAuthProvider}
                    end;
                {error, undefined} ->
                    {error, {no_domain_oauth, DomainName}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc Get all of the values for the keys provided from the proplist.
-spec get_values([Key], [proplists:property()]) ->
    {ok, [term]} | {error, {undefined, Key}} when Key :: term.
get_values(Keys, List) ->
    get_values(Keys, List, []).

get_values([], _List, Acc) ->
    {ok, lists:reverse(Acc)};
get_values([Key | Keys], List, Acc) ->
    case proplists:get_value(Key, List) of
        undefined ->
            {error, {undefined, Key}};
        Value ->
            get_values(Keys, List, [Value | Acc])
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_values_test() ->
    [?assertEqual(get_values(Keys, List), Expected)
     || {Keys, List, Expected}
            <- [{[a], [{a, 1}, {b, 2}], {ok, [1]}},
                {[b], [{a, 1}, {b, 2}], {ok, [2]}},
                {[a, b], [{a, 1}, {b, 2}], {ok, [1, 2]}},
                {[c], [{a, 1}, {b, 2}], {error, {undefined, c}}},
                {[a, b, c], [{a, 1}, {b, 2}], {error, {undefined, c}}}
               ]].

oauth_for_test() ->
    [?assertEqual(oauth_for(Account, Providers), Expected)
     || {Account, Providers, Expected}
            <- [
                {<<"account@gmail.com">>, [{google, prov}], {ok, prov}},
                {<<"account@gmail.com">>, [],
                 {error, {no_domain_oauth, [<<"gmail">>]}}},
                {<<"account@a.b.c.com">>, [],
                 {error, {no_domain_oauth, [<<"a">>, <<"b">>, <<"c">>]}}},
                {<<"account">>, [], {error, bad_account}}
               ]].

-endif().
