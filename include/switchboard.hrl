%% -*- mode: emacs -*- 
-define(TEST, true).
-include_lib("eunit/include/eunit.hrl").

-define(DEBUG, true).

%% Testing Account
-define(DISPATCH, <<"dispatchonme@gmail.com">>).
-define(DISPATCH_MAILBOX, <<"INBOX">>).
-define(DISPATCH_CONN_SPEC, {ssl, <<"imap.gmail.com">>, 993}).
-define(DISPATCH_AUTH, {plain, ?DISPATCH, <<"jives48_cars">>}).
