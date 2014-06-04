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
%% @copyright Copyright (c) 2014, ThusFresh, Inc
%% @end
%%------------------------------------------------------------------------------

%% @doc rfc2822
%% @end

-module(rfc2822).
-include("switchboard.hrl").

-export([decode/1]).


-spec decode(Message :: binary()) ->
    {ok, _} | {error, _}.
decode(Message) ->
    decode_headers(Message).


-spec decode_headers(Message :: binary()) ->
    {ok, {Headers :: [{binary(), binary()}], Rest :: binary()}} | {error, _}.
decode_headers(Message) ->
    [Line, Rest] = binary:split(Message, <<"\r\n">>),
    decode_headers(Rest, Line, []).

decode_headers(<<$\r, $\n, Message/binary>>, Line, Headers) ->
    {ok, {lists:reverse([line_to_header(Line) | Headers]), Message}};
%% Continuation line, append to current line.
decode_headers(<<" ", $\t, Message/binary>>, Line, Headers) ->
    [NextLine, Rest] = binary:split(Message, <<$\r, $\n>>),
    decode_headers(Rest, <<Line/binary, NextLine/binary>>, Headers);
decode_headers(Message, Line, Headers) ->
    [NextLine, Rest] = binary:split(Message, <<$\r, $\n>>),
    decode_headers(Rest, NextLine, [line_to_header(Line) | Headers]).


line_to_header(Line) ->
    [Key, Value] = binary:split(Line, <<$:>>),
    {Key, case Value of
              <<" ", Rest/binary>> -> Rest;
              Value -> Value
          end}.


-ifdef(TEST).

-define(RFC2822_MSG1, <<"From: John Doe <jdoe@machine.example>\r\nTo: Mary Smith <mary@example.net>\r\nSubject: Saying Hello\r\nDate: Fri, 21 Nov 1997 09:55:06 -0600\r\nMessage-ID: <1234@local.machine.example>\r\n\r\nThis is a message just to say hello. So, \"Hello\"">>).

suite_test_() ->
    [decode_assertions()].

decode_assertions() ->
    [?_assertEqual({ok, {[{<<"From">>, <<"John Doe <jdoe@machine.example>">>},
                          {<<"To">>, <<"Mary Smith <mary@example.net>">>},
                          {<<"Subject">>, <<"Saying Hello">>},
                          {<<"Date">>, <<"Fri, 21 Nov 1997 09:55:06 -0600">>},
                          {<<"Message-ID">>, <<"<1234@local.machine.example>">>}],
                          <<"This is a message just to say hello. So, \"Hello\"">>}},
                  decode(?RFC2822_MSG1))].

-endif.
