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

%% @private
%% @doc Utility Belt for Switchboard.

-module(switchboard_util).

-include("switchboard.hrl").

-export([take_first/2,
         get_values/2,
         await_death/1, await_death/2,
         reprop/2]).


%%==============================================================================
%% External API
%%==============================================================================

%% @doc Returns the first element that matches the predicate.
-spec take_first(fun((A) -> boolean()), [A]) ->
    A when A :: any().
take_first(_, []) ->
    undefined;
take_first(Fun, [Head | Rest]) ->
    case Fun(Head) of
        true ->
            Head;
        _ ->
            take_first(Fun, Rest)
    end.

%% @doc Returns the list of Values for the provided Keys from List, or undefined.
-spec get_values([_], [proplists:property()]) ->
    {[_], [_]}.
get_values(Keys, List) ->
    get_values(Keys, List, [], []).

get_values([], _List, Values, Undefineds) ->
    {lists:reverse(Values), lists:reverse(Undefineds)};
get_values([Key | Keys], List, Values, Undefineds) ->
    case proplists:get_value(Key, List) of
        undefined ->
            get_values(Keys, List, Values, [Key | Undefineds]);
        Value ->
            get_values(Keys, List, [Value | Values], Undefineds)
    end.


%% @equiv await_death(Pid, 5000)
-spec await_death(pid()) ->
    ok | {error, timeout}.
await_death(Pid) ->
    await_death(Pid, 5000).

%% @doc Await the death of the process.
-spec await_death(pid(), non_neg_integer()) ->
    ok | {error, timeout}.
await_death(Pid, Timeout) ->
    %% @todo race condition. Prop this up if being used outside of tests
    case is_process_alive(Pid) of
        true ->
            MonitorRef = monitor(process, Pid),
            receive
                {'DOWN', MonitorRef, process, Pid,  _} ->
                    ok
            after Timeout ->
                    true = demonitor(MonitorRef),
                    {error, timeout}
            end;
        false ->
            ok
    end.


%% @doc Alter a proplist's keys according to Mapping. If a key isn't present
%% in Mapping it will be dropped.
%%
%% XXX - Broken for Strings as keys!
reprop(PropList, Mapping) when is_list(Mapping) ->
    [reprop(PropList, M) || M <- Mapping];
reprop(PropList, {InProp, OutProp}) ->
    {OutProp, proplists:get_value(InProp, PropList)};
reprop(PropList, Prop) ->
    reprop(PropList, {Prop, Prop}).



%%==============================================================================
%% EUnit Tests
%%==============================================================================

-ifdef(TEST).

%% get_values

suite_test_() ->
    [take_first_asserts(),
     get_values_asserts(),
     await_death_asserts()].

take_first_asserts() ->
    [?_assertEqual(true, take_first(fun(A) -> A end, [false, true])),
     ?_assertEqual(undefined, take_first(fun(A) -> A end, [false, false])),
     ?_assertEqual(undefined, take_first(fun(A) -> A end, []))].

test_get_values({Keys, List}, Result) ->
    ?_assertEqual(Result, get_values(Keys, List)).

get_values_asserts() ->
    [test_get_values({[], [{a, 1}]}, {[], []}),
     test_get_values({[a], [{a, 1}]}, {[1], []}),
     test_get_values({[a, b], [{a, 1}, {b, 2}]}, {[1, 2], []}),
     test_get_values({[a], []}, {[], [a]})].

await_death_asserts() ->
    Watched = spawn(fun() -> timer:sleep(5000) end),
    Watcher = spawn(fun() -> await_death(Watched) end),
    MonitorRef = monitor(process, Watcher),
    exit(Watched, kill),
    [?_assertEqual(ok, receive {'DOWN', MonitorRef, process, Watcher, _} -> ok
                       after 2000 -> error end)].

-endif.
