% This file is part of tchains library.
%
%  Copyright (C) 2012-2013 Mikhail Sobolev <mss@mawhrin.net>
%  Copyright (C) 2012-2013 Naxuat Oy <contact@naxuat.com>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.

-module(tchains_basic).

-include("tchains_internal.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-else.
-export([
    ignore_error/3,
    choose/3,
    repeat_while/3,
    repeat_until/3,
    merge_all/3,
    merge_any/3,
    in_context/3
]).
-endif.

ignore_error([Tasks], Value, Engine) ->
    try
        tchains_engine:exec(Tasks, Value, Engine)
    catch
        {error, _} ->
            Value
    end.

choose([Selector, Whens | Rest], Value, Engine) ->
    choose(Selector, Whens, none_or_one(Rest, bad_otherwise), Value, Engine).

choose(Selector, Whens, Otherwise, Value, Engine) ->
    Key = tchains_engine:exec(Selector, Value, Engine),
    case {lists:keyfind(Key, 1, Whens), Otherwise} of
        {{_, When}, _} ->
            tchains_engine:exec(When, Value, Engine);

        {false, none} ->
            tchains_engine:error({unknown_selector_value, Key});

        {_, _} ->
            tchains_engine:exec(Otherwise, Value, Engine)
    end.

repeat_while([Condition, Body]=Args, Value, Engine) ->
    case tchains_engine:exec(Condition, Value, Engine) of
        true ->
            repeat_while(Args, tchains_engine:exec(Body, Value, Engine), Engine);

        _ ->
            Value
    end.

repeat_until([Body, Condition]=Args, Value, Engine) ->
    NewValue = tchains_engine:exec(Body, Value, Engine),
    case tchains_engine:exec(Condition, NewValue, Engine) of
        true ->
            NewValue;

        _ ->
            repeat_until(Args, NewValue, Engine)
    end.

merge_all([Tasks, Merger], Value, Engine) ->
    case split_and_join(Tasks, Value, Engine) of
        {[First | _], _} ->
            tchains_engine:fail(First);

        {[], Values} ->
            tchains_engine:exec(Merger, Values, Engine)
    end.

merge_any([Tasks, Merger], Value, Engine) ->
    case split_and_join(Tasks, Value, Engine) of
        {_, [_AtLeast | _]=Values} ->
            tchains_engine:exec(Merger, Values, Engine);

        {Errors, _} ->
            tchains_engine:stop({all_failed, Errors})
    end.

split_and_join(Tasks, Value, Engine) when is_list(Tasks) ->
    lists:splitwith(fun tchains_engine:is_bad/1,
                    do_split_and_join(Tasks, Value, Engine));
split_and_join(Task, Value, Engine) ->
    split_and_join([Task], Value, Engine).

do_split_and_join([], Value, _Engine) ->
    [Value];
do_split_and_join(Tasks, Value, Engine) ->
    TaskFun = fun(Task) -> tchains_engine:exec_catch([Task], Value, Engine) end,
    Self = self(),
    collect_results([spawn(fun() ->
                            Self ! {self(), TaskFun(Task)}
                           end) || Task <- Tasks]).

collect_results([Pid | Rest]) ->
    receive
        {Pid, Result} ->
            [Result | collect_results(Rest)]
    end;
collect_results([]) ->
    [].

in_context([Setup, Body | Rest], Value, Engine) ->
    TearDown = none_or_one(Rest, bad_teardown),
    Context = tchains_engine:exec(Setup, Value, Engine),
    Result = tchains_engine:exec_catch(Body, Context, Engine),
    if
        TearDown == none ->
            ok;
            
        true ->
            tchains_engine:exec_catch(TearDown, Context, Engine)
    end,
    tchains_engine:fail_if_bad(Result).

none_or_one([], _) ->
    none;
none_or_one([Result], _) ->
    Result;
none_or_one(_, Error) ->
    tchains_engine:stop(Error).

%% {{{ Eunit tests
-ifdef(TEST).
get_value([], Value, _Engine) ->
    Value.

put_value([NewValue], _, _Engine) ->
    NewValue.

bad([], {error, Error}, _Engine) ->
    tchains_engine:error(Error);
bad([], {stop, Reason}, _Engine) ->
    tchains_engine:stop(Reason).

error([Error], _, _Engine) ->
    tchains_engine:error(Error).

stop([Value], _, _Engine) ->
    tchains_engine:stop(Value).

ignore_error_test_() ->
    Chain1 = [
        {ignore_error, get_value}
    ],
    Chain2 = [
        {ignore_error, [
            {error, does_not_matter}
        ]}
    ],
    Handlers = [{handlers, [?MODULE]}],
    {"Tests for ignore_error", [
        ?_assertEqual(1, tchains:exec(Chain1, 1, Handlers)),
        ?_assertEqual(1, tchains:exec(Chain2, 1, Handlers))
    ]}.

choose_test_() ->
    Chain1 = [
        {choose, get_value, [
            {1, {put_value, 11}},
            {2, {put_value, 21}}
        ], {put_value, otherwise}}
    ],
    Chain2 = [
        {choose, get_value, [
            {1, {put_value, 11}},
            {2, {put_value, 21}}
        ]}
    ],
    Chain3 = [
        {choose, oopsy, [
            {1, {put_value, 11}},
            {2, {put_value, 21}}
        ], 1, 2}
    ],
    Chain4 = [
        {choose, get_value, [
            {1, {put_value, 11}},
            {2, {stop, hello}}
        ], {stop, otherwise}}
    ],
    Chain5 = [
        {choose, bad, [
        ]}
    ],
    Handlers = [{handlers, [?MODULE]}],
    [
        {"Good tests for choose", [
            ?_assertEqual(11, tchains:exec(Chain1, 1, Handlers)),
            ?_assertEqual(21, tchains:exec(Chain1, 2, Handlers)),
            ?_assertEqual(otherwise, tchains:exec(Chain1, 10, Handlers))
        ]},
        {"Some bad tests for choose", [
            ?_assertEqual({error, {unknown_selector_value, 3}}, tchains:exec(Chain2, 3, Handlers)),
            ?_assertEqual({stop, bad_otherwise}, tchains:exec(Chain3, does_not_matter, Handlers)),
            ?_assertEqual({error, error}, tchains:exec(Chain5, {error, error}, Handlers)),
            ?_assertEqual({stop, error}, tchains:exec(Chain5, {stop, error}, Handlers)),
            ?_assertEqual(11, tchains:exec(Chain4, 1, Handlers)),
            ?_assertEqual({stop, hello}, tchains:exec(Chain4, 2, Handlers)),
            ?_assertEqual({stop, otherwise}, tchains:exec(Chain4, 3, Handlers))
        ]}
    ].

increment([], Value, _Engine) ->
    Value + 1.

decrement([], Value, _Engine) ->
    Value - 1.

less_than([Limit], Value, _Engine) ->
    Value < Limit.

repeat_while_test_() ->
    Chain1 = [
        {repeat_while, {less_than, 10}, increment}
    ],
    Handlers = [{handlers, [?MODULE]}],
    {"Tests for repeat_while", [
        ?_assertEqual(10, tchains:exec(Chain1, 1, Handlers)),
        ?_assertEqual(11, tchains:exec(Chain1, 11, Handlers))
    ]}.

repeat_until_test_() ->
    Chain1 = [
        {repeat_until, increment, {less_than, 10}}
    ],
    Chain2 = [
        {repeat_until, decrement, {less_than, 5}}
    ],
    Handlers = [{handlers, [?MODULE]}],
    {"Tests for repeat_until", [
        ?_assertEqual(2, tchains:exec(Chain1, 1, Handlers)),
        ?_assertEqual(0, tchains:exec(Chain2, 1, Handlers)),
        ?_assertEqual(4, tchains:exec(Chain2, 10, Handlers))
    ]}.

max([], Values, _Engine) ->
    lists:max(Values).

merge_test_() ->
    BadChain = none,
    Chain1 = [
        {merge_all, [], max}
    ],
    Chain2 = [
        {merge_all, BadChain, max}
    ],
    Chain3 = [
        {merge_any, BadChain, max}
    ],
    Chain4 = [
        {merge_any, [none, get_value], max}
    ],
    Handlers = [{handlers, [?MODULE]}],
    {"Merge tests", [
        ?_assertEqual(1, tchains:exec(Chain1, 1, Handlers)),
        ?_assertMatch({stop, {unknown_task, _}}, tchains:exec(Chain2, 1, Handlers)),
        ?_assertMatch({stop, {all_failed, _}}, tchains:exec(Chain3, 1, Handlers)),
        ?_assertEqual(1, tchains:exec(Chain4, 1, Handlers))
    ]}.

in_context_test_() ->
    Chain1 = [
        {in_context, [], [], []}
    ],
    Chain2 = [
        {in_context, [], []}
    ],
    Handlers = [{handlers, [?MODULE]}],
    {"in_context tests", [
        ?_assertEqual(1, tchains:exec(Chain1, 1, Handlers)),
        ?_assertEqual(1, tchains:exec(Chain2, 1, Handlers))
    ]}.
-endif.
%% }}}
