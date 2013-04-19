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

-module(tchains_engine).

-export([
    new/1,
    env/1,
    is_bad/1,
    fail_if_bad/1,
    exec_catch/3,
    exec/3,
    error/1,
    stop/1,
    fail/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

% -include("tchains_internal.hrl").

-define(REQUIRED_ARITY, 3).
-define(IS_BAD(Atom), Atom == 'error' orelse Atom == 'stop').

-record(engine, {
    handlers,
    env
}).

-type error() :: {'error', Reason :: term()} | {'stop', Reason :: term()}.

-opaque engine() :: #engine{}.
-export_type([
    engine/0,
    error/0
]).

new(Params) ->
    case proplists:get_value(handlers, Params, []) of
        [] ->
            {error, no_handler_modules};

        Candidates ->
            Handlers = lists:map(fun get_tasks/1, Candidates),
            Env = proplists:get_value(env, Params, []),
            {ok, #engine{handlers=Handlers, env=Env}}
    end.

env(#engine{env=Env}) ->
    Env.

is_bad({Bad, _}) when ?IS_BAD(Bad) ->
    true;
is_bad(_) ->
    false.

fail_if_bad({Bad, _}=Error) when ?IS_BAD(Bad) ->
    fail(Error);
fail_if_bad(Value) ->
    Value.

-spec exec_catch(Chain, Value, Engine) -> Result when
    Chain :: [term()],          %% this requires a bit better specification
    Value :: term(),
    Engine :: engine(),
    Result :: error() | term().

exec_catch(Chain, Value, Engine) ->
    try
        exec(Chain, Value, Engine)
    catch
        {Bad, _}=Result when ?IS_BAD(Bad) ->
            Result
    end.

-spec exec(Chain, Value, Engine) -> Result when
    Chain :: [term()],          %% this requires a bit better specification
    Value :: term(),
    Engine :: engine(),
    Result :: term().

exec(Chain, Value, Engine) when is_list(Chain) ->
    do_exec(Chain, Value, Engine);
exec(Chain, Value, Engine) ->
    do_exec([Chain], Value, Engine).

stop(Reason) ->
    fail({stop, Reason}).

error(Reason) ->
    fail({error, Reason}).

fail(Reason) ->
    throw(Reason).

do_exec([], Value, _Engine) ->
    Value;
do_exec([Item | Rest], Value, Engine) ->
    do_exec(Rest, if
        is_tuple(Item) ->
            [Task | Args] = tuple_to_list(Item),
            execute_one(Task, Args, Value, Engine);

        true ->
            execute_one(Item, [], Value, Engine)
    end, Engine).

execute_one(Task, Args, Value, #engine{handlers=Handlers}=Engine) ->
    wrap_errors(resolve(Task, Handlers), Args, Value, Engine).

get_tasks(Module) ->
    {Module, lists:map(fun get_tasks_2/1,
                       lists:filter(fun get_tasks_1/1,
                                    Module:module_info(exports)))}.

get_tasks_1({Method, Arity}) when Method =/= module_info, Arity == ?REQUIRED_ARITY ->
    true;
get_tasks_1(_) ->
    false.

get_tasks_2({Method, _}) ->
    Method.

resolve(Task, Modules) when is_atom(Task) ->
    do_resolve(Task, Modules);
resolve(Invalid, _) ->
    stop({invalid_task_spec, Invalid}).

do_resolve(Task, []) ->
    stop({unknown_task, Task});
do_resolve(Task, [{Module, Tasks} | Rest]) ->
    case lists:member(Task, Tasks) of
        true ->
            fun Module:Task/?REQUIRED_ARITY;

        _ ->
            do_resolve(Task, Rest)
    end.

wrap_errors(Fun, Args, Value, Engine) ->
    try
        Fun(Args, Value, Engine)
    catch
        Exception:Reason ->
            case erlang:get_stacktrace() of
                [{?MODULE, Function, 1, _} | _] when Function == fail ->
                    fail(Reason);

                _ ->
                    stop({Exception, Reason})
            end
    end.

-ifdef(TEST).
divide([A, B], _, _Engine) ->
    A / B.

sanity_test_() ->
    Chain1 = [
        "hello"
    ],
    Chain2 = [
        {choose, [], [], []}
    ],
    Chain3 = [
        {divide, 3, 0}
    ],
    Chain4 = [
        oopsy
    ],
    Params = [{handlers, [?MODULE, tchains_flow]}],
    {"Sanity tests", [
        ?_assertMatch({stop, {invalid_task_spec, _}}, tchains:exec(Chain1, does_not_matter, Params)),
        ?_assertEqual(does_not_matter, tchains:exec(Chain2, does_not_matter, Params)),
        ?_assertMatch({stop, _}, tchains:exec(Chain3, does_not_matter, Params)),
        ?_assertEqual({stop, {unknown_task, oopsy}}, tchains:exec(Chain4, does_not_matter, Params)),
        ?_assert(is_bad({error, yes})),
        ?_assert(is_bad({stop, yes})),
        ?_assertNot(is_bad(good_value))
    ]}.
-endif.
