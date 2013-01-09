-module(tchains_utils).

-export([
    keyfind/4
]).

keyfind(Key, N, TupleList, Default) ->
    case lists:keyfind(Key, N, TupleList) of
        {_, Value} ->
            Value;

        _ ->
            Default
    end.
