-module(eachcons).
-export([eachcons/2, test/0]).

eachcons(L, N) ->
    eachcons(L, N, []).

eachcons(L, N, Result) when length(L) < N ->
    lists:reverse(Result);

eachcons(L = [_|T], N, Result) ->
    eachcons(T, N, [lists:sublist(L, N)|Result]).

test() ->
    [[1, 2], [2, 3], [3, 4]] = eachcons([1, 2, 3, 4], 2),
    [[1, 2, 3], [2, 3, 4]] = eachcons([1, 2, 3, 4], 3),
    [[1, 2, 3], [2, 3, 4]] = eachcons([1, 2, 3, 4], 3),
    all_passed.
