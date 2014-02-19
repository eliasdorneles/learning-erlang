-module(eachcons).
-export([eachcons/2]).

eachcons(L, N) ->
    eachcons(N, L, []).

eachcons(L, N, Result) when length(L) < N ->
    lists:reverse(Result);

eachcons(L = [_|T], N, Result) ->
    eachcons(T, N, [lists:sublist(L, N)|Result]).
