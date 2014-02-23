-module(math_functions).
-compile(export_all).

even(X) ->
    X rem 2 =:= 0.

odd(X) ->
    X rem 2 =:= 1.

filter(_, []) -> [];
filter(F, [H|T]) ->
    case F(H) of
        true  -> [H|filter(F, T)];
        false -> filter(F, T)
    end.

odds_and_evens_with_filter(L) ->
    {filter(fun odd/1, L), filter(fun even/1, L)}.

odds_and_evens(L) ->
    odds_and_evens_acc(L, [], []).

odds_and_evens_acc([], Odds, Evens) ->
    {lists:reverse(Odds), lists:reverse(Evens)};
odds_and_evens_acc([H|T], Odds, Evens) ->
    case (H rem 2) of
        0 -> odds_and_evens_acc(T, Odds, [H|Evens]);
        1 -> odds_and_evens_acc(T, [H|Odds], Evens)
    end.

