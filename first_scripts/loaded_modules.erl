-module(loaded_modules).
-compile(export_all).

by_number_of_functions() ->
    by_number_of_functions(10).

by_number_of_functions(Max) ->
    ModsWithFuns = [{ModName, length(Functions)}
                    || ModName <- [Mod || {Mod, _} <- code:all_loaded()],
                       {exports, Functions} <- ModName:module_info()],
    lists:sublist(lists:reverse(lists:keysort(2, ModsWithFuns)), 1, Max).

