-module(conc_tests).
-compile(export_all).

start(AnAtom, Fun) ->
    case whereis(AnAtom) of
        undefined ->
            io:format("non tem processo pro atomo: ~p~n", [AnAtom]),
            register(AnAtom, spawn(Fun)),
            io:format("Criei processo pra fun: ~p, com pid: ~p~n", [Fun, whereis(AnAtom)]);
        Pid ->
            throw({atomAlreadyRegistered, AnAtom, Pid})
    end.

