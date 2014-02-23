-module(my_time).
-export([print_date/0, print_date/6]).

print_date(Y, M, D, Hour, Min, Sec) ->
    Mes = case M of
              1 -> "Jan";
              2 -> "Feb";
              3 -> "Mar";
              4 -> "Apr";
              5 -> "May";
              6 -> "Jun";
              7 -> "Jul";
              8 -> "Ago";
              9 -> "Sep";
              10 -> "Oct";
              11 -> "Nov";
              12 -> "Dez"
          end,
    io:format("Today is ~s  ~w ~2.2.0w:~2.2.0w:~2.2.0w  ~4w~n", [Mes, D, Hour, Min, Sec, Y]).

print_date() ->
    {Y, M, D} = date(),
    {Hour, Min, Sec} = time(),
    print_date(Y, M, D, Hour, Min, Sec).
