-module(socket_examples).
-compile(export_all).
-import(lists, [reverse/1]).

get_url(Url) ->
    B = nano_get_url(Url),
    split_headers_body(B).

split_headers_body(B) ->
    S = binary_to_list(B),
    SplitPos = string:str(S, "\r\n\r\n"),
    RequestHeaders = string:substr(S, 1, SplitPos - 1),
    [HttpStatusLine|HeaderFields] = string:tokens(RequestHeaders, "\r\n"),
    [{status, HttpStatusLine},
     {headers, parse_http_headers(HeaderFields)},
     {body, string:substr(S, SplitPos + 4)}].

parse_http_headers([H]) -> [H];
parse_http_headers([H|T]) ->
    [H|parse_http_headers(T)].

nano_get_url() ->
    nano_get_url("www.google.com").

nano_get_url(Host) ->
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin} ->
            receive_data(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->
            list_to_binary(reverse(SoFar))
    end.
