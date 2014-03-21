-module(socket_examples).
-compile(export_all).
-import(lists, [reverse/1]).

get_url(Url) ->
    [HostAndQuery] = lists:filter(fun(L) -> length(L) > 0 end, split(Url, "http://", 1)),
    case split(HostAndQuery, "/", 1) of
        [Host, Query] ->
            split_headers_body(nano_get_url(Host, "/" ++ Query));
        [Host] ->
            split_headers_body(nano_get_url(Host, "/"))
    end.

split_headers_body(B) ->
    S = binary_to_list(B),
    [RequestHeaders, Body] = split(S, "\r\n\r\n", 1),
    [HttpStatusLine|HeadersSection] = string:tokens(RequestHeaders, "\r\n"),
    [HttpVersion, StatusCode, StatusText] = parse_status_line(HttpStatusLine),
    Headers = parse_http_headers(HeadersSection),
    case StatusCode of
        "302" ->
            {ok, LocationUrl} = get_header("Location", Headers),
            get_url(LocationUrl);
        _ ->
            [{httpVersion, HttpVersion},
             {statusCode, StatusCode},
             {statusText, StatusText},
             {headers, Headers},
             {body, Body}]
    end.

get_header(_, []) ->
    {error};
get_header(Field, HeaderFields) ->
    [{Key,Value}|T] = HeaderFields,
    case Key of
        Field -> {ok,Value};
        _ -> get_header(Field, T)
    end.

split(S, Sep) ->
    split(S, Sep, -1).
split(S, Sep, Max) ->
    case Max of
        0 -> [S];
        _ ->
            case SplitPos = string:str(S, Sep) of
                0 -> [S];
                SplitPos -> [string:substr(S, 1, SplitPos - 1)|
                             split(string:substr(S, SplitPos + length(Sep)), Sep, Max - 1)]
            end
    end.


parse_status_line(S) ->
    split(S, " ", 2).

parse_http_headers([]) -> [];
parse_http_headers(HeaderList) ->
    [H|T] = HeaderList,
    [list_to_tuple(split(H, ": "))|parse_http_headers(T)].

nano_get_url(Host) ->
    nano_get_url(Host, "/").

nano_get_url(Host, Query) ->
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    Request = "GET " ++ Query ++ " HTTP/1.0\r\n\r\n",
    ok = gen_tcp:send(Socket, Request),
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin} ->
            receive_data(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->
            list_to_binary(reverse(SoFar))
    end.
