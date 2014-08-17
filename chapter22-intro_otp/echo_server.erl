-module(echo_server).
-compile(export_all).
-import(server4, [rpc/2]).

echo(Something) -> rpc(name_server, {echo, Something}).

init() -> stateless.
handle({echo, Something}, State) -> {Something, State}.

