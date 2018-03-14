-module(stdinout).
-compile(export_all).

main() ->
    io:fwrite(io:get_line("")).

