-module(main).
-import(read_input, [read_input/0]).
-compile(export_all).

-record(input_data, {initial = "", nodes = []}).
-record(node_data, {name, connections}).

main() ->
    InputData = read_input:read_input(),
    io:fwrite("~p~n", [InputData#input_data.initial]),
    io:fwrite("~p~n", [InputData]).

