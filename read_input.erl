-module(read_input).
-import(string, [chomp/1, split/3]).
-compile(export_all).

-record(input_data, {initial = "", nodes = []}).
-record(node_data, {name, connections}).

get_line() ->
    Line = io:get_line(""),
    if
        Line == eof ->
            eof;
        Line == "" ->
            eof;
        Line == "\n" ->
            eof;
        Line == "\r\n" ->
            eof;
        true ->
            string:chomp(Line)
    end.

read_rest(#input_data{initial = Init, nodes = Nodes}) ->
    Line = get_line(),
    if
        Line == eof ->
            #input_data{initial = Init, nodes = Nodes};
        true ->
            [NodeName | NodeConnections] = string:split(Line, " ", all),
            NodeData = #node_data{name = NodeName, connections = NodeConnections},
            NewData = #input_data{initial = Init, nodes = [NodeData | Nodes]},
            read_rest(NewData)
    end.

read_first() ->
    Line = get_line(),
    #input_data{initial = Line, nodes = []}.

read_input() ->
    InputData = read_first(),
    read_rest(InputData).

main() ->
    InputData = read_input(),
    io:fwrite("~p~n", [InputData]).

