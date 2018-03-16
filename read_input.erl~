-module(read_input).
-import(string, [chomp/1, split/3]).
-include_lib("tarry_records.hrl").
-export([read_input/0]).

% Gets the line, and nicely handles EOF.
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

% Reads the rest of stdin.
read_rest(#input_data{initial = Init, nodes = Nodes}) ->
    Line = get_line(),
    if
        Line == eof ->
            #input_data{initial = Init, nodes = Nodes};
        true ->
            [NodeName | NodeNeighbours] = string:split(Line, " ", all),
            NodeData = #node_data{name = NodeName, neighbours = NodeNeighbours},
            NewData = #input_data{initial = Init, nodes = [NodeData | Nodes]},
            read_rest(NewData)
    end.

read_first() ->
    Line = get_line(),
    #input_data{initial = Line, nodes = []}.

read_input() ->
    InputData = read_first(),
    read_rest(InputData).
