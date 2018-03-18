-module(read_input).
-include_lib("tarry_records.hrl").
-export([read_input/0]).

%%%%%%%%%%%%%%%%%%%%%%%
% Main method to call %
%%%%%%%%%%%%%%%%%%%%%%%

% Get the input from stdin and return it as an input_data record.
read_input() ->
    InputDataInit = read_first(),
    InputData = read_rest(InputDataInit),
    check_input(InputData),
    InputData.

%%%%%%%%%%%%%%%%%
% Reading input %
%%%%%%%%%%%%%%%%%

% Gets the next line from stdin and handles different eof characters. 
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
            %Removes trailing \n and \r characters from non eof lines.
            string:chomp(Line)
    end.

%Get initiator node.
read_first() ->
    Line = get_line(),
    #input_data{initial = Line, nodes = []}.

% Reads the rest of the lines into the record.
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

%%%%%%%%%%%%%%%%%%
% Checking input %
%%%%%%%%%%%%%%%%%%

% Check the input is valid.
check_input(#input_data{initial = Init, nodes = Nodes}) ->
    check_valid_initiator(Init),
    check_empty_node_data(Nodes),
    check_initator_in_nodes(Init, Nodes).

% Check the initiator is valid.
check_valid_initiator(Init) ->
    MatchCheck = re:run(Init, "^[0-9A-Za-z]+$"),
    if
        MatchCheck == nomatch ->
            erlang:error("Bad initator.");
        true ->
            ok
    end.

% Check for node data.
check_empty_node_data(Nodes) ->
    if
        length(Nodes) == 0 ->
            erlang:error("Only initator given, no nodes.");
        true ->
            ok
    end.

% Check that the initator has coresponding node data.
check_initator_in_nodes(Init, [#node_data{name = NodeName} | Nodes]) ->
    if
        Init == NodeName ->
            ok;
        true ->
            check_initator_in_nodes(Init, Nodes)
    end;

% Initiator was not found in the node data list.
check_initator_in_nodes(_, []) ->
    erlang:error("Initiator has no node data.").
