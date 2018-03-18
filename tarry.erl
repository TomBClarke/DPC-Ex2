-module(tarry).
-import(read_input, [read_input/0]).
-include_lib("tarry_records.hrl").
-export([main/0]).

% Main method.
main() ->
    % Get data from stdin.
    InputData = read_input:read_input(),
    Nodes = InputData#input_data.nodes,
    % Setup the nodes and send neighbour information.
    Pids = setup_nodes(Nodes),
    send_node_neighbours(Nodes, Pids, 
        length(Nodes)),
    [Initiator|_] = get_pids_of_neighbours([InputData#input_data.initial], 
        Nodes, Pids),
    % Start the algorithm by giving the initiator the empty token.
    Initiator ! {self(), []},
    receive
        {_, Token} -> 
            % Receive the token after the algorithm has finished and print it.
            TokenString = string:join(Token, " "),
            io:fwrite("~ts", [TokenString]);
        Bad ->
            io:fwrite("Bad token error: ~p~n", [Bad]),
            erlang:error("Main received bad token.")
    end.

% Spawns nodes and returns Pids (in the same order as original nodes).
setup_nodes([]) ->
    [];

setup_nodes([NodeData|Tail]) ->
    Pid = spawn(tarry_node, setup, [NodeData#node_data.name]),
    Pids = setup_nodes(Tail),
    [Pid|Pids].

% Tell each node its neighbours Pids.
send_node_neighbours(_, _, 0) ->
    ok;

send_node_neighbours(Nodes, Pids, Index) ->
    NodeData = lists:nth(Index, Nodes),
    NeighbourPids = get_pids_of_neighbours(NodeData#node_data.neighbours, 
        Nodes, Pids),
    lists:nth(Index, Pids) ! NeighbourPids,
    send_node_neighbours(Nodes, Pids, Index - 1).

% Get Pids of neighbours.
get_pids_of_neighbours(_, [], []) ->
    [];

get_pids_of_neighbours(Neighbours, [Node|NT], [Pid|PT]) ->
    Rest = get_pids_of_neighbours(Neighbours, NT, PT),
    IsMember = lists:member(Node#node_data.name, Neighbours),
    if
        IsMember ->
            [Pid|Rest];
        true ->
            Rest
    end.
