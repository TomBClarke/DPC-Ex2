-module(nodeeg).
-compile(export_all).

% Starts nodes
start() ->
    io:format("Running~n"),
    Pid = spawn(nodeeg, node, ["node"] ),
    Pid ! {self()},
    Pid2 = spawn(nodeeg, node, ["node2"] ),
    Pid2 ! {self()},
    io:format("Made 2 nodes~n"),
    receive X ->
        io:format ("Start ~p got : ~p~n", [self(), X])
    end ,
    % The below receive needs to be Y (well, not X) otherwise it doesn't work
    receive Y ->
        io:format ("Start ~p got : ~p~n", [self(), Y])
    end .

% The function called when a node is created
node(N) ->
    receive
        {X} ->
            io:format(" Node ~p got : ~p ~n", [self(), X]), X ! {self(), N};
        X ->
            io:format("Node ~p got bad message : ~p~n", [self(), X])
    end .
