-module(tarry_node).
-export([setup/1]).

% Method to call when spawning.
setup(Name) ->
    % Listen for neighbours.
    receive 
        Neighbours ->
            wait_for_token_from_parent(Name, Neighbours)
    end.

% Listens for the first message to be received (from parent).
wait_for_token_from_parent(Name, Neighbours) ->
    receive
        {Pid, Token} ->
            % This filters the Pid from the list Neighbours
            NeighboursMinusParent = [E || E <- Neighbours, E /= Pid],
            handle_token(Name, NeighboursMinusParent, Pid, Token);
        Bad ->
            io:fwrite("Bad initial token error: ~p~n", [Bad]),
            erlang:error("Setup received bad initial token information.")
    end.

% Listens for a message to come in from a parent.
wait_for_token(Name, Neighbours, ParentPid) ->
    receive
        {_, Token} ->
            handle_token(Name, Neighbours, ParentPid, Token);
        Bad ->
            io:fwrite("Bad token error: ~p~n", [Bad]),
            erlang:error("Setup received bad token information.")
    end.

% Handles the token.
handle_token(Name, Neighbours, ParentPid, Token) ->
    % Add ourselves to the token.
    NextToken = Token ++ [Name],
    % Send to next neighbour, or back to parent.
    case Neighbours of
        [] ->
            ParentPid ! {self(), NextToken};
        _ ->
            SendTarget = lists:nth(rand:uniform(length(Neighbours)), Neighbours),
            Others = [E || E <- Neighbours, E /= SendTarget],
            SendTarget ! {self(), NextToken},
            wait_for_token(Name, Others, ParentPid)
    end.
