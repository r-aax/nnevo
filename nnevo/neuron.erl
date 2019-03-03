%% @doc
%% Neuron realization.

% Module name.
-module(neuron).

-export([create/4, loop/1]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Create neuron.
%%   NNN - neuronet number,
%%   NN - neuron number,
%%   W - weights array,
%%   B - bias.
create(NNN, NN, W, B) ->
    Pid = spawn(?MODULE, loop, [W ++ [B]]),
    register(utils:neuron_atom(NNN, NN), Pid),
    Pid.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Neuron infinite loop.
%%   W - weights array (including bias).
loop(W) ->
    receive

        % Sense.
        {From, I} ->
            Dot = utils:dot(I, W),
            Out = math:tanh(Dot),
            From ! Out,
            loop(W);

        % Unknown command.
        % Neuron dies.
        _ ->
            io:format("neuron: unknown command~n")
    end.

%---------------------------------------------------------------------------------------------------
