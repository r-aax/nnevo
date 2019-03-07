%% @doc
%% Neuron declaration.

-record(neuron_state,
{
    atom,    % neuron atom
    weights, % weights with bias
    ipids,   % input pids
    opids,   % output pids
    isignals % input signals
}).
