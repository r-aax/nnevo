%% @doc
%% Neuron declaration.

%% @doc
%% Neuron state.
-record(neuron_state,
{
    atom,    % neuron atom
    weights, % weights
    bias,    % bias
    ipids,   % input pids
    opids,   % output pids
    isignals % input signals
}).
