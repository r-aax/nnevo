%% @doc
%% Neuron declaration.

%% @doc
%% Neuron state.
-record(neuron_state,
{
    atom,    % neuron atom
    weights, % weights
    bias,    % bias
    ips,     % lists of tupples {pid, signal} (input signals)
    opids    % output pids
}).
