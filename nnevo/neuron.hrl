%% @doc
%% Neuron declaration.

%% @doc
%% Neuron state.
-record(neuron_state,
{
    atom,    % neuron atom
    weights, % weights
    bias,    % bias
    ips,     % list of tupples {pid, signal} (IN data)
    ops      % list of tupples {pid, signal} (OUT data)
}).
