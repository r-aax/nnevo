%% @doc
%% Neuron declaration.

%% @doc
%% Neuron state.
-record(neuron_state,
{
    atom,    % neuron atom
    s,       % saved signals
    weights, % weights
    bias,    % bias
    z,       % z value
    a,       % a value
    e,       % error
    ips,     % list of tupples {pid, signal} (IN data)
    ops      % list of tupples {pid, signal} (OUT data)
}).
