%% @doc
%% Neuron declaration.

%% @doc
%% Neuron state.
-record(neuron_state,
{
    atom,    % neuron atom
    weights, % weights
    bias,    % bias
    ps, % lists of tupples {pid, signal}
    opids    % output pids
}).
