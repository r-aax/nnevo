%% @doc
%% Neuronet declaration.

%% @doc
%% Neuronet state.
-record(nnet_state,
{
    atom,    % atom
    neurons, % all neurons
    flayer,  % first layer
    lps,     % list of tupples {pid, signal} (last layer data)
    source   % signal source
}).
