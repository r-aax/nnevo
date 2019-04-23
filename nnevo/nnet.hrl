%% @doc
%% Neuronet declaration.

%% @doc
%% Neuronet state.
-record(nnet_state,
{
    atom,    % atom
    neurons, % all neurons
    fps,     % list of tupples {pid, signal} (first layer data)
    lps,     % list of tupples {pid, signal} (last layer data)
    source   % signal source
}).
