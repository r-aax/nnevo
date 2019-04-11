%% @doc
%% Neuronet declaration.

%% @doc
%% Neuronet state.
-record(nnet_state,
{
    atom,    % atom
    neurons, % all neurons
    flayer,  % first layer
    ps,      % list of tupples {pid, signal} (output signals)
    source   % signal source
}).
