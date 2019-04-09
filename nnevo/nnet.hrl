%% @doc
%% Neuronet declaration.

%% @doc
%% Neuronet state.
-record(nnet_state,
{
    atom,    % atom
    flayer,  % first layer
    llayer,  % last layer
    source,   % signal source
    osignals % output signals
}).
