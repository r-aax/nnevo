%% @doc
%% Neuronet declaration.

-record(nnet_state,
{
    atom,    % atom
    flayer,  % first layer
    llayer,  % last layer
    osignals % output signals
}).
