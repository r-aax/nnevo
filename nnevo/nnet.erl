%% @doc
%% Neuronet realization.

% Module name.
-module(nnet).

-export([create/1, loop/4]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Create neuronnet.
%%   NNN - neuronert number.
create(NNN) ->
    A = utils:nnet_atom(NNN),

    % Test neurons.
    %
    %    --->N00---->N10---
    %   /       \   /      \
    %  S          X         A
    %   \       /   \      /
    %    --->N01---->N11---
    %
    N00 = neuron:create(NNN, 0, [0.5], 0.4),
    N01 = neuron:create(NNN, 1, [0.8], -1.2),
    N10 = neuron:create(NNN, 2, [1.2, 0.3], -0.1),
    N11 = neuron:create(NNN, 3, [0.9, 1.7], 0.7),
    FirstLayer = [N00, N01],
    LastLayer = [N10, N11],

    Pid = spawn(?MODULE, loop,
                [A,
                 FirstLayer,
                 LastLayer,
                 lists:duplicate(length(LastLayer), none)]),
    register(A, Pid),

    % Set pids.
    N00 ! {set_pids, [Pid], [N10, N11]},
    N01 ! {set_pids, [Pid], [N10, N11]},
    N10 ! {set_pids, [N00, N01], [Pid]},
    N11 ! {set_pids, [N00, N01], [Pid]},

    Pid.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Neuronert infinite loop.
%%   A - atom,
%%   NNN - neuronet number,
%%   FirstLayer - first layer,
%%   LastLayer - last layer,
%%   OSignals - out signals.
loop(A, FirstLayer, LastLayer, OSignals) ->
    receive

        % Sense from outer world.
        {sense, Signal} ->
            utils:send_array_to_array(Signal, FirstLayer),
            io:format("~w: ~w is sent to ~w~n", [A, Signal, FirstLayer]),
            loop(A, FirstLayer, LastLayer, lists:duplicate(length(LastLayer), none));

        % Sense from last layer.
        {sense, From, Signal} ->

            %io:format("~w: before insert ~w ~w ~w ~w~n", [A, OSignals, Signal, LastLayer, From]),
            NewOSignals = utils:insert_signal(OSignals, Signal, LastLayer, From),

            IsSignalsReady = utils:is_signals_ready(NewOSignals),

            if
                IsSignalsReady ->
                    io:format("~w: out signals ~w~n", [A, NewOSignals]),
                    loop(A, FirstLayer, LastLayer, lists:duplicate(length(LastLayer), none));

                true ->
                    loop(A, FirstLayer, LastLayer, NewOSignals)
            end;

        % Stop command.
        stop ->
            %% @todo
            %% We must stop all neurons.
            ok;

        % Unknown command.
        % Neuron dies.
        Any ->
            io:format("~w: unknown command ~w~n", [A, Any])
    end.

%---------------------------------------------------------------------------------------------------
