%% @doc
%% Neuronet realization.

% Module name.
-module(nnet).

-include("nnet.hrl").

-export([create/1, loop/1,
         sense/2]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Create neuronnet.
%%   NNN - neuronert number.
create(NNN) ->
    Atom = utils:nnet_atom(NNN),

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

    LLayer = [N10, N11],
    State = #nnet_state
    {
        atom = Atom,
        flayer = [N00, N01],
        llayer = LLayer,
        source = none,
        osignals = utils:nones(LLayer)
    },
    Pid = spawn(?MODULE, loop, [State]),
    register(Atom, Pid),

    % Set pids.
    N00 ! {set_pids, [Pid], [N10, N11]},
    N01 ! {set_pids, [Pid], [N10, N11]},
    N10 ! {set_pids, [N00, N01], [Pid]},
    N11 ! {set_pids, [N00, N01], [Pid]},

    Pid.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Neuronert infinite loop.
%%   State - state.
loop(#nnet_state{atom = Atom,
                 flayer = FLayer,
                 llayer = LLayer,
                 source = Source,
                 osignals = OSignals} = State) ->

    % Listen.
    receive

        % Sense from outer world.
        {out_sense, From, Signal} ->
            utils:send_array_to_array(Signal, FLayer),
            loop(State#nnet_state{source = From});

        % Sense from last layer.
        {sense, From, Signal} ->

            NewOSignals = utils:insert_signal(OSignals, Signal, LLayer, From),
            IsSignalsReady = utils:is_signals_ready(NewOSignals),

            if
                IsSignalsReady ->
                    Source ! {response, self(), NewOSignals},
                    loop(State#nnet_state{source = none, osignals = utils:nones(LLayer)});

                true ->
                    loop(State#nnet_state{osignals = NewOSignals})
            end;

        % Stop command.
        stop ->
            %% @todo
            %% We must stop all neurons.
            ok;

        % Unknown command.
        % Neuron dies.
        Any ->
            io:format("~w: unknown command ~w~n", [Atom, Any])
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Sense neuronet.
%%   Pid - process id,
%%   Signal - signal.
sense(Pid, Signal) ->
    Pid ! {out_sense, self(), Signal},
    receive
        {response, Pid, Out} ->
            Out
    end.

%---------------------------------------------------------------------------------------------------
