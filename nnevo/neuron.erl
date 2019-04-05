%% @doc
%% Neuron realization.

% Module name.
-module(neuron).

-include("neuron.hrl").

-export([create/4, loop/1]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Create neuron.
%%   NNN - neuronet number,
%%   NN - neuron number,
%%   Weights - weights array,
%%   Bias - bias.
create(NNN, NN, Weights, Bias) ->
    Atom = utils:neuron_atom(NNN, NN),
    State = #neuron_state
    {
        atom = Atom,
        weights = Weights,
        bias = Bias,
        ipids = [],
        opids = [],
        isignals = []
    },
    Pid = spawn(?MODULE, loop, [State]),
    register(Atom, Pid),
    Pid.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Neuron infinite loop.
%%   State - neuron state.
loop(#neuron_state{atom = Atom,
                   weights = Weights,
                   bias = Bias,
                   ipids = IPids,
                   opids = OPids,
                   isignals = ISignals} = State) ->

    % Listen.
    receive

        % Sense.
        {sense, From, Signal} ->

            NewISignals = utils:insert_signal(ISignals, Signal, IPids, From),
            IsSignalsReady = utils:is_signals_ready(NewISignals),

            if
                IsSignalsReady ->
                    Out = utils:sigmoid(NewISignals, Weights, Bias),
                    utils:send_one_to_array(Out, OPids);

                true ->
                    ok
            end,

            loop(State#neuron_state{isignals = NewISignals});

        % Set pids.
        {set_pids, NewIPids, NewOPids} ->

            NewISignals = utils:nones(NewIPids),

            loop(State#neuron_state{ipids = NewIPids,
                                    opids = NewOPids,
                                    isignals = NewISignals});

        % Add destination.
        {add_dst, Dst} ->
            loop(State#neuron_state{opids = OPids ++ [Dst]});

        % Add source.
        {add_src, Src, W} ->
            NewIPids = IPids ++ [Src],
            NewISignals = utils:nones(NewIPids),
            loop(State#neuron_state{weights = Weights ++ [W],
                                    ipids = NewIPids,
                                    isignals = NewISignals});

        % Stop.
        stop ->
            io:format("~w: stopped~n", [Atom]);

        % Unknown command.
        % Neuron dies.
        Any ->
            io:format("~w: unknown command ~w~n", [Atom, Any])
    end.

%---------------------------------------------------------------------------------------------------
