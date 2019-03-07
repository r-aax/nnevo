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
%%   W - weights array,
%%   B - bias.
create(NNN, NN, W, B) ->
    Atom = utils:neuron_atom(NNN, NN),
    State = #neuron_state
    {
        atom = Atom,
        weights = W ++ [B],
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
                   ipids = IPids,
                   opids = OPids,
                   isignals = ISignals} = State) ->

    % Listen.
    receive

        % Sense.
        {sense, From, Signal} ->

            %io:format("~w: before insert ~w ~w ~w ~w~n",
            %          [Atom, ISignals, Signal, IPids, From]),

            NewISignals = utils:insert_signal(ISignals, Signal, IPids, From),
            IsSignalsReady = utils:is_signals_ready(NewISignals),

            if
                IsSignalsReady ->
                    Dot = utils:dot(NewISignals, Weights),
                    Out = math:tanh(Dot),
                    utils:send_one_to_array(Out, OPids);

                true ->
                    ok
            end,

            loop(State#neuron_state{isignals = NewISignals});

        % Set pids.
        {set_pids, NewIPids, NewOPids} ->

            NewISignals = utils:nones(NewIPids),

            %io:format("~w: IPids (~w), ISignals (~w), OPids (~w) are set~n",
            %          [Atom, NewIPids, NewISignals, NewOPids]),

            loop(State#neuron_state{ipids = NewIPids,
                                    opids = NewOPids,
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
