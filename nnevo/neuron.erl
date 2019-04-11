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
        ps = [],
        opids = []
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
                   ps = PS,
                   opids = OPids} = State) ->

    % Listen.
    receive

        % Sense.
        {sense, From, Signal} ->

            NewPS = utils:insert_signal(PS, From, Signal),
            IsSignalsReady = utils:is_signals_ready(NewPS),

            if
                IsSignalsReady ->
                    Out = utils:sigmoid(NewPS, Weights, Bias),
                    utils:send_one_to_array(Out, OPids),
                    loop(State#neuron_state{ps = utils:nones_signals(PS)});

                true ->
                    loop(State#neuron_state{ps = NewPS})
            end;

        % Set pids.
        {set_pids, NewIPids, NewOPids} ->

            loop(State#neuron_state{ps = lists:zip(NewIPids, utils:nones(NewIPids)),
                                    opids = NewOPids});

        % Add destination.
        {add_dst, Dst} ->
            loop(State#neuron_state{opids = OPids ++ [Dst]});

        % Add source.
        {add_src, Src, W} ->
            NewPS = PS ++ [{Src, none}],
            loop(State#neuron_state{weights = Weights ++ [W],
                                    ps = NewPS});

        % Stop.
        stop ->
            io:format("~w: stopped~n", [Atom]);

        % Unknown command.
        % Neuron dies.
        Any ->
            io:format("~w: unknown command ~w~n", [Atom, Any])
    end.

%---------------------------------------------------------------------------------------------------
