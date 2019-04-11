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
        in = [],
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
                   in = In,
                   opids = OPids} = State) ->

    % Listen.
    receive

        % Sense.
        {sense, From, Signal} ->

            NewIn = utils:insert_signal_2(In, From, Signal),
            IsSignalsReady = utils:is_signals_ready_2(NewIn),

            if
                IsSignalsReady ->
                    Out = utils:sigmoid_2(NewIn, Weights, Bias),
                    utils:send_one_to_array(Out, OPids),
                    loop(State#neuron_state{in = utils:nones_2(In)});

                true ->
                    loop(State#neuron_state{in = NewIn})
            end;

        % Set pids.
        {set_pids, NewIPids, NewOPids} ->

            loop(State#neuron_state{in = lists:zip(NewIPids, utils:nones(NewIPids)),
                                    opids = NewOPids});

        % Add destination.
        {add_dst, Dst} ->
            loop(State#neuron_state{opids = OPids ++ [Dst]});

        % Add source.
        {add_src, Src, W} ->
            NewIn = In ++ [{Src, none}],
            loop(State#neuron_state{weights = Weights ++ [W],
                                    in = NewIn});

        % Stop.
        stop ->
            io:format("~w: stopped~n", [Atom]);

        % Unknown command.
        % Neuron dies.
        Any ->
            io:format("~w: unknown command ~w~n", [Atom, Any])
    end.

%---------------------------------------------------------------------------------------------------
