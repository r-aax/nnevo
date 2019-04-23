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
        z = 0.0,
        a = 0.0,
        e = 0.0,
        ips = [],
        ops = []
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
                   ips = IPS,
                   ops = OPS} = State) ->

    % Listen.
    receive

        % Forward propagation.
        {forward, From, Signal} ->

            NewIPS = utils:insert_signal(IPS, From, Signal),
            IsSignalsReady = utils:is_signals_ready(NewIPS),

            if
                IsSignalsReady ->
                    {NewZ, NewA} = utils:sigmoid(NewIPS, Weights, Bias),
                    utils:send_one_to_array(forward, NewA, OPS),
                    loop(State#neuron_state{z = NewZ, a = NewA, ips = utils:nones_signals(IPS)});

                true ->
                    loop(State#neuron_state{ips = NewIPS})
            end;

        % Act.
        {act, F} ->
            F(State),
            loop(State);

        % Set pids.
        {set_pids, NewIPids, NewOPids} ->
            loop(State#neuron_state{ips = utils:nones_signals(NewIPids),
                                    ops = utils:nones_signals(NewOPids)});

        % Add destination.
        {add_dst, Dst} ->
            NewOPS = OPS ++ [{Dst, none}],
            loop(State#neuron_state{ops = NewOPS});

        % Add source.
        {add_src, Src, W} ->
            NewIPS = IPS ++ [{Src, none}],
            loop(State#neuron_state{weights = Weights ++ [W],
                                    ips = NewIPS});

        % Stop.
        stop ->
            io:format("~w: stopped~n", [Atom]);

        % Unknown command.
        Any ->
            io:format("~w: unknown command ~w~n", [Atom, Any])
    end.

%---------------------------------------------------------------------------------------------------
