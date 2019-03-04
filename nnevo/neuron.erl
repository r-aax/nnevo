%% @doc
%% Neuron realization.

% Module name.
-module(neuron).

-export([create/4, loop/5]).

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
    A = utils:neuron_atom(NNN, NN),
    Pid = spawn(?MODULE, loop, [A, W ++ [B], none, none, none]),
    register(A, Pid),
    Pid.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Neuron infinite loop.
%%   A - atom,
%%   W - weights array (including bias),
%%   IPids - input pids,
%%   ISignals - input signals,
%%   OPids - output pids.
loop(A, W, IPids, ISignals, OPids) ->
    receive

        % Sense.
        {sense, From, Signal} ->
            %io:format("~w: before insert ~w ~w ~w ~w~n", [A, ISignals, Signal, IPids, From]),
            NewISignals = utils:insert_signal(ISignals, Signal, IPids, From),

            IsSignalsReady = utils:is_signals_ready(NewISignals),

            if
                IsSignalsReady ->
                    %io:format("~w: before dot ~w ~w~n", [A, NewISignals, W]),
                    Dot = utils:dot(NewISignals, W),
                    Out = math:tanh(Dot),
                    utils:send_one_to_array(Out, OPids);

                true ->
                    ok
            end,

            loop(A, W, IPids, NewISignals, OPids);

        % Set pids.
        {set_pids, NewIPids, NewOPids} ->
            NewISignals = lists:duplicate(length(NewIPids), none),
            io:format("~w: IPids (~w), ISignals (~w), OPids (~w) are set~n",
                      [A, NewIPids, NewISignals, NewOPids]),
            loop(A, W, NewIPids, NewISignals, NewOPids);

        % Stop.
        stop ->
            io:format("~w: stopped~n", [A]);

        % Unknown command.
        % Neuron dies.
        Any ->
            io:format("~w: unknown command ~w~n", [A, Any])
    end.

%---------------------------------------------------------------------------------------------------
