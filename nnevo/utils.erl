%% @doc
%% Util functions.

% Module name.
-module(utils).

-export([neuron_atom/2, nnet_atom/1,
         nones_signals/1, sigmoid/3,
         send_one_to_array/2, send_array_to_array/2,
         insert_signal/3, is_signals_ready/1,
         multilayer_nnet_edges/1]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Print to atom function.
%%   F - format string.
print_to_atom(F) ->
    fun(P) ->
        list_to_atom(lists:flatten(io_lib:format(F, P)))
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Neuron atom.
%%   NNN - neuronet Number,
%%   NN - neuron Number
neuron_atom(NNN, NN) ->
    (print_to_atom("neuron_~5..0w_~5..0w"))([NNN, NN]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Neuronet atom.
%%   NNN - neuronert number.
nnet_atom(NNN) ->
    (print_to_atom("nnet_~5..0w"))([NNN]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Set signals to nones.
%%   Arr - array.
nones_signals([{_, _} | _] = PS) ->
    lists:map(fun({X, _}) -> {X, none} end, PS);
nones_signals(P) ->
    lists:map(fun(X) -> {X, none} end, P).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Sigmoid function.
%%   PS - inputs vector of Pid-Signal tuples,
%%   W - weights vector,
%%   B - bias.
sigmoid(PS, W, B) ->
    Dot =
        fun
            F([], []) ->
                0.0;
            F([{_, SH} | PST], [WH | WT]) ->
                SH * WH + F(PST, WT)
        end,
    1.0 / (1.0 + math:exp(-Dot(PS, W) + B)).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Send one signal to array of pids.
%%   S - signal,
%%   Ps - pids.
send_one_to_array(S, Ps) ->
    lists:foreach(fun(P) -> P ! {sense, self(), S} end, Ps).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Send array of signals to array of pids.
%%   Ss - signals,
%%   Ps - pids.
send_array_to_array(Ss, Ps) ->
    lists:foreach(fun({S, P}) -> P ! {sense, self(), S} end, lists:zip(Ss, Ps)).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Insert signal to Pid-Signal tuples array into correct position (from right pid).
%%   PS - array of Pid-Signal tuples,
%%   P - pid to check.
%%   S - income signal,
insert_signal(PS, P, S) ->
    {F, [_ | T]} = lists:splitwith(fun({X, _}) -> X /= P end, PS),
    lists:concat([F, [{P, S} | T]]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Check if signals array is ready.
%%   Ss - list of signals.
is_signals_ready(Ss) ->
    lists:all(fun({_, S}) -> S /= none end, Ss).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Multilayer neuronet edges.
%%   Layer - layers.
multilayer_nnet_edges(Layers) ->
    multilayer_nnet_edges(Layers, 1, []).

%% @doc
%% Multilayer neuronet edges.
%%   Layer - layers,
%%   N - neuron current number,
%%   Res - result.
multilayer_nnet_edges([_], _, R) ->
    R;
multilayer_nnet_edges([F, S | T], N, R) ->

    % Edges set from layer F to layer S.
    Fs = lists:seq(N, N + F - 1),
    Ss = lists:seq(N + F, N + F + S - 1),
    E = [{X, Y, 1.0} || X <- Fs, Y <- Ss],

    multilayer_nnet_edges([S | T], N + F, R ++ E).

%---------------------------------------------------------------------------------------------------
