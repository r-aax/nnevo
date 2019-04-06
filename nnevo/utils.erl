%% @doc
%% Util functions.

% Module name.
-module(utils).

-export([neuron_atom/2, nnet_atom/1, nones/1,
         dot_b/3, sigmoid/3,
         send_one_to_array/2, send_array_to_array/2,
         insert_signal/4, is_signals_ready/1,
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
%% Arrays of nones.
%%   Arr - array.
nones(Arr) ->
    lists:duplicate(length(Arr), none).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Dot product including bias.
%%   I - inputs vector,
%%   W - weights vector,
%%   B - bias.
dot_b(I, W, B) ->
    lists:sum(lists:zipwith(fun(X, Y) -> X * Y end, I, W)) + B.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Sigmoid function.
%%   I - inputs vector,
%%   W - weights vector,
%%   B - bias.
sigmoid(I, W, B) ->
    1.0 / (1.0 + math:exp(-dot_b(I, W, B))).

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
%% Insert signal to signals array to correct position (from right pid).
%%   Ss - signals array,
%%   S - income signal,
%%   Ps - pids array,
%%   P - pid to check.
insert_signal(Ss, S, Ps, P) ->
    insert_signal(Ss, S, Ps, P, []).

%% @doc
%% Insert signal to signals array to correct position (from right pid).
%%   Ss - signals array,
%%   S - income signal,
%%   Ps - pids array,
%%   P - pid to check,
%%   R - result.
insert_signal([_ | SsT], S, [P | _], P, R) ->
    lists:reverse(R) ++ [S | SsT];
insert_signal([SsH | SsT], S, [_ | PsT], P, R) ->
    insert_signal(SsT, S, PsT, P, [SsH | R]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Check if signals array is ready.
%%   Ss - list of signals.
is_signals_ready(Ss) ->
    lists:all(fun(S) -> S /= none end, Ss).

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
