%% @doc
%% Util functions.

% Module name.
-module(utils).

-export([neuron_atom/2, dot/2]).

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
    (print_to_atom("neuron_~3..0w_~3..0w"))([NNN, NN]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Dot product (including bias).
%%   I - inputs vector,
%%   W - weights vector and bias.
dot(I, W) ->
    dot(I, W, 0.0).

%% @private
%% @doc
%% Dot product (including bias).
%%   I - inputs vector,
%%   W - weights vector,
%%   Acc - accumulator.
dot([], [B], Acc)->
    Acc + B;
dot([HI | TI], [HW | TW], Acc) ->
    dot(TI, TW, Acc + HI * HW).

%---------------------------------------------------------------------------------------------------
