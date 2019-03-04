%% @doc
%% Util functions.

% Module name.
-module(utils).

-export([neuron_atom/2, nnet_atom/1,
         dot/2,
         send_one_to_array/2, send_array_to_array/2,
         insert_signal/4, is_signals_ready/1]).

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
%% Neuronet atom.
%%   NNN - neuronert number.
nnet_atom(NNN) ->
    (print_to_atom("nnet_~3..0w"))([NNN]).

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

%% @doc
%% Send one signal to array of pids.
%%   S - signal,
%%   P - pids.
send_one_to_array(_, []) ->
    ok;
send_one_to_array(S, [PH | PT]) ->
    PH ! {sense, self(), S},
    send_one_to_array(S, PT).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Send array of signals to array of pids.
%%   S - signals,
%%   P - pids.
send_array_to_array([], []) ->
    ok;
send_array_to_array([SH | ST], [PH | PT]) ->
    PH ! {sense, self(), SH},
    send_array_to_array(ST, PT).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Insert signal to signals array to correct position (from right pid).
%%   Signasls - signals array,
%%   Signal - income signal,
%%   Pids - pids array,
%%   Pid - pid to check.
insert_signal(Signals, Signal, Pids, Pid) ->
    insert_signal(Signals, Signal, Pids, Pid, []).

%% @doc
%% Insert signal to signals array to correct position (from right pid).
%%   Signasls - signals array,
%%   Signal - income signal,
%%   Pids - pids array,
%%   Pid - pid to check,
%%   NewSignals - new signals array.
insert_signal([_ | ST], Signal, [Pid | _], Pid, NewSignals) ->
    lists:reverse(NewSignals) ++ [Signal | ST];
insert_signal([SH | ST], Signal, [_ | PT], Pid, NewSignals) ->
    insert_signal(ST, Signal, PT, Pid, [SH | NewSignals]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Check if signals array is ready.
is_signals_ready([]) ->
    true;
is_signals_ready([none | _]) ->
    false;
is_signals_ready([_ | T]) ->
    is_signals_ready(T).

%---------------------------------------------------------------------------------------------------
