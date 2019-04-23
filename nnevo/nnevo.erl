%% @doc
%% Main module.

% Module name.
-module(nnevo).

-include("neuron.hrl").

-export([start/0,
         test_1_run/0, mnist_run/0]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Test run.
test_1_run() ->
    Net = nnet:create_multilayer(1, [1, 1, 1]),
    test_1_run(Net).

%% @doc
%% Test run.
%%   Net - neuronet.
test_1_run(Net) ->
    X = [1.0],
    Y = [1.0],
    test_1_run(Net, X, Y).

%% @doc
%% Test run.
%%   Net - neuronet,
%%   X - input vector,
%%   Y - output vector (right).
test_1_run(Net, X, Y) ->
    A = nnet:sense_forward(Net, X),
    C = utils:cost(Y, A),

    if
        C < 0.001 ->
            io:format("test_1_run : learning is finished (cost = ~w)~n", [C]);

        true ->
            io:format("test_1_run : cost = ~w~n", [C]),
            nnet:sense_back(Net, lists:zipwith(fun(Y1, A1) -> Y1 - A1 end, Y, A)),
            nnet:correct_weights_and_biases(Net, 0.0001),
            test_1_run(Net, X, Y)
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Run MNIST tests.
mnist_run() ->

    % Simple neuronet for digits recognition.
    Net = nnet:create_multilayer(1, [784, 15, 10]),

    B = parser:mnist_get_binaries("../data/mnist/t10k-images.idx3-ubyte",
                                  "../data/mnist/t10k-labels.idx1-ubyte"),
    mnist_run(Net, B, 1).

%% @doc
%% Run MNIST tests.
%%   Net - neuronet,
%%   B - binary data,
%%   C - current number.
mnist_run(Net, B, C) ->

    % Get next data.
    case parser:mnist_get_next(B) of
        {{I, L}, Rest} ->
            S = nnet:sense_forward(Net, I),
            io:format("case ~w : label = ~w (sense = ~w)~n", [C, L, S]),
            mnist_run(Net, Rest, C + 1);
        _ ->
            Net ! stop
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Start function.
start() ->
    test_1_run().

%---------------------------------------------------------------------------------------------------
