%% @doc
%% Main module.

% Module name.
-module(nnevo).

-include("neuron.hrl").
-include("defines.hrl").

-export([start/0,
         single_learn/3,
         test_1_run/0, test_2_run/0, test_5_run/0,
         test_mnist_1_run/0,
         mnist_run/0]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Single learn.
%%   Net - neuronet,
%%   X - input vector,
%%   Y - output vector (right).
single_learn(Net, X, Y) ->
    Ms0 = utils:ms(),
    A = nnet:sense_forward(Net, X),
    C = utils:cost(Y, A),

    if
        C < 0.00001 ->
            %io:format("X = ~w, Y = ~w, A = ~w~n", [X, Y, A]),
            io:format("single_learn : learning is finished (cost = ~w)~n", [C]),
            nnet:print(Net),
            halt();

        true ->
            %io:format("X = ~w, Y = ~w, A = ~w~n", [X, Y, A]),
            io:format("single_learn : cost = ~w~n", [C]),
            nnet:sense_back(Net, lists:zipwith(fun(Y1, A1) -> Y1 - A1 end, Y, A)),
            nnet:correct_weights_and_biases(Net, ?LEARNING_SPEED_TAU),
            Ms1 = utils:ms(),
            io:format("               iter time = ~w ms~n", [Ms1 - Ms0]),
            single_learn(Net, X, Y)
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Test run.
test_1_run() ->
    single_learn(nnet:create_multilayer(1, [1, 1, 1]),
                 [1.0],
                 [0.1]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Test run.
test_2_run() ->
    single_learn(nnet:create_multilayer(1, [2, 2, 2]),
                 [1.0, 1.0],
                 [0.5, 0.3]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Test run.
test_5_run() ->
    single_learn(nnet:create_multilayer(1, [5, 5, 5, 5, 5]),
                 [1.0, 2.0, 3.0, 4.0, 5.0],
                 [0.9, 0.8, 0.7, 0.6, 0.5]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Test run.
test_mnist_1_run() ->
    Net = nnet:create_multilayer(1, [784, 15, 10]),
    B = parser:mnist_get_binaries("../data/mnist/t10k-images.idx3-ubyte",
                                  "../data/mnist/t10k-labels.idx1-ubyte"),
    {{I, L}, _} = parser:mnist_get_next(B),
    single_learn(Net,
                 lists:map(fun(X) -> X / 255.0 end, I),
                 parser:mnist_label_to_prob(L)).

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
    test_mnist_1_run().

%---------------------------------------------------------------------------------------------------
