%% @doc
%% Main module.

% Module name.
-module(nnevo).

-include("neuron.hrl").
-include("defines.hrl").

-export([start/0,
         test_1_run/0, test_2_run/0, test_5_run/0,
         test_mnist_1_run/0,
         mnist_run/0]).

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
    Y = [0.1],
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
        C < 0.00001 ->
            io:format("test_1_run : learning is finished (cost = ~w)~n", [C]),
            nnet:print(Net),
            io:format("X = ~w, Y = ~w, A = ~w~n", [X, Y, A]),
            halt();

        true ->
            io:format("test_1_run : cost = ~w~n", [C]),
            nnet:sense_back(Net, lists:zipwith(fun(Y1, A1) -> Y1 - A1 end, Y, A)),
            nnet:correct_weights_and_biases(Net, ?LEARNING_SPEED_TAU),
            test_1_run(Net, X, Y)
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Test run.
test_2_run() ->
    Net = nnet:create_multilayer(1, [2, 2, 2]),
    test_2_run(Net).

%% @doc
%% Test run.
%%   Net - neuronet.
test_2_run(Net) ->
    X = [1.0, 1.0],
    Y = [0.5, 0.3],
    test_2_run(Net, X, Y).

%% @doc
%% Test run.
%%   Net - neuronet,
%%   X - input vector,
%%   Y - output vector (right).
test_2_run(Net, X, Y) ->
    A = nnet:sense_forward(Net, X),
    C = utils:cost(Y, A),

    if
        C < 0.00001 ->
            io:format("test_2_run : learning is finished (cost = ~w)~n", [C]),
            nnet:print(Net),
            io:format("X = ~w, Y = ~w, A = ~w~n", [X, Y, A]),
            halt();

        true ->
            io:format("test_2_run : cost = ~w~n", [C]),
            nnet:sense_back(Net, lists:zipwith(fun(Y1, A1) -> Y1 - A1 end, Y, A)),
            nnet:correct_weights_and_biases(Net, ?LEARNING_SPEED_TAU),
            test_2_run(Net, X, Y)
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Test run.
test_5_run() ->
    Net = nnet:create_multilayer(1, [5, 5, 5, 5, 5]),
    test_5_run(Net).

%% @doc
%% Test run.
%%   Net - neuronet.
test_5_run(Net) ->
    X = [1.0, 2.0, 3.0, 4.0, 5.0],
    Y = [0.9, 0.8, 0.7, 0.6, 0.5],
    test_5_run(Net, X, Y).

%% @doc
%% Test run.
%%   Net - neuronet,
%%   X - input vector,
%%   Y - output vector (right).
test_5_run(Net, X, Y) ->
    A = nnet:sense_forward(Net, X),
    C = utils:cost(Y, A),

    if
        C < 0.00001 ->
            io:format("test_5_run : learning is finished (cost = ~w)~n", [C]),
            nnet:print(Net),
            io:format("X = ~w, Y = ~w, A = ~w~n", [X, Y, A]),
            halt();

        true ->
            io:format("test_5_run : cost = ~w~n", [C]),
            nnet:sense_back(Net, lists:zipwith(fun(Y1, A1) -> Y1 - A1 end, Y, A)),
            nnet:correct_weights_and_biases(Net, ?LEARNING_SPEED_TAU),
            test_5_run(Net, X, Y)
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Test run.
test_mnist_1_run() ->
    Net = nnet:create_multilayer(1, [784, 15, 10]),
    test_mnist_1_run(Net).

%% @doc
%% Test run.
%%   Net - neuronet.
test_mnist_1_run(Net) ->
    B = parser:mnist_get_binaries("../data/mnist/t10k-images.idx3-ubyte",
                                  "../data/mnist/t10k-labels.idx1-ubyte"),
    {{I, L}, _} = parser:mnist_get_next(B),
    test_mnist_1_run(Net,
                     lists:map(fun(X) -> X / 255.0 end, I),
                     parser:mnist_label_to_prob(L)).

%% @doc
%% Test run.
%%   Net - neuronet,
%%   X - input vector,
%%   Y - output vector (right).
test_mnist_1_run(Net, X, Y) ->
    ForwardMs0 = utils:ms(),
    A = nnet:sense_forward(Net, X),
    ForwardMs1 = utils:ms(),
    io:format("                   forward - ~w ms~n", [ForwardMs1 - ForwardMs0]),
    C = utils:cost(Y, A),

    if
        C < 0.00001 ->
            io:format("X = ~w, Y = ~w, A = ~w~n", [X, Y, A]),
            io:format("test_mnist_1_run : learning is finished (cost = ~w)~n", [C]),
            nnet:print(Net),
            halt();

        true ->
            %io:format("X = ~w, Y = ~w, A = ~w~n", [X, Y, A]),
            io:format("test_mnist_1_run : cost = ~w~n", [C]),
            BackMs0 = utils:ms(),
            nnet:sense_back(Net, lists:zipwith(fun(Y1, A1) -> Y1 - A1 end, Y, A)),
            BackMs1 = utils:ms(),
            io:format("                   back    - ~w ms~n", [BackMs1 - BackMs0]),
            CorrectMs0 = utils:ms(),
            nnet:correct_weights_and_biases(Net, ?LEARNING_SPEED_TAU),
            CorrectMs1 = utils:ms(),
            io:format("                   correct - ~w ms~n", [CorrectMs1 - CorrectMs0]),
            test_mnist_1_run(Net, X, Y)
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
    test_mnist_1_run().

%---------------------------------------------------------------------------------------------------
