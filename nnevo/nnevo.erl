%% @doc
%% Main module.

% Module name.
-module(nnevo).

-export([start/0,
         test_run/0, mnist_run/0]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Test run.
test_run() ->
    Net = nnet:create_multilayer(1, [2, 2, 2]),
    test_run(Net).

%% @doc
%% Test run.
%%   Net - neuronet.
test_run(Net) ->
    I = [0.2, 0.3],
    S = nnet:sense(Net, I),
    io:format("result : ~p~n", [S]).

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
            S = nnet:sense(Net, I),
            io:format("case ~w : label = ~w (sense = ~w)~n", [C, L, S]),
            mnist_run(Net, Rest, C + 1);
        _ ->
            ok
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Start function.
start() ->
    test_run(),
    halt().

%---------------------------------------------------------------------------------------------------
