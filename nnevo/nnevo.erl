%% @doc
%% Main module.

% Module name.
-module(nnevo).

-export([start/0, mnist_run/3]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Run MNIST tests.
%%   Net - neuronet,
%%   B - binary data,
%%   C - current number.
mnist_run(Net, B, C) ->

    % Get next data.
    case parser:mnist_get_next(B) of
        {{_I, L}, Rest} ->
            io:format("case ~w : label = ~w~n", [C, L]),
            mnist_run(Net, Rest, C + 1);
        _ ->
            ok
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Start function.
start() ->
    Net = nnet:create_multilayer(1, [2, 2]),
    B = parser:mnist_get_binaries("../data/mnist/t10k-images.idx3-ubyte",
                                  "../data/mnist/t10k-labels.idx1-ubyte"),
    mnist_run(Net, B, 1),
    halt().

%---------------------------------------------------------------------------------------------------
