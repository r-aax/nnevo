%% @doc
%% Input data parser.

% Module name.
-module(parser).

-include("defines.hrl").

%% @doc
%% Digit picture size from MNIST (28 * 28 = 784).
-define(MNIST_DIGIT_PICTURE_SIZE, 784).

%% @doc
%% Unsigned integer of 32 bits (big endian).
-define(LONG, 32/unsigned-big-integer).

%% @doc
%% Unsigned integer of 8 bits (big endian).
-define(BYTE, 8/unsigned-big-integer).

-export([mnist_get_binaries/2, mnist_get_next/1]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Open MNIST files and extract binaries.
%%   ImagesFile - images file,
%%   LabelsFile - labels file.
mnist_get_binaries(ImagesFile, LabelsFile) ->

    % Open files.
    ?OK(ImagesBin) = file:read_file(ImagesFile),
    ?OK(LabelsBin) = file:read_file(LabelsFile),

    % Cut off the heads.
    <<16#803:?LONG, ImagesCount:?LONG, ImagesBinRest/binary>> = ImagesBin,
    <<16#801:?LONG, LabelsCount:?LONG, LabelsBinRest/binary>> = LabelsBin,

    % Check count of images and labels.
    ImagesCount = LabelsCount,

    % Close fils.
    file:close(ImagesBin),
    file:close(LabelsBin),

    % Count of cases and binaries.
    {ImagesCount, ImagesBinRest, LabelsBinRest}.

%---------------------------------------------------------------------------------------------------

%% @doc

%---------------------------------------------------------------------------------------------------

%% @doc
%% Get next cases of MNIST.
%%   N - count of cases,
%%   ImagesBin - images binary,
%%   LabelsBin - labels binary.
mnist_get_next({0, _, _}) ->
    none;
mnist_get_next({N,
                <<ImageBin:?MNIST_DIGIT_PICTURE_SIZE/binary, ImagesBinRest/binary>>,
                <<Label:?BYTE, LabelsBinRest/binary>>}) ->
    {
        {[X || <<X:?BYTE>> <= ImageBin], Label},
        {N - 1, ImagesBinRest, LabelsBinRest}
    }.

%---------------------------------------------------------------------------------------------------
