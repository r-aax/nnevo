%% @doc
%% Input data parser.

% Module name.
-module(parser).

-include("defines.hrl").

-export([mnist_open_files/2, mnist_get_next/1]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Open MNIST files.
%%   ImagesFile - images file,
%%   LabelsFile - labels file.
mnist_open_files(ImagesFile, LabelsFile) ->

    % Open files.
    ?OK(ImagesBin) = file:read_file(ImagesFile),
    ?OK(LabelsBin) = file:read_file(LabelsFile),

    % Cut off the heads.
    <<16#803:32/unsigned-big-integer,
      ImagesCount:32/unsigned-big-integer,
      ImagesBinRest/binary>> = ImagesBin,
    <<16#801:32/unsigned-big-integer,
      LabelsCount:32/unsigned-big-integer,
      LabelsBinRest/binary>> = LabelsBin,

    % Check count of images and labels.
    ImagesCount = LabelsCount,

    % Close fils.
    file:close(ImagesBin),
    file:close(LabelsBin),

    % Count of cases and binaries.
    {ImagesCount, ImagesBinRest, LabelsBinRest}.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Get next cases of MNIST.
%%   N - count of cases,
%%   ImagesBin - images binary,
%%   LabelsBin - labels binary.
mnist_get_next({_N, _ImagesBin, _LabelsBin}) ->
    ok.

%---------------------------------------------------------------------------------------------------
