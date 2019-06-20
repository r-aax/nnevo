%% @doc
%% Genotype implementation.
%%
%% Genotype is just the list containing rules for neuronet generating.

% Module name.
-module(genotype).

-export([empty/3]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Empty genotype.
empty(FirstLayerSize, LastLayerSize, FLLayersConnectionType) ->
    [{genotype, FirstLayerSize, LastLayerSize, FLLayersConnectionType}].

%---------------------------------------------------------------------------------------------------
