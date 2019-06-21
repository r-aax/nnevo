%% @doc
%% Extended lists functions.

% Module name.
-module(xlists).

-export([find_first_index/2, replace/3, filter_map/3]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Find first index of element E for which F(E) == true.
find_first_index([], _) ->
    -1;
find_first_index(L, F) ->
    find_first_index(L, F, 1).

%% @doc
%% Find first index of element E for which F(E) == true.
find_first_index([H | T], F, I) ->
    R = F(H),
    if
        R ->
            I;
        true ->
            find_first_index(T, F, I + 1)
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Replace element.
replace(L, I, V) ->
    {L1, [_ | L2]} = lists:split(I - 1, L),
    lists:append([L1, [V], L2]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Map with filter.
filter_map(L, FF, MF) ->
    ReplaceFun =
        fun(X) ->
            Is = FF(X),
            if
                Is ->
                    MF(X);
                true ->
                    X
            end
        end,
    [ReplaceFun(X) || X <- L].

%---------------------------------------------------------------------------------------------------
