%% @doc
%% Neuronet realization.

% Module name.
-module(nnet).

-include("nnet.hrl").

-export([create/1, create/5, loop/1,
         sense/2]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Create neuronnet.
%%   NNN - neuronert number.
create(NNN) ->
    Atom = utils:nnet_atom(NNN),

    % Test neurons.
    %
    %    --->N00---->N10---
    %   /       \   /      \
    %  S          X         A
    %   \       /   \      /
    %    --->N01---->N11---
    %
    N00 = neuron:create(NNN, 0, [1], 2),
    N01 = neuron:create(NNN, 1, [1], 2),
    N10 = neuron:create(NNN, 2, [1, 1], 2),
    N11 = neuron:create(NNN, 3, [1, 1], 2),

    LLayer = [N10, N11],
    State = #nnet_state
    {
        atom = Atom,
        flayer = [N00, N01],
        llayer = LLayer,
        source = none,
        osignals = utils:nones(LLayer)
    },
    Pid = spawn(?MODULE, loop, [State]),
    register(Atom, Pid),

    % Set pids.
    N00 ! {set_pids, [Pid], [N10, N11]},
    N01 ! {set_pids, [Pid], [N10, N11]},
    N10 ! {set_pids, [N00, N01], [Pid]},
    N11 ! {set_pids, [N00, N01], [Pid]},

    Pid.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Create neuronet from skeleton.
%%   NNN - neuronet number,
%%   Biases - biases list,
%%   FLayerSize - first layer size,
%%   LLayerSize - last layer size,
%%   Edges - edges list.
create(NNN, Biases, FLayerSize, LLayerSize, Edges) ->
    Atom = utils:nnet_atom(NNN),

    % Create neurons.
    NeuronsCount = length(Biases),
    NeuronsNumbers = lists:seq(0, NeuronsCount - 1),
    Neurons =
        lists:map
        (
            fun({NN, B}) ->
                % Just create neurons without links.
                neuron:create(NNN, NN, [], B)
            end,
            lists:zip(NeuronsNumbers, Biases)
        ),

    % Create first and last layers.
    {FLayer, _} = lists:split(FLayerSize, Neurons),
    LLayer = lists:nthtail(NeuronsCount - LLayerSize, Neurons),

    % Spawn process.
    State = #nnet_state
    {
        atom = Atom,
        flayer = FLayer,
        llayer = LLayer,
        source = none,
        osignals = utils:nones(LLayer)
    },
    Pid = spawn(?MODULE, loop, [State]),
    register(Atom, Pid),

    % Set sources to first layer.
    lists:foreach
    (
        fun(FLE) ->
            FLE ! {add_src, Pid, 1.0}
        end,
        FLayer
    ),

    % Set inner edges.
    lists:foreach
    (
        fun({Src, Dst, W}) ->
            SrcPid = lists:nth(Src + 1, Neurons),
            DstPid = lists:nth(Dst + 1, Neurons),
            SrcPid ! {add_dst, DstPid},
            DstPid ! {add_src, SrcPid, W}
        end,
        Edges
    ),

    % Set destinations for last layer.
    lists:foreach
    (
        fun(LLE) ->
            LLE ! {add_dst, Pid}
        end,
        LLayer
    ),

    % Return pid.
    Pid.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Neuronet infinite loop.
%%   State - state.
loop(#nnet_state{atom = Atom,
                 flayer = FLayer,
                 llayer = LLayer,
                 source = Source,
                 osignals = OSignals} = State) ->

    % Listen.
    receive

        % Sense from outer world.
        {out_sense, From, Signal} ->
            utils:send_array_to_array(Signal, FLayer),
            loop(State#nnet_state{source = From});

        % Sense from last layer.
        {sense, From, Signal} ->

            NewOSignals = utils:insert_signal(OSignals, Signal, LLayer, From),
            IsSignalsReady = utils:is_signals_ready(NewOSignals),

            if
                IsSignalsReady ->
                    Source ! {response, self(), NewOSignals},
                    loop(State#nnet_state{source = none, osignals = utils:nones(LLayer)});

                true ->
                    loop(State#nnet_state{osignals = NewOSignals})
            end;

        % Stop command.
        stop ->
            %% @todo
            %% We must stop all neurons.
            ok;

        % Unknown command.
        % Neuron dies.
        Any ->
            io:format("~w: unknown command ~w~n", [Atom, Any])
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Sense neuronet.
%%   Pid - process id,
%%   Signal - signal.
sense(Pid, Signal) ->
    Pid ! {out_sense, self(), Signal},
    receive
        {response, Pid, Out} ->
            Out
    end.

%---------------------------------------------------------------------------------------------------
