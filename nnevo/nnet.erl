%% @doc
%% Neuronet realization.

% Module name.
-module(nnet).

-include("nnet.hrl").

-export([create/5, create_multilayer/2,
         loop/1,
         sense/2]).

%---------------------------------------------------------------------------------------------------
% Functions.
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
    NeuronsNumbers = lists:seq(1, NeuronsCount),
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
            SrcPid = lists:nth(Src, Neurons),
            DstPid = lists:nth(Dst, Neurons),
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
%% Create multilayer net.
%%   NNN - neuronet number,
%%   Layers - list of layers sizes.
create_multilayer(NNN, Layers) ->

    % Neurons count is sum of neurons in all layers.
    NeuronsCount = lists:sum(Layers),

    % The first layer size and the last layers size are
    % the first and the last elements of the Layers list.

    % Main constructor.
    create(NNN,
           lists:duplicate(NeuronsCount, 1.0),
           lists:nth(1, Layers),
           lists:last(Layers),
           utils:multilayer_nnet_edges(Layers)).

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
