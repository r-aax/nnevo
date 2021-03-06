%% @doc
%% Neuronet realization.

% Module name.
-module(nnet).

-include("nnet.hrl").
-include("neuron.hrl").
-include("defines.hrl").

-export([create/5, create_multilayer/2,
         loop/1,
         sense_forward/2, sense_back/2,
         act/2,
         correct_weights_and_biases/2,
         print/1]).

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
        neurons = Neurons,
        fps = utils:nones_signals(FLayer),
        lps = utils:nones_signals(LLayer),
        source = none
    },
    Pid = spawn(?MODULE, loop, [State]),
    register(Atom, Pid),

    % Set sources to first layer.
    lists:foreach
    (
        fun(FLE) ->
            FLE ! {add_src, Pid, ?INI_WEIGHT}
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
           lists:duplicate(NeuronsCount, ?INI_BIAS),
           lists:nth(1, Layers),
           lists:last(Layers),
           utils:multilayer_nnet_edges(Layers)).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Neuronet infinite loop.
%%   State - state.
loop(#nnet_state{atom = Atom,
                 fps = FPS,
                 lps = LPS,
                 source = Source} = State) ->

    % Listen.
    receive

        % Sense from outer world in the forward direction.
        {sense_forward, From, Signal} ->
            utils:send_array_to_array(forward, Signal, FPS),
            loop(State#nnet_state{source = From});

        % Sennse from outer world in the back direction.
        {sense_back, From, Signal} ->
            utils:send_array_to_array(back, Signal, LPS),
            loop(State#nnet_state{source = From});

        % Forward propagation from the last layer.
        {forward, From, Signal} ->

            NewLPS = utils:insert_signal(LPS, From, Signal),
            IsSignalsReady = utils:is_signals_ready(NewLPS),

            if
                IsSignalsReady ->
                    {_, Res} = lists:unzip(NewLPS),
                    Source ! {response, self(), Res},
                    loop(State#nnet_state{source = none, lps = utils:nones_signals(LPS)});

                true ->
                    loop(State#nnet_state{lps = NewLPS})
            end;

        % Back propagation from the first layer.
        {back, From, Signal} ->

            NewFPS = utils:insert_signal(FPS, From, Signal),
            IsSignalsReady = utils:is_signals_ready(NewFPS),

            if
                IsSignalsReady ->
                    {_, Res} = lists:unzip(NewFPS),
                    Source ! {response, self(), Res},
                    loop(State#nnet_state{source = none, fps = utils:nones_signals(FPS)});

                true ->
                    loop(State#nnet_state{fps = NewFPS})
            end;

        % Act.
        {act, From, F} ->
            lists:foreach
            (
                fun(Neuron) ->
                    Neuron ! {act, self(), F},
                    receive
                        {response, Neuron, Res} ->
                            Res
                    end
                end,
                State#nnet_state.neurons
            ),
            From ! {response, self(), ok},
            loop(State);

        % Stop command.
        stop ->
            lists:foreach(fun(Neuron) -> Neuron ! stop end, State#nnet_state.neurons);

        % Unknown command.
        Any ->
            io:format("~w: unknown command ~w~n", [Atom, Any])
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Sync call.
%%   Atom - atom,
%%   Pid - process id,
%%   Data - data.
sync_call(Atom, Pid, Data) ->
    Pid ! {Atom, self(), Data},
    receive
        {response, Pid, Res} ->
            Res
    end.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Sense neuronet in forward direction.
%%   Pid - process id,
%%   Signal - signal.
sense_forward(Pid, Signal) ->
    sync_call(sense_forward, Pid, Signal).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Sense neuronet in back direction.
%%   Pid - process id,
%%   Signal - signal.
sense_back(Pid, Signal) ->
    sync_call(sense_back, Pid, Signal).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Act.
act(Pid, F) ->
    sync_call(act, Pid, F).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Correct weights and biases.
%%   Net - neuronet,
%%   Eta - learning speed.
correct_weights_and_biases(Net, Eta) ->
    F =
        fun(#neuron_state{s = S, weights = W, bias = B, e = E} = State) ->
            DW = lists:map(fun(S1) -> S1 * E end, S),
            DB = E,
            NW = lists:zipwith(fun(W1, DW1) -> W1 + Eta * DW1 end, W, DW),
            NB = B + Eta * DB,
            State#neuron_state{weights = NW, bias = NB}
        end,
    act(Net, F).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Print.
%%   Net - neuronet.
print(Net) ->
    F =
        fun(#neuron_state{atom = Atom, weights = Weights, bias = Bias} = State) ->
            io:format("~w : w = ~w, b = ~w~n", [Atom, Weights, Bias]),
            State
        end,
    act(Net, F).

%---------------------------------------------------------------------------------------------------
