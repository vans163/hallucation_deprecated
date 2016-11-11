-module(hallucation).
-behavior(gen_statem).

-export([behaviour_info/1]).
-export([callback_mode/0, terminate/3, code_change/4, start/2, start_link/2]).
-export([init/1, handle_event/4]).

behaviour_info(callbacks) ->
    [{init, 1}, {state_changed, 4}, {handle_info, 2}];
behaviour_info(_) ->
    undefined.

callback_mode() ->  handle_event_function.

terminate(_R, _S, _D) -> ok.
code_change(_V, S, D, _E) -> {ok, S, D}.

start(HallucationMod, Params) -> start(HallucationMod, Params, start).
start_link(HallucationMod, Params) -> start(HallucationMod, Params, start_link).

start(HallucationMod, Params, StartAtom) ->
    {InitialState, InitialPrivState} = erlang:apply(HallucationMod, init, [Params]),
    case verify_state(InitialState) of
        {error, Err} -> {error, Err};
        ok -> erlang:apply(gen_statem, StartAtom, [
            ?MODULE, {HallucationMod, InitialState, InitialPrivState}, []]
        )
    end.

init({HallucationMod, InitialState, InitialPrivState}) ->
    process_flag(trap_exit, true),

    {ok, 
        initialized,
        #{  
            hallu_mod=> HallucationMod, 
            pstate=> InitialPrivState, 

            old_state=> undefined,
            old_state_hash=> undefined,

            state=> InitialState,
            state_hash=> erlang:phash2(InitialState)
        },
        0
    }.

handle_event(info, Term, initialized, D) ->
    Mod = maps:get(hallu_mod, D),
    PrivState = maps:get(pstate, D),
    State = maps:get(state, D),
    StateHash = maps:get(state_hash, D),

    {D2, Timeout} = case erlang:apply(Mod, handle_info, 
            [Term, {State, PrivState}]) 
    of
        {ok, NewPrivState} -> {D#{pstate=> NewPrivState}, ignore};
        {state_update, NewState, NewPrivState} -> 
            case verify_state(NewState) of
                ok -> ok;
                {error, Err} -> throw({error, Err})
            end,

            NewStateHash = erlang:phash2(NewState),
            case StateHash =:= NewStateHash of
                true -> 
                    {D#{pstate=> NewPrivState}, ignore};
                false -> 
                    {D#{
                        pstate=> NewPrivState, 
                        state=> NewState, 
                        state_hash=> NewStateHash
                        }, 
%TODO: FIX THIS, this will never proc if we get updates every < 200ms
                    200}
            end
    end,

    case Timeout of
        ignore -> {next_state, initialized, D2};
        Time -> {next_state, initialized, D2, Time}
    end;

handle_event(timeout, _, initialized, D) ->
    OldState = maps:get(old_state, D),
    OldStateHash = maps:get(old_state_hash, D),
    State = maps:get(state, D),
    StateHash = maps:get(state_hash, D),

    case OldStateHash == StateHash of
        true -> {next_state, initialized, D};
        false -> 
            HalluMod = maps:get(hallu_mod, D),
            PrivState = maps:get(pstate, D),

            Diff = diff_state(OldState, State),

            erlang:apply(HalluMod, state_changed, [StateHash, State, Diff, PrivState]),
            {next_state, initialized, D#{old_state=> State, old_state_hash=> StateHash}}
    end.

    

% ====================
%  Internal Functions
% ====================

diff_state(Old, New) when Old =:= New -> #{};
diff_state(Old, New) when is_map(Old), is_map(New) == false -> New;
diff_state(Old, New) when is_map(Old) ->
    OldKeys = maps:keys(Old),
    NewKeys = maps:keys(New),

    OldMinusNew = OldKeys -- NewKeys,
    Map1 = lists:foldl(fun(Key, Acc) ->
            Acc#{Key=> 'zxcye23_delete_4334de'}
        end, #{}, OldMinusNew 
    ),

    NewMinusOld = NewKeys -- OldKeys,
    Map2 = lists:foldl(fun(Key, Acc) ->
            Acc#{Key=> maps:get(Key, New)}
        end, #{}, NewMinusOld 
    ),

    RemainingKeys = NewKeys -- NewMinusOld,
    Map3 = lists:foldl(fun(Key, Acc) ->
            OldVal = maps:get(Key, Old),
            NewVal = maps:get(Key, New),
            case OldVal =:= NewVal of
                true -> Acc;
                false ->
                    Acc#{ Key=> diff_state(OldVal, NewVal) }
            end
        end, #{}, RemainingKeys
    ),
    maps:merge(maps:merge(Map1, Map2), Map3);

diff_state(Old, New) when Old =/= New -> New.



verify_state(State) when is_atom(State) -> ok;
verify_state(State) when is_binary(State) -> ok;
verify_state(State) when is_integer(State) -> ok;
verify_state(State) when is_float(State) -> ok;

verify_state(State) when is_map(State) -> 
    try
        lists:foreach(fun(Key) ->
                ok = verify_state(Key),
                Val = maps:get(Key, State),
                ok = verify_state(Val)
            end, maps:keys(State)
        )
    catch
        _E:_R -> {error, {state_invalid_type, State}}
    end;

verify_state(State) -> {error, {state_invalid_type, State}}.