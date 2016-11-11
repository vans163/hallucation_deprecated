## hallucation
<img src="http://i.imgur.com/iTk2iiL.png" />

## <img src="http://i.imgur.com/cNoYlvo.png" width="250" height="250" /> Experimental

## Description

hallucation is a one way state differential for reactive clients.  
State flows from your data, to hallucation, to your front end. 
It cannot flow the other way.  
  
hallucation is meant to solve the problem of complexity in state management.  
  
hallucation does this by removing the need for a Request/Response based interface,
instead you only send Request and the Response comes in the form of a state differential.

## How to use hallucation

To use hallucation you probably want a reactive frontend, such as React.js.  Where
any changes to the state will automatically trigger a rerendering.  
  
Some questions around how to receive one time events arise, as well as the possibility to ACK a request.  Suggestions are welcome.

## How hallucation works

Uses gen_statem under the hood  
Start off with an initial state  
Subscribe/+Expose functions that mutate state  
Every 200ms OldState and NewState produce diff  
Your backend now decides how to send this diff to peer  

NOTE:  
Every state is hashed with erlang:phash2/1, this makes it possible to restart
a session from a disconnection.

## Limitations

Erlang R19.1+  
State MUST be a Map  
State can only contain types atom, integer, float, binary and map  

## Example

You have a module 'ws_api_actions' which has functions like update_email/1.  
You receive a message from the client over websockets in json such as  
```
{"opcode": "update_email", "email": "new_email@example.com"}
```

You call update_email/1 in 'ws_api_actions' which may look like  
```erlang
update_email(UserId, NewEmail) ->
    mnesia:activity(transaction, fun()-> 
        [User] = mnesia:wread({user, UserId}), 
        ok = mnesia:write(User#user{email=NewEmail}) 
    end).
```

Your hallucation behavior can now look like  


```erlang
hallucation:start(example_hallucation, {WSPid, uuid_1234})


-module(example_hallucation).
-behavior(hallucation).


init({WSPid, Uuid}) ->
    add_mnesia_subscription(user),
    InitialPublicState = initial_state(Uuid),
    InitialPrivateState = #{wspid=> WSPid, uid=> Uuid},
    { InitialPublicState, InitialPrivateState }.

state_changed(NewStateHash, NewState, Diff, PrivState) ->
    WSPid = maps:get(wspid, PrivState),
    stargate_plugin:ws_send(WSPid, {text, jsx:encode(#{hash=>NewStateHash, diff=>Diff})}).



% =================
% Private Functions
% =================

add_mnesia_subscription(Table) -> 
    {ok, _} = mnesia:subscribe({table, Table, detailed}).

initial_state(Uuid) ->
    [User] = mnesia:dirty_read({user, Uuid}),
    #{user=> #{email=> User#user.email}}.

%Updated user
handle_info({delete, user, What, _, _}, {State, PrivState}) ->
    {state_update, maps:remove(user, State), PrivState}.

handle_info({write, user, NewRecord, [OldRecord], _}, {State, PrivState}) ->

    NewState =
    (fun C([], NS) -> NS;
         C([{K,V}|T], NS) -> C(T, NS);
         C([{email,V}|T], NS) ->
            User = maps:get(user, NS),
            C(T, NS#{user=> User#{email=V}});

    )(
        diff_record(NewRecord, OldRecord), 
        State
    ),

    {state_update, NewState, PrivState};

handle_info(Term, {State, PrivState}) -> {ok, PrivState}.
```
