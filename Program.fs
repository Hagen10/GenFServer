open GenServer
open System.Threading
open System

let state = State(Map ["a", 0; "b", 0; "c", 0])
let state2 = State(Map ["a", 0; "b", 0; "c", 0])

let server = GenServer.start state
let server2 = GenServer.start state2


let handleIncrement (delay : int) state  =
    match state with
    | State state' -> 
        match Map.tryFind "b" state' with
        | Some oldValue ->
            // Update value only if the key "b" exists
            Thread.Sleep(delay)

            let newValue = oldValue + 10
            let res = state' |> Map.add "b" newValue
            State(res)
        | None ->
            State(state')

let handlePrintState state =
    match state with
    | State state' -> state' |> Map.iter (fun key value -> Console.WriteLine(sprintf "Key: %s, Value: %i" key value))

let handleCall key state =
    match state with
    | State state' -> 
        match Map.tryFind key state' with
        | Some oldValue ->
            state, Int(oldValue)

        | None ->
            state, Int 0


GenServer.info server handlePrintState

GenServer.info server2 handlePrintState

GenServer.cast server (handleIncrement 0)

GenServer.cast server2 (handleIncrement 5000)

printfn "%A" (GenServer.call server (handleCall "a"))

printfn "%A" (GenServer.call server2 (handleCall "b"))



let listener = Listener.startListener()

let stateUpdater = GenServer.start (State(listener, 100))

let updateState state = 
    match state with
    | State(listener, value) -> 
        let updatedValue = value - 1
        Listener.messageUpdated listener
        State(listener, updatedValue)


let getState state =
    match state with
    | State(listener, value) -> state, Int(value)

// let updater = GenServer.start

GenServer.cast stateUpdater updateState

printfn "state status %A" (GenServer.call stateUpdater getState)

GenServer.cast stateUpdater updateState

printfn "state status %A" (GenServer.call stateUpdater getState)

GenServer.cast stateUpdater updateState

GenServer.cast stateUpdater updateState

printfn "state status %A" (GenServer.call stateUpdater getState)

