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
            // Update value only if the key "b" exists
            // state, Reply(oldValue)
            state, Reply(state')

        | None ->
            // state, Reply(0)
            state, Reply(state')


printfn "WE ARE HERE!"

GenServer.info server handlePrintState

GenServer.info server2 handlePrintState

GenServer.cast server (handleIncrement 0)

GenServer.cast server2 (handleIncrement 5000)


printfn "%A" (GenServer.call server (handleCall "a"))

printfn "%A" (GenServer.call server2 (handleCall "a"))


// vale |> Map.iter (fun key value -> Console.WriteLine(sprintf "Key: %s, Value: %i" key value))

// Example usage
// let server = GenServer.start 0

// // Define functions for handling cast and info messages
// let handleIncrement state = { state with counter = state.counter + 1 }
// let handlePrint state = printfn "Counter: %d" state.counter

// // Send cast message using custom handler function
// GenServer.cast server handleIncrement

// // Send info message using custom handler function
// GenServer.info server handlePrint

// // Make a call to GenServer and handle the result

// let res = GenServer.call server (fun state -> 
//     let tempVal = state.counter + 5

//     state, tempVal
// )

// printfn "%i" res

// let responseChannel = GenServer.call server (fun state _ -> state, "Result")
// let result = Async.RunSynchronously responseChannel
// printfn "Received result: %A" result
