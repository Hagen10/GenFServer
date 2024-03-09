open GenServer

// Example usage
let server = GenServer.start 0

// Define functions for handling cast and info messages
let handleIncrement state = { state with counter = state.counter + 1 }
let handlePrint state = printfn "Counter: %d" state.counter

// Send cast message using custom handler function
GenServer.cast server handleIncrement

// Send info message using custom handler function
GenServer.info server handlePrint

// Make a call to GenServer and handle the result

let res = GenServer.call server (fun state -> 
    let tempVal = state.counter + 5

    state, tempVal
)

printfn "%A" res

// let responseChannel = GenServer.call server (fun state _ -> state, "Result")
// let result = Async.RunSynchronously responseChannel
// printfn "Received result: %A" result
