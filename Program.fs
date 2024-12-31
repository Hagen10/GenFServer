open GenServer
open Listener
open StateHandler
open TestServer
open System.Threading

// Listener will not be called directly but instead passed to the updater GenServer which will call one of its functions every time its state has been updated
let listener = new Listener(State("listener", 0))
let updater = new StateHandler(State(Map ["a", 0]), listener)

updater.PrintState()
Thread.Sleep(100)
updater.UpdateState "a" 1
Thread.Sleep(100)
updater.PrintState()
Thread.Sleep(100)
updater.UpdateState "a" 0
Thread.Sleep(100)
updater.PrintState()

let server = new TestServer({count = 0; other = "hello"})

printfn "STARTING \n"
printfn "RES: %A" (server.Call GetCount)
Thread.Sleep(100)
server.Cast IncrCount
Thread.Sleep(100)
printfn "RES: %A" (server.Call GetCount)
Thread.Sleep(100)
server.Cast DecrCount
Thread.Sleep(100)
printfn "RES: %A" (server.Call GetCount)