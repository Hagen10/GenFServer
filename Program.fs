open GenServer
open Listener
open StateHandler

// Listener will not be called directly but instead passed to the updater GenServer which will call one of its functions every time its state has been updated
let listener = new Listener(State("listener", 0))
let updater = new StateHandler(State(Map ["a", 0; "b", 0; "c", 0]), listener)

updater.PrintState()

updater.UpdateState "a" 5

let value = updater.GetValue "a"

printfn "should be an Int: %A" value

let value2 = updater.GetValue "d"

printfn "Should be None: %A" value2

updater.UpdateState "c" 17

updater.PrintState()

