module Listener 
    open GenServer

    type Listener(state) =
        inherit GenServerI<string * int>(state)

        member this.StateUpdated() =
            let handler (state : State<string * int>) =
                match state with
                    | State(msg, int) -> 
                        let res = int + 1
                        printfn "State updated %i times" res
                        State(msg, res)

            this.Cast handler