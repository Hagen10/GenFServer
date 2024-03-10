module Listener 

    open GenServer

    type Listener = MailboxProcessor<Message<string * int>>

    let startListener () =
        let state = State("Messages received count", 0)

        GenServer.start state

    let messageUpdated (listener : Listener) =
        
        let func (state : State<string * int>) =
            match state with
                | State(msg, int) -> 
                    let res = int + 1
                    printfn "State updated %i times" res
                    State(msg, res)

        GenServer.cast listener func