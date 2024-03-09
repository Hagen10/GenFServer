module GenServer

    type State = {
        mutable counter: int
    }

    type Message =
        | Cast of (State -> State)
        | Info of (State -> unit)
        | Call of (AsyncReplyChannel<obj> * (State -> State * int))
        // | Call of (MailboxProcessor<obj> * (State -> State * obj))
        // | Call of (State -> 'T -> State * 'T)

    type Server = MailboxProcessor<Message>

    let rec agentLoop (state: State) (inbox: Server) =
        async {
            let! msg = inbox.Receive()
            match msg with
            | Cast handler ->
                let newState = handler state
                return! agentLoop newState inbox
            | Info handler ->
                handler state
                return! agentLoop state inbox
            | Call(replyChannel, cont)  ->
                let newState, res = cont state
                replyChannel.Reply(res)
                return! agentLoop newState inbox
            // | Call cont -> 
            //     let newState, res = cont state
            //     inbox.Reply res
            //     return! agentLoop newState inbox
            // | Call (replyChannel, cont) -> 
            //     let newState, res = cont state
            //     replyChannel.Post(res)
            //     return! agentLoop newState inbox
        }

    let start initialState =
        let state = { counter = initialState }
        MailboxProcessor.Start(fun inbox -> agentLoop state inbox)

    let cast (server : Server) handler =
        server.Post(Cast handler)

    let info (server : Server) handler =
        server.Post(Info handler)

    let call<'T> (server : Server) (cont: State -> State * int) =
        // let replyChannel : MailboxProcessor<obj> = MailboxProcessor.Start(fun inbox ->
        //     async {
        //         let! response = inbox.Receive()
        //         return response
        //     })
        // server.Post(Call(replyChannel, cont))

        server.PostAndReply(fun replyChannel -> Call(replyChannel, cont))

        // Post the continuation function along with the reply channel
        // server.Post(Call (fun state result -> 
        //     // Send the response back to the reply channel
        //     replyChannel.Post result
        //     // Return the new state
        //     cont state result))
        // Async.RunSynchronously replyChannel

        // let result =
        //     async {
        //         let! response = replyChannel
        //         return response
        //     } |> Async.RunSynchronously

        // result