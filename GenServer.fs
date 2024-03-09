module GenServer

    type State = {
        mutable counter: int
    }

    type Message =
        | Cast of (State -> State)
        | Info of (State -> unit)
        | Call of (AsyncReplyChannel<obj> * (State -> State * int))

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
        }

    let start initialState =
        let state = { counter = initialState }
        MailboxProcessor.Start(fun inbox -> agentLoop state inbox)

    let cast (server : Server) handler =
        server.Post(Cast handler)

    let info (server : Server) handler =
        server.Post(Info handler)

    let call<'T> (server : Server) (cont: State -> State * int) =

        server.PostAndReply(fun replyChannel -> Call(replyChannel, cont))
