module GenServer

    type State<'T> = State of 'T

    type Reply<'T> =
        | Reply of 'T

    type Message<'T> =
        | Cast of (State<'T> -> State<'T>)
        | Info of (State<'T> -> unit)
        | Call of (AsyncReplyChannel<obj> * (State<'T> -> State<'T> * Reply<'T>))

    type Server<'T> = MailboxProcessor<Message<'T>>

    let rec agentLoop (state: State<'T>) (inbox: Server<'T>) =
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
        MailboxProcessor.Start(fun inbox -> agentLoop initialState inbox)

    let cast (server : Server<'T>) handler =
        server.Post(Cast handler)

    let info (server : Server<'T>) handler =
        server.Post(Info handler)

    let call<'T> (server : Server<'T>) (cont: State<'T> -> State<'T> * Reply<'T>) =
        server.PostAndReply(fun (replyChannel) -> Call(replyChannel, cont))
