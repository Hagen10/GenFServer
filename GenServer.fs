module GenServer

    type State<'T> = State of 'T

    type Reply = 
        | None
        | Int of int

    type Result =
        | OK
        | Error of string

    type Message<'T> =
        | Cast of (State<'T> -> State<'T>)
        | Info of (State<'T> -> unit)
        | Call of (AsyncReplyChannel<Reply> * (State<'T> -> State<'T> * Reply))
        | StateData of (AsyncReplyChannel<State<'T>>)

    type Server<'T> = MailboxProcessor<Message<'T>>

    type Interface3 =
        abstract member Cast: State<'T> -> unit

    [<AbstractClass>]
    type GenServerI<'T>(state) = 
        let rec agentLoop (state: State<'T>) (inbox: Server<'T>) =
            async {
                let! msg = inbox.Receive()
                match msg with
                | Cast handler ->
                    let newState = handler state
                    printfn "NEW STATE %A" newState

                    return! agentLoop newState inbox
                | Info handler ->
                    handler state
                    return! agentLoop state inbox
                | Call(replyChannel, cont)  ->
                    let newState, res = cont state

                    printfn "reply: %A CALL STATE %A" res newState

                    replyChannel.Reply(res)
                    return! agentLoop newState inbox
                | StateData replyChannel  ->
                    replyChannel.Reply(state)
                    return! agentLoop state inbox
            }

        let server = MailboxProcessor.Start(fun inbox -> agentLoop state inbox)

        member this.Cast handler = server.Post(Cast handler)
        member this.Call contFunction =
            server.PostAndReply(fun ((replyChannel : AsyncReplyChannel<Reply>)) -> Call(replyChannel, contFunction))

        member this.Info handler = server.Post(Info handler)

        member this.GetState() = server.PostAndReply(fun replyChannel -> StateData(replyChannel))
