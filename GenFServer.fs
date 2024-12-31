module GenFServer

type CommunicationType =
    | Cast of obj
    | Call of AsyncReplyChannel<obj> * obj
    | Info of obj

type Server = MailboxProcessor<CommunicationType>

[<AbstractClass>]
type GenFServer<'T>(state) as this =
    let rec agentLoop (state': 'T) (inbox': Server) =
        async {
            let! msg = inbox'.Receive()

            match msg with
            | Cast request ->
                let newState = this.handleCast request state'
                return! agentLoop newState inbox'
            | Info request ->
                let newState = this.handleInfo request state'
                return! agentLoop newState inbox'
            | Call(replyChannel, request) ->
                let reply, newState = this.handleCall request state'
                replyChannel.Reply(reply)
                return! agentLoop newState inbox'
        }

    let server = MailboxProcessor.Start(fun inbox -> agentLoop state inbox)

    abstract member handleCast: obj -> 'T -> 'T
    abstract member handleCall: obj -> 'T -> obj * 'T
    abstract member handleInfo: obj -> 'T -> 'T

    // member private server = MailboxProcessor.Start(fun inbox ->
    //     let rec agentLoop' (state' : 'T) (inbox' : Server) =
    //         async {
    //             let! msg = inbox'.Receive()
    //             match msg with
    //             | Cast request ->
    //                 let newState = this.handleCast request state'

    //                 printfn "NEW STATE %A" newState
    //                 return! agentLoop' newState inbox'
    //             | Info request ->
    //                 let newState = this.handleInfo request state'
    //                 return! agentLoop' newState inbox'
    //             | Call(replyChannel, request)  ->
    //                 let reply, newState = this.handleCall request state'

    //                 printfn "reply: %A CALL STATE %A" reply newState

    //                 replyChannel.Reply(reply)
    //                 return! agentLoop' newState inbox'
    //         }
    //     agentLoop' state inbox
    // )


    // member this.agentLoop (state: 'T) (inbox: Server) =
    //     printfn "Starting agentloop"
    //     let rec agentLoop' (state' : 'T) (inbox' : Server) =
    //         async {
    //             let! msg = inbox'.Receive()
    //             match msg with
    //             | Cast request ->
    //                 let newState = this.handleCast request state'

    //                 printfn "NEW STATE %A" newState
    //                 return! agentLoop' newState inbox'
    //             | Info request ->
    //                 let newState = this.handleInfo request state'
    //                 return! agentLoop' newState inbox'
    //             | Call(replyChannel, request)  ->
    //                 let reply, newState = this.handleCall request state'

    //                 printfn "reply: %A CALL STATE %A" reply newState

    //                 replyChannel.Reply(reply)
    //                 return! agentLoop' newState inbox'
    //         }
    //     agentLoop' state inbox


    // member createServer () =
    //     MailboxProcessor.Start(fun inbox -> this.agentLoop state inbox)

    // member val server = lazy (createServer ())

    member this.Cast request = server.Post(Cast request)

    member this.Call request =
        server.PostAndReply(fun ((replyChannel: AsyncReplyChannel<obj>)) -> Call(replyChannel, request))

    member this.Info request = server.Post(Info request)
