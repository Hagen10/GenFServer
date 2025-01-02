module GenFServer

open System.Threading

type CommunicationType =
    | Cast of obj
    | Call of AsyncReplyChannel<obj> * obj
    | AsyncCall of AsyncReplyChannel<obj> * obj
    | Info of obj

type Server = MailboxProcessor<CommunicationType>

[<AbstractClass>]
type GenFServer<'T>(state) as this =
    let rec agentLoop (state': 'T) (inbox': Server) =
        async {
            try
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
                | AsyncCall(replyChannel, request) ->
                    let reply, newState = this.handleCall request state'
                    replyChannel.Reply(reply)
                    return! agentLoop newState inbox'
            with ex -> 
                printfn "ENCOUNTERED AN EXCEPTION %s STATE WAS %A" ex.Message state'
                // raise ex
                return! agentLoop state' inbox'
        }

    let server = MailboxProcessor.Start(fun inbox -> agentLoop state inbox)

    abstract member handleCast: obj -> 'T -> 'T
    abstract member handleCall: obj -> 'T -> obj * 'T
    abstract member handleInfo: obj -> 'T -> 'T

    member this.Cast request = server.Post(Cast request)

    member this.Call request =
        server.PostAndReply(fun ((replyChannel: AsyncReplyChannel<obj>)) -> Call(replyChannel, request))

    member this.AsyncCall request =
        server.PostAndAsyncReply(fun ((replyChannel: AsyncReplyChannel<obj>)) -> AsyncCall(replyChannel, request))

    member this.Info request = server.Post(Info request)
