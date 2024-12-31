module TestServer

    open GenFServer

    // type State =
    //     | Count
    //     | BLABLA

    type CastType =
        | DecrCount
        | IncrCount

    type CallType =
        | GetCount
        | GetState

    type State = {
        count: int;
        other: string
    }

    type TestServer(initState ) =
        inherit GenFServer<State>(initState)

        override this.handleCast request  state = 
            match request with
            | :? CastType as cast ->
                match cast with
                | DecrCount ->  
                    {state with count = state.count - 1}
                    // match Map.tryFind Count state with
                    // | Some count -> 
                    //     // printfn "DECREASING %i with 1" count
                    //     let updateCount = count - 1

                    //     Map.add Count updateCount state
                    // | _ -> state
                | IncrCount -> 
                    {state with count = state.count + 1}
                    // match Map.tryFind Count state with
                    // | Some count -> 
                    //     // printfn "INCREASING %i with 1" count
                    //     let updateCount = count + 1


                    //     let newState = Map.add Count updateCount state

                    //     // printfn "new state: %A" newState

                    //     newState
                    // | _ -> state
            | _ -> raise (System.NotImplementedException())

        override this.handleCall request state = 
            match request with
            | :? CallType as call ->
                match call with
                | GetCount -> 
                    state.count, state
                    // printfn "FOUND state: %A" state
                    // match Map.tryFind Count state with
                    // | None -> None, state
                    // | result -> result, state
                | GetState -> state, state

            | _ -> raise (System.NotImplementedException())

        override this.handleInfo request state = 
            match request with
            | _ -> raise (System.NotImplementedException())

