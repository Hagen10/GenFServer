module TestServer

    open GenFServer

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
                | IncrCount -> 
                    {state with count = state.count + 1}
            | _ -> raise (System.NotImplementedException())

        override this.handleCall request state = 
            match request with
            | :? CallType as call ->
                match call with
                | GetCount -> 
                    state.count, state
                | GetState -> state, state

            | _ -> raise (System.NotImplementedException())

        override this.handleInfo request state = 
            match request with
            | _ -> raise (System.NotImplementedException())

