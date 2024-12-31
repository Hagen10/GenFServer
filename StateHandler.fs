module StateHandler

    open GenServer
    open Listener

     type StateHandler(state, listener : Listener) =
        inherit GenServerI<Map<string, int>>(state)

        member this.UpdateState key value =
            let handler a b state =
                match state with
                | State state' -> 
                    match Map.tryFind a state' with
                    | Some _ ->
                        let res = state' |> Map.add a b
                        State(res)
                    | _ -> state

            listener.StateUpdated()
            this.Cast (handler key value)

        member this.GetValue key =
            let handler key state =
                match state with
                | State state' ->
                    match Map.tryFind key state' with 
                    | Some value ->
                        state, Int(value)
                    | _ -> state, None

            this.Call (handler key)

        member this.PrintState() =
            match this.GetState() with
            | State state' -> 
                state' |> Map.iter  (fun key value -> printfn "Key: %s, Value: %i" key value)
