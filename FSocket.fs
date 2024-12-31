module FSocket

    open GenServer
    open System.Net.WebSockets

    type FSocket(state) =
        inherit GenServerI<Map<string, int>>(state)



    