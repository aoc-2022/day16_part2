module day16_part2.State

open day16_part2.Actors
open day16_part2.ValveMap 

type State(time: int, actors: Actors, valves: ValveMap, best: int) =
    member this.Time = time
    member this.Actors = actors
    member this.Valves = valves
    member this.OutOfTime = time < 1

    member this.UpperLimitRemaining = valves.Remaining * (time-1)

    member this.Best = best

    member this.OpenValve(s: string) =
        let valves = valves.Open s (time - 1)
        let best = max valves.Released best
        State(time, actors, valves, best)

    member this.MoveIdleTime() =
        let idle = actors.PendingIdleTime
        let time = time - idle
        let actors = actors.Wait idle
        State(time, actors, valves, best)

    member this.EleCanOpen =
        let valveClosed = valves.CanOpen actors.Ele.Location
        actors.Ele.Awake && (not actors.Together) && valveClosed

    member this.YouCanOpen: bool = actors.You.Awake && (valves.CanOpen actors.You.Location)
    member this.BothCanOpen = this.EleCanOpen && this.YouCanOpen

    member this.TryEleOpen = if this.EleCanOpen then Some(this.EleOpen()) else None

    member this.TryYouOpen = if this.YouCanOpen then Some(this.YouOpen()) else None

    member this.YouOpen() =
        let state = this.OpenValve actors.You.Location
        let actors = actors.YouCommit 1
        State(time, actors, state.Valves, best)

    member this.EleOpen() =
        let state = this.OpenValve actors.Ele.Location
        let actors = actors.EleCommit 1
        State(time, actors, state.Valves, best)

    member this.EleMoves() =
        if actors.Ele.Awake then
            let moves = valves.Valves[actors.Ele.Location].LeadsTo.Keys |> Seq.toList
            moves |> List.filter (actors.EleIsAfterYou)
        else
            []

    member this.YouMoves() =
        if actors.You.Awake then
            valves.Valves[actors.You.Location].LeadsTo.Keys |> Seq.toList
        else
            []

    member this.YouMove(target: string) =
        let cost = valves.Valves[actors.You.Location].LeadsTo[target]
        // if (cost > 10) then printfn $"COST>10 valves = {valves}"
        let actors = actors.YouMove target cost
        State(time, actors, valves, best)

    member this.EleMove(target: string) =
        let cost = valves.Valves[actors.Ele.Location].LeadsTo[target]
        let actors = actors.EleMove target cost
        State(time, actors, valves, best)

    member this.RegisterNewResult(result: int) =
        if (result > best) then
            printfn $"New best: {result} old={best}"

        State(time, actors, valves, max result best)

    member this.withValves(newValves: ValveMap) = State(time, actors, newValves, best)

    override this.ToString() =
        $"State time={time} {actors} {valves} {best}"

let cleanState (state: State) : State =
    let valves = cleanValves state.Valves
    let valves = clearTooFar state.Time valves
    state.withValves valves
