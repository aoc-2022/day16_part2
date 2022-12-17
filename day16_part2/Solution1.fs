module day16_part2.Solution1

open day16_part2.Valve
open day16_part2.ValveMap
open day16_part2.Actors
open day16_part2.State
open day16_part2.Cache

let rec solve (state: State) (cache:Cache) : int*Cache=
    // printfn $"solve(1) time={state.Time} actors={state.Actors} res={state.Valves.Released} best={state.Best} cache={cache}"
    let state = state.MoveIdleTime()
    let (cache,shouldContinue) = cache.ShouldContinue state
    if not shouldContinue then
        // printfn $"Already cached better {cache}"
        state.Best,cache
    else 
    if state.UpperLimitRemaining + state.Valves.Released < state.Best then
        state.Best,cache // too little time left to release enough pressure
    else
        let state =
            if state.Time % 4 = 0 && state.Time > 5 then
                cleanState state
            else
                state

        if state.OutOfTime then
            let state = state.RegisterNewResult state.Valves.Released

            if (state.Best > 1000) then
                printfn $"Outoftime & > 1000: {state.Best}"
            // printfn $"Returning {state.Best}"
            state.Best,cache
        else
            let youOpenState = state.TryYouOpen |> Option.toArray |> Array.toList
            let eleOpenState = state.TryEleOpen |> Option.toArray |> Array.toList
            let youMoves = state.YouMoves() |> List.map state.YouMove

            let eleMoves =
                (if youMoves.IsEmpty then state.EleMoves() else []) |> List.map state.EleMove

            let next = [ youOpenState; eleOpenState; youMoves; eleMoves ] |> List.concat

            let rec stepOn (l: State list) (best: int) (cache:Cache) =
                // printfn $"stepOn {best}"

                match l with
                | [] -> best,cache
                | s :: rest ->
                    let s = s.RegisterNewResult best
                    // printfn $"-> {s.Actors} {s.Best}"
                    let res,cache = solve s cache
                    let best = max best res
                    stepOn rest best cache 

            stepOn next state.Best cache 

let solution1 (optimizedValves: Map<string, Valve>) =
    let valves = ValveMap.start optimizedValves
    let initState = State(26, Actors.start, valves, 0)
    let top = optimizedValves.Values |> Seq.map (fun v -> v.FlowRate) |> Seq.max
    solve initState Cache.empty

    printfn $"top={top}"
