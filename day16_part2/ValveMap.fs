module day16_part2.ValveMap

open day16_part2.Valve

type ValveMap(valves: Map<string, Valve>, released: int, remaining: int, opened: Set<string>) =
    member this.Valves = valves
    member this.Released = released
    member this.Remaining = remaining
    member this.Opened = opened 

    member this.Open (id: string) (time: int) : ValveMap =
        let value = valves[id].FlowRate * time
        let valves = valves.Add(id, valves[ id ].Open())
        let released = released + value
        ValveMap(valves, released, remaining - valves[id].FlowRate, opened.Add id)

    member this.CanOpen(id: string) : bool = valves[id].FlowRate > 0

    override this.ToString() = $"Valves({valves} released={released})"

    static member start(valves: Map<string, Valve>) =
        let totalFlow = valves.Values |> Seq.map (fun v -> v.FlowRate) |> Seq.sum
        ValveMap(valves, 0, totalFlow, Set.empty)

let cleanValves (valves: ValveMap) : ValveMap =
    // it's ok if we stand on the cleaned ones, as we're only removing ingoing
    let clean (id: string, valve: Valve) =
        let clean (next: string, cost: int) : seq<string * int> =
            let nextValve = valves.Valves[next]

            if nextValve.FlowRate = 0 then
                nextValve.LeadsTo |> Map.toSeq |> Seq.map (fun (id, i) -> (id, i + cost))
            else
                seq { (next, cost) }

        let leadsTo = valve.LeadsTo |> Map.toSeq |> Seq.collect clean |> Map.ofSeq
        (id, Valve(valve.Name, valve.FlowRate, leadsTo))

    let newMap = valves.Valves |> Map.toSeq |> Seq.map clean |> Map.ofSeq
    ValveMap(newMap, valves.Released, valves.Remaining,valves.Opened)

let clearTooFar (timeLeft: int) (valveMap: ValveMap) =
    let valves: Valve list = valveMap.Valves.Values |> Seq.toList

    let clean (valve: Valve) : Valve =
        let leadsTo =
            valve.LeadsTo
            |> Map.toSeq
            |> Seq.filter (fun (_, i) -> i < timeLeft)
            |> Map.ofSeq

        Valve(valve.Name, valve.FlowRate, leadsTo)

    let valves = valves |> List.map clean |> List.map (fun v -> v.Name, v) |> Map.ofList
    ValveMap(valves, valveMap.Released, valveMap.Remaining, valveMap.Opened)
