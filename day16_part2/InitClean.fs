module day16_part2.InitClean

open day16_part2.Valve 

let removeDeads (valves: Map<string, Valve>) : Map<string, Valve> =
    let isDead (v: Valve) = v.DeadEnd
    let toName (v: Valve) = v.Name
    let deads = valves.Values |> Seq.filter isDead |> Seq.map toName |> Set.ofSeq

    let alive =
        valves.Values
        |> Seq.filter (fun v -> deads.Contains v.Name |> not)
        |> Seq.toList

    let enrich (living: Valve) =
        let deadKeys = living.LeadsTo.Keys |> Seq.filter deads.Contains |> Set.ofSeq

        let livingKeys =
            living.LeadsTo.Keys
            |> Seq.filter (fun v -> deads.Contains v |> not)
            |> Set.ofSeq

        let pickups =
            deadKeys
            |> Set.toSeq
            |> Seq.map (fun s -> valves[s].FlowRate)
            |> Seq.toList

        // printfn $"dead = {deadKeys} living={livingKeys} pickups={pickups}"

        let leadsTo =
            livingKeys
            |> Set.toSeq
            |> Seq.map (fun n -> (n, living.LeadsTo[n]))
            |> Map.ofSeq

        Valve(living.Name, living.FlowRate, leadsTo)

    alive |> List.map enrich |> List.map (fun v -> v.Name, v) |> Map.ofList

let deTunnel (valveMap: Map<string, Valve>) =
    let tunnels =
        valveMap.Values
        |> Seq.filter (fun v -> v.IsTunnel)
        |> Seq.map (fun v -> v.Name)
        |> Set.ofSeq

    printfn $"TUNNELS: {tunnels}"
    
    let deTunnel (valve: Valve) =
        let replace = valve.LeadsTo.Keys |> Seq.filter (tunnels.Contains) |> Seq.toList

        let keep =
            valve.LeadsTo.Keys
            |> Seq.filter (fun id -> tunnels.Contains id |> not)
            |> Seq.toList

        let replaceId (id: string) =
            let cost = valve.LeadsTo[id]
            valveMap[id].LeadsTo |> Map.toList |> List.map (fun (id, v) -> id, v + cost)

        let replaced =
            replace
            |> List.collect replaceId
            |> List.filter (fun (id, _) -> id = valve.Name |> not)

        let kept = keep |> Seq.map (fun id -> id, valve.LeadsTo[id]) |> Seq.toList
        let updatedLeadsTo = [ replaced; kept ] |> List.concat |> Map.ofList
        Valve(valve.Name, valve.FlowRate, updatedLeadsTo)

    valveMap.Values |> Seq.map deTunnel |> Seq.map (fun v -> v.Name, v) |> Map.ofSeq

let removeUnreachable (valves: Map<string, Valve>) =
    let reachable = valves.Values |> Seq.collect (fun v -> v.LeadsTo.Keys) |> Set.ofSeq
    let all = valves.Keys |> Set.ofSeq

    let cleaned =
        valves
        |> Map.toSeq
        |> Seq.filter (fun (k, _) -> reachable.Contains k)
        |> Map.ofSeq

    let cleaned = cleaned.Add("AA", valves["AA"]) // keep the starting point even though it's a tunnel
    //    printfn $"REACHABLE: {all.Count} {reachable.Count} {all.Count - reachable.Count}"
    //    printfn $"CLEANED: {cleaned}"
    cleaned

let optimize(valves:Map<string,Valve>) = 
    valves |> deTunnel
    |> deTunnel
    |> deTunnel
    |> deTunnel
    |> deTunnel
    |> removeUnreachable
