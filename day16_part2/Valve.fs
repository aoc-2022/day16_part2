module day16_part2.Valve

type Valve(name: string, flowRate: int, leadsTo: Map<string, int>) =
    member this.Name = name
    member this.LeadsTo = leadsTo
    member this.FlowRate = flowRate
    member this.DeadEnd = leadsTo.Count = 1
    member this.IsTunnel = flowRate = 0 
    member this.Open() = Valve(name, 0, leadsTo)

    override this.ToString() =
        $"Valve({name}, {flowRate}, {leadsTo})"

