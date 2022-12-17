module day16_part2.Actor

type Actor(loc: string, busy: int, justLeft: Option<string>) =
    member this.Location = loc
    member this.Busy = busy
    member this.Awake = busy < 1
    member this.JustLeft = justLeft
    member this.Commit(committed: int) = Actor(loc, busy + committed, justLeft)
    member this.Wait(t: int) = Actor(loc, max (busy - t) 0, None)
    member this.MoveTo(target: string) (cost: int) = Actor(target, busy + cost, Some(loc))
    override this.ToString() = $"[{loc},{busy}]"
