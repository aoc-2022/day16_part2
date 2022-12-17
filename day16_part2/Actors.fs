module day16_part2.Actors

open day16_part2.Actor

type Actors(you: Actor, ele: Actor) =
    member this.You = you
    member this.Ele = ele
    member this.PendingIdleTime = min you.Busy ele.Busy

    member this.Wait(t: int) =
        // printfn $"wait(1) {t}: {you} {ele}"
        let you = you.Wait t
        let ele = ele.Wait t
        // printfn $"wait(2) {t}: {you} {ele}"
        Actors(you, ele)

    member this.YouCommit(t: int) = Actors(you.Commit t, ele)

    member this.YouMove (target: string) (cost: int) =
        let you = you.MoveTo target cost 
        Actors(you, ele)

    member this.EleCommit(t: int) = Actors(you, ele.Commit t)

    member this.EleMove (target: string) (cost: int) =
        let ele = ele.MoveTo target cost 
        Actors(you, ele)
    
    member this.EleIsAfterYou (target:string) =
        if you.JustLeft = Some(ele.Location) then target <= you.Location
        else true // ele is somewhere else, so at least not stealing your first options 
    member this.Together = (you.Location = ele.Location) && (you.Awake = ele.Awake)

    override this.ToString() = $"[{you}{ele}]"
    static member start = Actors(Actor("AA", 0,None), Actor("AA", 0,None))
