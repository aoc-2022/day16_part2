module day16_part2.Cache

open day16_part2.State

type CacheKey = string*string*int*Set<string>

// cachekey -> time,score
type Cache(cache:Map<CacheKey,int>) =
    let buildCacheKey (state:State) =
            let you = state.Actors.You.Location
            let ele = state.Actors.Ele.Location
            let opened = state.Valves.Opened
            let time = state.Time 
            you,ele,time,opened
    let shouldCheck (state:State) =
        state.Actors.You.Awake && state.Actors.Ele.Awake
        
    member this.ShouldContinue (state:State) : Cache*bool =
        if shouldCheck state then 
            let key = buildCacheKey state
            match cache.TryFind key with
            | Some score ->
                if score >= state.Valves.Released then 
                    this,false
                else
                    Cache(cache.Add (key,state.Valves.Released)),true
            | None -> Cache(cache.Add (key,state.Valves.Released)),true
        else
            this,true 
        
    override this.ToString () = $"Cache size={cache.Count}"
    
    static member empty = Cache(Map.empty)
     

