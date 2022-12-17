open System.IO
open day16_part2.Valve
open day16_part2.Input
open day16_part2.InitClean
open day16_part2.Solution1

let valves = readValveMap "/tmp/aoc/input.t"

// valves |> Map.toList |> List.map (printfn "%A")

let optimizedValves = optimize valves 

printfn "Optimized valves: "
optimizedValves |> Map.values |> Seq.toList |> List.map (printfn "%A")



solution1 optimizedValves
