module day16_part2.Input

open System.IO
open day16_part2.Valve 

let readValveMap (fileName:string) = 
    let input = File.ReadAllLines fileName |> Array.toList

    let parse (s: string) =
        let s = s.Split [| ' '; ';'; ','; '=' |]
        let name = s[1]
        let value = s[5] |> int
        let next = s |> Array.skip 11 |> Array.toList |> List.filter (fun s -> s.Length = 2)
        let next = next |> List.map (fun s -> s, 1) |> Map.ofList
        Valve(name, value, next)

    input |> List.map parse |> List.map (fun v -> v.Name, v) |> Map.ofList


