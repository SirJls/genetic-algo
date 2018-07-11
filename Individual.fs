module Individual

open System

type Individual =
| Absent
| Individual of int
with
    member i.Number =
        match i with
            | Absent -> 0
            | Individual i -> i
    member i.Binary =
        let rec makeBinary b n =
            match n with
                | n when n > 0 ->
                    let b' = sprintf "%i" (n &&& 1) in
                    makeBinary (b' + b) (n >>> 1)
                | _ -> b
        makeBinary "" i.Number
    member i.BinaryArray =
        let rec makeBinary n =
           match n with
               | n when n > 0 ->
                   let b' = (n &&& 1) in
                   b' :: makeBinary (n >>> 1)
               | _ -> []
        makeBinary i.Number |> List.rev
    member i.Length baseN =
        let rec length n nd =
            match n with
                | n when n = 0 -> nd
                | _ -> length (n / baseN) (nd + 1)
        length i.Number 0
    member i.GetBitRange left right =
        if left > 32 then
            failwith "Shift count overflow: left > 32."
        else
            // get bit status (0 | 1) multiplied by the decimal number on that
            // position for both left and right
            if left < right then
                0
            else
                (((i.Number >>> left) &&& 1) * (1 <<< left)) + i.GetBitRange (left-1) right
    member i.Mutate p =
        match i with
            | Individual n -> Individual (n ^^^ (1 <<< p))
            | _ -> Absent
    static member Equal a b =
        match a,b with
            | Individual an,Individual bn  -> an = bn
            | _ -> false
    static member None = Absent
    static member Create n =
        Individual n
