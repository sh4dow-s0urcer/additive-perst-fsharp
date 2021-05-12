let digitosSum n =
    let rec loop n ady vals =
        let actual = n % 10I
        if n > 0I then
            loop (n/10I) (ady + actual) (actual::vals)
        else ady, vals
    let res, stx = loop n 0I []
    List.map string stx|> String.concat "+"
    |> fun v -> printfn "%s = %A" v res
    res

let persistenciaAditiva n =
    let rec loop n times =
        if n < 10I then times
        else loop (digitosSum n) (times + 1)
    loop (System.Numerics.BigInteger.Parse(n.ToString())) 0

persistenciaAditiva 50500505005050I;