let sumadeDigitos g =
    let rec reit g lad value =
        let pivot = g % 10I
        if g > 0I then
            reit (g/10I) (lad + pivot) (pivot::value)
        else lad, value
    let resource, stg = reit g 0I []
    List.map string stg|> String.concat "+"
    |> fun x -> printfn "%s = %A" x resource
    resource

let AditivaPerst g =
    let rec reit g veces =
        if g < 10I then veces
        else reit (sumadeDigitos g) (veces + 1)
    reit (System.Numerics.BigInteger.Parse(g.ToString())) 0


AditivaPerst 55552222111119I;