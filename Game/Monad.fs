namespace Monad
#if INTERACTIVE
#r @"C:\Users\Dan\Source\Repos\CodexParser\packages\FSPowerPack.Core.Community.2.0.0.0\Lib\Net40\FSharp.PowerPack.dll"
#endif
module Distribution =
    type 'a Outcome = { Value: 'a; Probability : BigRational  }

    type 'a Distribution = 'a Outcome seq

    // P(A AND B) = P(A | B) * P(B)
    let bindD (dist:'a Distribution) (f: 'a -> 'b Distribution) =
        dist 
            |> Seq.map (fun p1 -> 
                    f p1.Value
                    |> Seq.map (fun p2 -> 
                            { Value = p2.Value; 
                                Probability = 
                                  p1.Probability * p2.Probability}))
            |> Seq.concat : 'b Distribution

    let returnD (value:'a) : 'a Distribution =   
        Seq.singleton { Value = value ; Probability = 1N/1N }

    type DistributionMonadBuilder() =
        member this.Bind (r, f) = bindD r f
        member this.Return x = returnD x
        member this.ReturnFrom m = m

    let distribution = DistributionMonadBuilder()

    let toUniformDistribution seq : 'a Distribution =
        let l = Seq.length seq
        seq |> Seq.map (fun e -> 
                { Value = e;  Probability = 1N / bignum.FromInt l })

    let probability (dist:'a Distribution) = 
        dist |> Seq.map (fun o -> o.Probability)  |> Seq.sum

    let average (dist: 'a Distribution) =
        dist |> Seq.map (fun o -> o.Probability * BigRational.FromInt(o.Value) ) |> Seq.sum

    let filter predicate (dist:'a Distribution) =
        dist |> Seq.filter (fun o -> predicate o.Value)

    let D sides = toUniformDistribution [1..sides]
    let D6 = D 6
    let HitDice = [D6; D6; D6; D6] |> List.map (fun x -> x |> average)|> List.sum;
//
//    type CoinSide = | Heads | Tails
//
//    let fairCoin = toUniformDistribution [Heads; Tails]
//
//    let fairCoinAndDice = distribution {
//        let! d = fairDice 6
//        let! c = fairCoin
//        return d,c }
//
//    fairCoinAndDice
//      |> filter (fun (d,c) -> c = Heads && d > 3)
//      |> probability
//      |> printfn "P(Heads and dice > 3) = %A" // "1/4N"