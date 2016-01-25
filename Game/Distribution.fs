namespace Distribution
module Distribution =
    type 'T Outcome = { Value: 'T; Probability: float;} 
//        static member (+) (x,y) = 
//            let xProbNormalized = x.Probability / (x.Probability + y.Probability)
//            let yProbNormalized = y.Probability / (x.Probability + y.Probability)
//            let xNormalized = x.Value
//            {Value = (x.Value * xProbNormalized) + (y.Value * yProbNormalized); Probability = y.Probability + x.Probability}
//        static member DivideByInt (value,i) = value
//        static member Zero() = {Value = (float)0;Probability=(float)0}
    type 'T Distribution = 'T Outcome seq
 
    let bindD (dist:'T Distribution) (rest: 'T -> 'U Distribution) =
        dist
        |> Seq.map (fun o1 -> 
            o1.Value
            |> rest
            |> Seq.map (fun o2 -> { Value = o2.Value; Probability = o1.Probability * o2.Probability}))
        |> Seq.concat
 
    let returnD value = Seq.singleton { Value = value ; Probability = 1.0 }

    /// Monadic wrapper
    type DistributionMonadBuilder () =
        member this.Bind (r, f) = bindD r f
        member this.Return x = returnD x
        member this.ReturnFrom m = m
 
    let distribution = DistributionMonadBuilder ()

    /// Utility functions
    let toUniformDistribution seq =
        let l = Seq.length seq
        seq |> Seq.map (fun e -> { Value = e; Probability = 1.0 / (float l) })

    let certainly = returnD
 
    let probability dist =
        dist |> Seq.map (fun o -> o.Probability) |> Seq.sum
 
    let filter predicate dist =
        dist |> Seq.filter (fun o -> predicate o.Value)

    let average dist = 
        let f = (float)
        dist |> Seq.map (fun o -> o.Probability * f o.Value ) |>  Seq.sum

    let map f dist = 
        dist 
          |> Seq.map (fun o -> { Value = f o.Value; Probability = o.Probability })

    
    let apply fDist xDist = distribution {
        let! d = fDist
        let! c = xDist
        return d c}
    
    //Traverse f seq = List<a> -> Distribution<List<b>>
    let traverseDistributionA f seq =

        // define the applicative functions
        let (<*>) = apply
        let retn = returnD

        // define a "cons" function
        let cons head tail = head :: tail

        // right fold over the list
        let initState = retn []
        let folder head tail = 
            retn cons <*> (f head) <*> tail

        Seq.foldBack folder seq initState 
    let traverseDistributionM f list =

        // define the monadic functions
        let (>>=) = bindD 
        let retn = returnD

        // define a "cons" function
        let cons head tail = head :: tail

        // right fold over the list
        let initState = retn []
        let folder head tail = 
            f head >>= (fun h -> 
            tail >>= (fun t ->
            retn (cons h t) ))

        List.foldBack folder list initState 

     /// Transform a "list<Distribution>" into a "Distribution<list>"
    /// and collect the Distributions using apply
    /// Distribution<'a> list -> Distribution<'a list>
    /// Array[Seq[{Value = 1; Probability = 0.1666666667;}; 
                ///    {Value = 2;Probability = 0.1666666667;};
                ///    {Value = 3;Probability = 0.1666666667;};
    /// to: 
    /// Seq[{Value = [1; 1; 1; 1]; Probability = 0.0007716049383;}; 
    ///    {Value = [1; 1; 1; 2]; Probability = 0.0007716049383;
    let sequenceDistributionA x = traverseDistributionA id x

    /// Transform a "list<Distribution>" into a "Distribution<list>" 
    /// and collect the Distributions using bind.
    /// Distribution<'a> list -> Distribution<'a list>
    let sequenceDistributionM x = traverseDistributionM id x
    
//    type CoinSide = | Heads | Tails
//
//    let fairDice n = toUniformDistribution [1..n]
//    let fairCoin = toUniformDistribution [Heads; Tails]
//
//    let fairCoinAndDice = distribution {
//        let! d = fairDice 6
//        let! c = fairCoin
//        return d,c }
