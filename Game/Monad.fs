namespace Monad
#if INTERACTIVE
#r @"..\packages\FSPowerPack.Core.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.dll"
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

    let average f (dist: 'a Distribution) = Seq.map (fun o -> o.Probability * (f o.Value) ) >> Seq.sum

    let filter predicate (dist:'a Distribution) =
        dist |> Seq.filter (fun o -> predicate o.Value)

    let filterInAnyOrder = Seq.fold (fun d item -> filter (Seq.exists ((=) (item))) d) 

    let map f (dist:'a Distribution) : 'b Distribution = 
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
    let sequenceDistributionA x = traverseDistributionA id x

    /// Transform a "list<Distribution>" into a "Distribution<list>" 
    /// and collect the Distributions using bind.
    /// Distribution<'a> list -> Distribution<'a list>
    let sequenceDistributionM x = traverseDistributionM id x

//    let selectOne values =
//        [for e in values -> e,values |> Seq.filter ((<>) e)] 
//          |> toUniformDistribution
//
//    let rec selectMany n values =
//        match n with 
//        | 0 -> returnD values
//        | _ -> 
//            distribution {
//                 let! (x,c1) = selectOne values
//                 let! (xs,c2) = selectMany (n-1) c1
//                 return x::xs,c2}
//    
//    let select n values = 
//        selectMany n values   
//          |> map (fst >> List.rev)
//
    let remove items = Seq.filter (fun v -> Seq.forall ((<>) v) items)

    let D sides = toUniformDistribution [1..sides]
    let D6 = D 6
    let FourD6 =  [D6; D6; D6; D6] 
    let HitDice = FourD6 |> List.map (fun x -> x |> average)|> List.sum


    type CoinSide = | Heads | Tails

    let fairDice n = toUniformDistribution [1..n]
    let fairCoin = toUniformDistribution [Heads; Tails]

    let fairCoinAndDice = distribution {
        let! d = fairDice 6
        let! c = fairCoin
        return d,c }
