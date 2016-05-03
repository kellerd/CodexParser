namespace Distribution

module Probability =
    open Distribution

    // selectOne [1; 2; 3] =
    //    seq [ { Value = (1, [2; 3]); Probability = 1.0 / 3.0 }
    //         { Value = (2, [1; 3]); Probability = 1.0 / 3.0 }
    //         { Value = (3, [1; 2]); Probability = 1.0 / 3.0 } ]
    let selectOne values =
        [for e in values -> (e, values |> Seq.filter ((<>) e))]
        |> toUniformDistribution
 
    // selectMany 0 [1; 2; 3] = seq [{Value = ([], [1; 2; 3]); Probability = 1.0;}]
    // selectMany 1 [1; 2; 3] = selectOne [1; 2; 3]
    // selectMany 2 [1; 2; 3] =
    //    seq [ { Value = ([1; 2], seq [3]); Probability = 1.0 / 6.0 }
    //         { Value = ([1; 3], seq [2]); Probability = 1.0 / 6.0 } 
    //         [... 3 ...]
    //         { Value = ([3; 2], seq [1]); Probability = 1.0 / 6.0 } ]
    let rec selectMany n values =
        match n with
        | 0 -> certainly ([], values)
        | _ ->
            distribution {
                let! (x,c1) = selectOne values
                let! (xs,c2) = selectMany (n-1) c1
                return x::xs,c2 }

    // select 2 [1; 2; 3] =
    //    seq [ { Value = [2; 1]; Probability = 1.0 / 6.0 }
    //        { Value = [3; 1]; Probability = 1.0 / 6.0 }
    //        [... 3 ...]
    //        { Value = [3; 2]; Probability = 1.0 / 6.0 } ]
    let select n values =
        selectMany n values
        |> Seq.map (fun o ->
            { Value = o.Value |> fst |> List.rev;
              Probability = o.Probability })

    let remove items = Seq.filter (fun v -> Seq.forall ((<>) v) items)

    let filterInAnyOrder items dist =
        items
        |> Seq.fold (fun d item -> filter (Seq.exists ((=) item)) d) dist