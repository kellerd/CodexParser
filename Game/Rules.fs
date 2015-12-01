namespace Domain

module WarhammerRules =
    open Domain.WarhammerDomain

    type DiceRoll = DiceRoll of int

    type AttemptToShoot = AttemptToShoot of BallisticSkill
    type ShootingHitResult = ShootingHitResult of float
    type AttemptToWound = AttemptToWound of ShootingHitResult * Strength * Toughness
    type ShootingWoundResult = ShootingWoundResult of float
    type AttemptToKill = AttemptToKill of ShootingWoundResult * Saves * ArmorPen
    type ShootingUnsavedResult = ShootingUnsavedResult of float

    let Check d6 dPlus =
        let (DiceRoll die) = d6()
        printfn "Die: %A" die |> ignore
        if die >= dPlus then 1. else 0.
    
    let d6 = 
        let rnd = System.Random()
        fun () -> DiceRoll (rnd.Next(1,7))
    
    let SvsT s t d6check =
        printfn "S-T:%A" (s - t) |> ignore
        match s - t with 
        | 0 -> d6check 4
        | 1 -> d6check 3
        | -1 -> d6check 5
        | -2 -> d6check 6
        | -3 -> d6check 6
        | x when x > 0 -> d6check 2
        | _ -> d6check 0


    let getHits shot d6Check =
         let (AttemptToShoot (bs)) = shot
         ShootingHitResult(d6Check (System.Math.Max(7 - bs, 2)))
    
    let getWounds a d6Check  =
         let (AttemptToWound (h, str,tough)) = a
         let (ShootingHitResult(hits)) = h
         ShootingWoundResult(hits * SvsT str tough d6Check)

    let getUnsavedWounds wounds d6Check  =
         let (AttemptToWound (h, str,tough)) = a
         let (ShootingHitResult(hits)) = h
         ShootingWoundResult(hits * SvsT str tough d6Check)
         
//    let a = Attack(4, 4)
    let h = getHits (AttemptToShoot(4)) (Check d6) 
    let w = getWounds (AttemptToWound(h, 4, 4)) (Check d6)
    let w = getSaves (AttemptToKill(w, 2 , 4)) (Check d6)

//    
//
//
//
//
//    type 'T Rule = {Calculation: ('T Distribution.Distribution list -> 'T Distribution.Distribution); 
//                    Check:('T->bool)}
//    
//    let sumOfTwo = Distribution.sequenceDistributionA >> Distribution.map Seq.sum
//
//    //LD >= Result
//    let MortalCheck ld = 
//        ld |> (>=)     
//    
//    let RunePriest = 
//        let ld = 10
//        let Moral = {Calculation=sumOfTwo; Check=MortalCheck ld}
//        [Moral]
//    
//    printfn "%A" ([D6; D6] |> RunePriest.Head.Calculation |> Distribution.filter RunePriest.Head.Check |> Distribution.probability) |> ignore
//    printfn "%A" ([D6; D6] |> RunePriest.Head.Calculation |> Distribution.average ) |> ignore

        
//
//type Rank = int
//type Suit = | Spades | Hearts | Diamonds | Clubs
//type Card = Rank * Suit
//
//let value = fst
//let suit = snd
//
//let A,K,Q,J,T = 14,13,12,11,10
//let allRanksInSuit suit = [2..A] |> List.map (fun rank -> rank,suit)
//let completeDeck = 
//  [Spades; Hearts ; Diamonds; Clubs] 
//    |> List.map allRanksInSuit 
//    |> List.concat
//let isPair c1 c2 = value c1 = value c2
//let isSuited c1 c2 = suit c1 = suit c2
//let isConnected c1 c2 = 
//    let v1,v2 = value c1,value c2
//    (v1 - v2 |> abs |> (=) 1) ||
//    (v1 = A && v2 = 2) ||
//    (v1 = 2 && v2 = A)

//// draw Ace of Clubs and Ace of Spaces in order
//completeDeck 
//  |> select 2
//  |> filter ((=) [A,Clubs; A,Spades])
//  |> probability 
//  |> printfn "%A"  // prints "1/2652"
//
//// draw Ace of Clubs and Ace of Spaces in any order
//completeDeck 
//  |> select 2
//  |> filterInAnyOrder [A,Clubs; A,Spades]    
//  |> probability
//  |> printfn "A"  //  prints "1/1326"

