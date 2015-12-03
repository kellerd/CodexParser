namespace Domain

module WarhammerRules =
    open Domain.WarhammerDomain
    type Result<'TSuccess,'TFailure> = 
        | Success of 'TSuccess
        | Failure of 'TFailure

    type DiceRoll = DiceRoll of int
    type AttemptToShoot = AttemptToShoot of BallisticSkill
    type ShootingHitResult = ShootingHitResult of Result<int,int>
    type AttemptToWound = AttemptToWound of Strength * Toughness
    type ShootingWoundResult = ShootingWoundResult of Result<int,int>
    type AttemptToKill = AttemptToKill of Saves * ArmorPen
    type ShootingUnsavedResult = ShootingUnsavedResult of Result<int,int>

    
    type AttemptToAssault = AttemptToAssault of WeaponSkill * WeaponSkill
    type AssaultingHitResult = AssaultingHitResult of Result<int,int>
    type AssaultingWoundResult = AssaultingWoundResult of Result<int,int>
    type AssaultingUnsavedResult = AssaultingUnsavedResult of Result<int,int>

    let Check d6 dPlus =
        let (DiceRoll die) = d6()
        printfn "Die: %A" die |> ignore
        if die >= dPlus then Success die else Failure die 
    
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
    let SvvsPen sv pen d6check =
        match pen - sv with
        | x when x > 0 -> d6check sv
        | _ -> d6check 2

// Try one
    let d6Check = (Check d6)
    let getHits shoot =
         let (AttemptToShoot (bs)) = shoot
         ShootingHitResult(d6Check (System.Math.Max(7 - bs, 2))) //Redo calculation
    
    let getWounds attempts =
         let (AttemptToWound ( str,tough)) = attempts
         ShootingWoundResult(SvsT str tough d6Check) //Redo calculation

    let getUnsavedWounds attempts =
         let (AttemptToKill (saves,pen)) = attempts
         ShootingUnsavedResult(SvvsPen saves pen d6Check) //Redo calculation

    let getHitsAssault assault =
         let (AttemptToAssault (ws, wsOpponent)) = assault
         let check = match ws,wsOpponent with
                        | x,y when x > y -> d6Check 3
                        | x,y when y > x * 2 -> d6Check 5
                        | _ -> d6Check 4
         AssaultingHitResult(check) //Redo calculation
    
    let getWoundsAssault attempts  =
         let (AttemptToWound ( str,tough)) = attempts
         AssaultingWoundResult(SvsT str tough d6Check) //Redo calculation

    let getUnsavedAssault attempts =
         let (AttemptToKill (saves,pen)) = attempts
         AssaultingUnsavedResult(SvvsPen saves pen d6Check) //Redo calculation

 //Try 2
    
    //One to two normal track, to two x two special track
    let bind switchFunction twoTrackInput = 
        match twoTrackInput with
        | Success s -> switchFunction s
        | Failure f -> Failure f
    
    let (>>=) twoTrackInput switchFunction = 
        bind switchFunction twoTrackInput 
    

    let Assault = 
        let getWounds' = bind getWounds
        let getUnsavedWounds' = bind getUnsavedWounds
        let getHits' = bind getHits
        getHits >> getWounds' >> getUnsavedWounds







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

