namespace Domain

module WarhammerRules =
    open Domain.WarhammerDomain
    type Result<'TSuccess,'TFailure> = 
        | Success of 'TSuccess
        | Failure of 'TFailure

    type DiceRoll = DiceRoll of int
    
    type AttemptToShoot = AttemptToShoot of Model * Model * Weapon * (int -> Result<int,int>)
    type DieResult = int
    type ShootResult = ShootResult of Model * Model * Weapon * DieResult
    //type AttemptToAssault = AttemptToAssault of Model * Model * Weapon

    let Check d6 dPlus =
        let (DiceRoll die) = d6()
        printfn "Die: %A" die |> ignore
        if die >= dPlus then Success die else Failure die 
    
    let d6 = 
        let rnd = System.Random()
        fun () -> DiceRoll (rnd.Next(1,7))
    
// Try one
    let getHitsShooting bs d6Check =
         d6Check (System.Math.Max(7 - bs, 2)) //Redo calculation
    
    let getWounds str tough d6Check =
         match str - tough with 
            | 0 -> d6Check 4
            | 1 -> d6Check 3
            | -1 -> d6Check 5
            | -2 -> d6Check 6
            | -3 -> d6Check 6
            | x when x > 0 -> d6Check 2
            | _ -> d6Check 0

    let getUnsavedWounds saves pen d6Check =
        match pen - saves with
        | x when x > 0 -> d6Check saves
        | _ -> d6Check 2

    let getHitsAssault ws wsOpponent d6Check =
         match ws,wsOpponent with
            | x,y when x > y -> d6Check 3
            | x,y when y > x * 2 -> d6Check 5
            | _ -> d6Check 4

//    let makeHitAssault shooter,target,weapon,d6Check  =
//        make (getHitsAssault shooter.WeaponSkill target.WeaponSkill d6Check) attempt
//
//    let makeWoundAssault attempt =
//        let (shooter,target,weapon,d6Check,dieResult) = attempt
//        make (getWounds weapon.weaponStrength target.Toughness d6Check) (ignoreRight attempt)
//
//    let makeUnsavedWoundAssault attempt =
//        let (shooter,target,weapon,d6Check,dieResult) = attempt
//        make (getUnsavedWounds weapon.weaponArmorPen target.Saves d6Check) (ignoreRight attempt)
//
//    let subtractWound (shooter,target,weapon, d6Check,dieVal)  =
//        Success (shooter,{target with Wounds = target.Wounds-1})
//
//    let doAssault = 
//        makeHitAssault >=> makeWoundAssault >=> makeUnsavedWoundAssault >=> subtractWound
 //Try 2
     // apply either a success function or failure function
    let either successFunc failureFunc twoTrackInput =
        match twoTrackInput with
        | Success s -> successFunc s
        | Failure f -> failureFunc f
    let succeed x = 
        Success x

    let fail x = 
        Failure x

    //One to two normal track, to two x two special track
    let bind f = 
        either f fail

    let (>>=) x f = 
        bind f x

    let (>=>) s1 s2 = 
        s1 >> bind s2

    let inline flatten (result : Result<Result<_,_>,_>) =
        result |> bind (fun x -> x)


    let inline apply wrappedFunction result = 
        match wrappedFunction, result with
            | Success f, Success x -> Success(f x)
            | Failure f, Success x -> Failure(x)
            | Success f, Failure x -> Failure(x)
            | Failure f, Failure x -> Failure(x)   
    let (<*>) = apply

    /// Lifts a function into a Result container and applies it on the given result.
    let inline lift f result = apply (succeed f) result

    /// Lifts a function into a Result and applies it on the given result.
    /// This is the infix operator version of ErrorHandling.lift
    let inline (<!>) f result = lift f result
//
//    let makeHitAssault assaulter target weapon d6Check = 
//        lift (getHitsAssault assaulter.WeaponSkill target.WeaponSkill d6Check)

    type ResultBuilder() =

        member this.Bind(m, f) = 
            bind f m

        member this.Return(x) = 
            Success x

        member this.ReturnFrom(x) = 
            x

        member this.Zero() = 
            Failure

    // make an instance of the workflow                
    let result = new ResultBuilder()


    let d6Check = (Check d6)


    let m1={WeaponSkill=4;BallisticSkill=4;Strength=4;Toughness=4;Wounds=2;Saves=4}
    let target={WeaponSkill=4;BallisticSkill=4;Strength=4;Wounds=2;Toughness=4;Saves=4}
    let weap={weaponName="PowerFist";weaponType=Assault;weaponAttacks=1;weaponStrength=5;weaponArmorPen=3;weaponRange=0;weaponIsTwinLinked=false;}


    //Try 3

    let makeHitAssault2 assaulter target weapon d6Check = result {
        let! r = getHitsAssault assaulter.WeaponSkill target.WeaponSkill d6Check
        return r
    }

    let makeWoundsAssault2 assaulter target weapon d6Check previousResult = result {
        let! r = getWounds assaulter.Strength target.Toughness d6Check
        return r
    }
    let makeUnsavedWounds assaulter target weapon d6Check previousResult = result {
        let! r = getUnsavedWounds target.Saves weapon.weaponArmorPen d6Check 
        return r
    }

    let x = makeHitAssault2 m1 target weap d6Check
    let y = makeWoundsAssault2 m1 target weap d6Check x
    let z = makeUnsavedWounds m1 target weap d6Check y

    let doAssault assaulter target weapon d6Check = result {
        let! r = makeHitAssault2 assaulter target weapon d6Check
        let! t = makeWoundsAssault2 assaulter target weapon d6Check r
        let! s = makeUnsavedWounds assaulter target weapon d6Check t
        return s
    }

    let x1 = doAssault m1 target weap d6Check

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

