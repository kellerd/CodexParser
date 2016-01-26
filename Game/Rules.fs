namespace Rules

//namespace Domain

//
//module Try2 =
//    open Domain.WarhammerDomain
//    type Result<'TSuccess,'TFailure> = 
//        | Success of 'TSuccess
//        | Failure of 'TFailure
//
//    type DiceRoll = DiceRoll of int
//    
//    type AttemptToShoot = AttemptToShoot of Model * Model * Weapon * (int -> Result<int,int>)
//    type DieResult = int
//    type ShootResult = ShootResult of Model * Model * Weapon * DieResult
//    //type AttemptToAssault = AttemptToAssault of Model * Model * Weapon
//
//    let Check d6 dPlus =
//        let (DiceRoll die) = d6()
//        printfn "Die: %A" die |> ignore
//        if die >= dPlus then Success die else Failure die 
//    
//    let d6 = 
//        let rnd = System.Random()
//        fun () -> DiceRoll (rnd.Next(1,7))
//    
//// Try one
//    let getHitsShooting bs d6Check =
//         d6Check (System.Math.Max(7 - bs, 2)) //Redo calculation
//    
//    let getWounds str tough d6Check =
//         match str - tough with 
//            | 0 -> d6Check 4
//            | 1 -> d6Check 3
//            | -1 -> d6Check 5
//            | -2 -> d6Check 6
//            | -3 -> d6Check 6
//            | x when x > 0 -> d6Check 2
//            | _ -> d6Check 0
//
//    let getUnsavedWounds saves pen d6Check =
//        match pen - saves with
//        | x when x > 0 -> d6Check saves
//        | _ -> d6Check 2
//
//    let getHitsAssault ws wsOpponent d6Check =
//         match ws,wsOpponent with
//            | x,y when x > y -> d6Check 3
//            | x,y when y > x * 2 -> d6Check 5
//            | _ -> d6Check 4
//
//
// //Try 2
//     // apply either a success function or failure function
//    let either successFunc failureFunc twoTrackInput =
//        match twoTrackInput with
//        | Success s -> successFunc s
//        | Failure f -> failureFunc f
//    let succeed x = 
//        Success x
//
//    let fail x = 
//        Failure x
//
//    //One to two normal track, to two x two special track
//    let bind f = 
//        either f fail
//
//    let (>>=) x f = 
//        bind f x
//
//    let (>=>) s1 s2 = 
//        s1 >> bind s2
//
//    let inline flatten (result : Result<Result<_,_>,_>) =
//        result |> bind (fun x -> x)
//
//
//    let inline apply wrappedFunction result = 
//        match wrappedFunction, result with
//            | Success f, Success x -> Success(f x)
//            | Failure f, Success x -> Failure(x)
//            | Success f, Failure x -> Failure(x)
//            | Failure f, Failure x -> Failure(x)   
//    let (<*>) = apply
//
//    /// Lifts a function into a Result container and applies it on the given result.
//    let inline lift f result = apply (succeed f) result
//
//    /// Lifts a function into a Result and applies it on the given result.
//    /// This is the infix operator version of ErrorHandling.lift
//    let inline (<!>) f result = lift f result
////
////    let makeHitAssault assaulter target weapon d6Check = 
////        lift (getHitsAssault assaulter.WeaponSkill target.WeaponSkill d6Check)
//
//    type ResultBuilder() =
//
//        member this.Bind(m, f) = 
//            bind f m
//
//        member this.Return(x) = 
//            Success x
//
//        member this.ReturnFrom(x) = 
//            x
//
//        member this.Zero() = 
//            Failure
//
//    // make an instance of the workflow                
//    let result = new ResultBuilder()
//
//
//    let d6Check = (Check d6)
//
//
//    let m1={WeaponSkill=4;BallisticSkill=4;Strength=4;Toughness=4;Wounds=2;Saves=4}
//    let target={WeaponSkill=4;BallisticSkill=4;Strength=4;Wounds=2;Toughness=4;Saves=4}
//    let weap={weaponName="PowerFist";weaponType=Assault;weaponAttacks=1;weaponStrength=5;weaponArmorPen=3;weaponRange=0;weaponIsTwinLinked=false;}
//
//
//    //Try 3
//
//    let makeHitAssault2 assaulter target weapon d6Check previousResult = result {
//        let! r = getHitsAssault assaulter.WeaponSkill target.WeaponSkill d6Check
//        return r
//    }
//
//    let makeWoundsAssault2 assaulter target weapon d6Check previousResult = result {
//        let! r = getWounds assaulter.Strength target.Toughness d6Check
//        return r
//    }
//    let makeUnsavedWounds assaulter target weapon d6Check previousResult = result {
//        let! r = getUnsavedWounds target.Saves weapon.weaponArmorPen d6Check 
//        return r
//    }
//
//    let x = makeHitAssault2 m1 target weap d6Check Success
//    let y = makeWoundsAssault2 m1 target weap d6Check x
//    let z = makeUnsavedWounds m1 target weap d6Check y
//
//    let doAssault assaulter target weapon d6Check = result {
//        let! r = makeHitAssault2 assaulter target weapon d6Check Success
//        let! t = makeWoundsAssault2 assaulter target weapon d6Check r
//        let! s = makeUnsavedWounds assaulter target weapon d6Check t
//        return s
//    }
//    let xy = result {
//        let! a = makeHitAssault2 <!> (Success m1) <*> (Success target) <*> (Success weap) <*> (Success d6Check)
//        return a
//    }
//
//    let x1 = doAssault m1 target weap d6Check
#if INTERACTIVE
    #r @"C:\Users\Dan\Source\Repos\CodexParser\packages\Chessie.0.4.0\lib\net40\Chessie.dll"
    #r @"C:\Users\Dan\Source\Repos\CodexParser\packages\FSharpx.Extras.1.10.3\lib\40\FSharpx.Extras.dll"
    #load "Distribution.fs"
    #load "Domain.fs"
#endif
module RuleEquations = 
    open Chessie.ErrorHandling
    open Domain.WarhammerDomain

    let D = 
        let r = System.Random()
        let d num = (r.Next(1,num+1))
        d

    let getWsTable ws wsopponent = 
        match ws, wsopponent with 
            | WeaponSkill ws,  WeaponSkill wsopponent when ws > wsopponent -> 3
            | WeaponSkill ws,  WeaponSkill wsopponent when wsopponent > ws * 2 -> 5
            | _ -> 4    

    let getHitsShooting bs  = 
          (System.Math.Max(7 - bs, 2)) //Redo calculation
    
    let getWounds str tough  =
         let (Strength str') = str
         let (Toughness tough') = tough
         match str' - tough' with 
            | 0 ->  4
            | 1 ->  3
            | -1 ->  5
            | -2 ->  6
            | -3 ->  6
            | x when x > 0 ->  2
            | _ ->  0

    let getUnsavedWounds saves pen  =
        match pen - saves with
        | x when x > 0 -> saves
        | _ ->  2

module Try5 =
    open RuleEquations
    open Chessie.ErrorHandling
    open Chessie.ErrorHandling.Trial
    open Domain.WarhammerDomain
    
    type DiceRoll<'a> = DiceRoll of (unit->'a)
    type DiceCheck<'a> = DiceCheck of (int->'a)
    type RollResult<'a> = RollResult of 'a 

    let D6 = 
        DiceRoll (fun () -> D 6)
    let D6s =
        Seq.initInfinite (fun _ -> D6)

    type Rule<'a> = Rule of 'a
    type Hits = Hits of int
    let mapList xs = xs |> List.map (fun z -> RollResult z)

    type MakeHit<'a> = MakeHit of (Rule<int->Result<'a,'a>> -> WeaponSkill -> WeaponSkill ->  Hits*Result<'a,'a>)

    let run rule input = 
        let (Rule innerFn) = rule
        innerFn input
    
    let makeD6 d6s  =
        let innerFn dPlus =
           let die = Seq.head d6s |> (fun (DiceRoll d) -> d())
           if die >= dPlus then ok die else fail die
        Rule innerFn

    let makeHit D6 = 
        let innerFn (wsTarget,wsOpponent) =
            let result = getWsTable wsTarget wsOpponent |> run D6
            match result with 
                | Ok (x,y) -> ok (Hits 1, x) //,RollResult x, mapList y)
                | Bad (y) -> fail (Hits 0, y |> List.tryHead) //, mapList y)
        Rule innerFn
    let makeWounds D6  = 
        let innerFn (hits,(str,tough)) = 
            let (Hits hits') = hits
            let result = getWounds str tough |> run D6
            match result with 
                    | Ok (x,y) -> ok (Wounds (hits' * 1),x)
                    | Bad (y) -> fail (Wounds 0)
//            let result = List.init hits' (fun _ -> getWounds str tough |> run D6)
//            result |> List.map (function
//                                        | Ok (x,y) -> ok (Wounds 1,x)//,RollResult x, mapList y)
//                                        | Bad (y) -> fail (Wounds 0)//, mapList y)
//                                    )
        Rule innerFn
        
    let andThen rule1 rule2 =
        let innerFn input input2 =
            let result1 = run rule1 input
            match result1 with
            | Bad err -> 
                fail [],err
            | Ok ((value1,result),resultList) -> 
                let result2 =  run rule2 (value1,input2)
                match result2 with 
                |  Bad err2 ->
                    fail err2, resultList 
                | Ok (value2,resultList2) -> 
                    let newValue = (value1,value2)
                    ok (newValue),[]
        Rule innerFn 

    let map f r =
        let innerFn input =
            let result = run r input
            match result with 
                | Ok (v,l) -> 
                    let newValue = f v
                    ok (newValue, l |> List.tryHead)
                | Bad err ->
                    fail (err |> List.tryHead)
        Rule innerFn

    let ( .>>. ) = andThen


    let D6R = makeD6 D6s
    let mh2 = makeHit D6R
    let mw2 = makeWounds D6R
    let mapped = map (fun (Hits(x),_) -> x) mh2
    let doHits = run mh2 (WeaponSkill 4, WeaponSkill 4)
    let doHits' = run mapped (WeaponSkill 4, WeaponSkill 4)
    let hitsThenWounds = mh2 .>>. mw2
    let hitsThenWounds' = run hitsThenWounds (WeaponSkill 4, WeaponSkill 4) (Strength 4, Toughness 4)


