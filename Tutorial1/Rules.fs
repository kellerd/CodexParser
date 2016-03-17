

namespace Rules
//
//module Try2 =
//    open Domain.WarhammerDomain
//    type Result<'TOk,'TFailure> = 
//        | Ok of 'TOk
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
//        if die >= dPlus then Ok die else Failure die 
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
//        | Ok s -> successFunc s
//        | Failure f -> failureFunc f
//    let succeed x = 
//        Ok x
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
//            | Ok f, Ok x -> Ok(f x)
//            | Failure f, Ok x -> Failure(x)
//            | Ok f, Failure x -> Failure(x)
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
//            Ok x
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
//    let x = makeHitAssault2 m1 target weap d6Check Ok
//    let y = makeWoundsAssault2 m1 target weap d6Check x
//    let z = makeUnsavedWounds m1 target weap d6Check y
//
//    let doAssault assaulter target weapon d6Check = result {
//        let! r = makeHitAssault2 assaulter target weapon d6Check Ok
//        let! t = makeWoundsAssault2 assaulter target weapon d6Check r
//        let! s = makeUnsavedWounds assaulter target weapon d6Check t
//        return s
//    }
//    let xy = result {
//        let! a = makeHitAssault2 <!> (Ok m1) <*> (Ok target) <*> (Ok weap) <*> (Ok d6Check)
//        return a
//    }
//
//    let x1 = doAssault m1 target weap d6Check
//#if INTERACTIVE
//    #r @"C:\Users\Dan\Source\Repos\CodexParser\packages\FSharpx.Extras.1.10.3\lib\40\FSharpx.Extras.dll"
//    #load "Distribution.fs"
//    #load "Domain.fs"
//#else
//namespace Rules
//module RuleEquations = 
//#endif
//    open Domain.WarhammerDomain
//
//    let D = 
//        let r = System.Random()
//        let d num = (r.Next(1,num+1))
//        d
//
//    let getWsTable ws wsopponent = 
//        match ws, wsopponent with 
//            | WeaponSkill ws,  WeaponSkill wsopponent when ws > wsopponent -> 3
//            | WeaponSkill ws,  WeaponSkill wsopponent when wsopponent > ws * 2 -> 5
//            | _ -> 4    
//
//    let getHitsShooting bs  = 
//          (System.Math.Max(7 - bs, 2)) //Redo calculation
//    
//    let getWounds str tough  =
//         let (Strength str') = str
//         let (Toughness tough') = tough
//         match str' - tough' with 
//            | 0 ->  4
//            | 1 ->  3
//            | -1 ->  5
//            | -2 ->  6
//            | -3 ->  6
//            | x when x > 0 ->  2
//            | _ ->  0
//
//    let getUnsavedWounds saves pen  =
//        match pen - saves with
//        | x when x > 0 -> saves
//        | _ ->  2
//#if !INTERACTIVE
//module Try5 =
//    open RuleEquations
//#endif
//    open Domain.WarhammerDomain
//    open Chessie.ErrorHandling
//    open Chessie
//
//    type DiceRoll<'a> = DiceRoll of (unit->'a)
//    type DiceCheck<'a> = DiceCheck of (int->'a)
//    type RollResult<'a> = RollResult of 'a 
//
//    let D6 = 
//        DiceRoll (fun () -> D 6)
//    let D6s =
//        Seq.initInfinite (fun _ -> D6)
//
//    type Rule<'a> = Rule of 'a
//
//    /// type alias (optional)
//    type RuleResult<'a,'b> = Rule<Result<'a,'b>>
//    module Rule =
//        let retn = Rule
//        let apply fR xR = 
//            let (Rule f) = fR
//            let (Rule x) = xR
//            retn (f x)
//        let map  (f:'a->'b) (rule:Rule<'a>) = 
//            let (Rule r) = rule
//            retn (f r)
//        let lift2 (f:'a->'b->'c) (x:Rule<'a>) (y:Rule<'b>) = 
//            let (Rule r) = x
//            let (Rule r2) = y
//            retn (f r r2)
//        let bind (f:'a->Rule<'b>) xRule = 
//            let (Rule x) = xRule
//            f x
//        let run rule input = 
//            let (Rule innerFn) = rule
//            innerFn input
//
//        type RuleBuilder() =
//            member this.Bind(m, f) = bind f m
//            member this.Return(x) = Rule x
//            member this.ReturnFrom(x) = x
//        let rule = new RuleBuilder()
//
//    module RuleResult =
//        
//        let map f (x:RuleResult<'a,'c>) : RuleResult<'b,'c> = 
//            (f |> Trial.lift |> Rule.map ) x
//
//        let lift2 f x y : RuleResult<'c,'d> =
//            (f |> Trial.lift2 |> Rule.lift2) x y
//
//        let retn x : RuleResult<'a,'b> = 
//            x |> ok |> Rule.retn
//
//        let apply (f:RuleResult<'a->'b,'c>) (x:RuleResult<'a,'c>) : RuleResult<'b,'c> = 
//            f |> Rule.bind (fun fResult -> 
//            x |> Rule.map (fun xResult -> 
//            Trial.apply fResult xResult))
//
//        let bind (f:'a->RuleResult<'b,'c>) (x:RuleResult<'a,'c>) : RuleResult<'b,'c> = Rule.rule {
//            let! xResult = x 
//            match xResult with
//            | Ok (x,y) -> return! f x
//            | Bad err -> return (Bad err)
//            }
//        type RuleRBuilder() =
//            member this.Bind(m, f) = bind f m
//            member this.Return(x) = RuleResult x
//            member this.ReturnFrom(x) = x
//        let ruleresult = new RuleRBuilder()
//    type Hits = Hits of int
//    let mapList xs = xs |> List.map (fun z -> RollResult z)
//
//    
//    let makeD6 d6s  =
//        let innerFn dPlus =
//           let die = Seq.head d6s |> (fun (DiceRoll d) -> d())
//           if die >= dPlus then ok die else fail die
//        Rule innerFn
//
//    let makeHit D6 = 
//        let innerFn (wsTarget,wsOpponent) =
//            getWsTable wsTarget wsOpponent 
//                |> Rule.run D6 
//                |> lift (fun roll -> (Hits 1, roll))
//                |> mapFailure (fun roll -> [(Hits 0, roll |> List.head)])
//        Rule innerFn
//    let makeWounds D6  = 
//        let innerFn hits (str,tough)  = 
//            let (Hits hits',_) = hits
//            getWounds str tough 
//                |> Rule.run D6
//                |> lift (fun roll -> (Wounds (1 * hits'), roll))
//                |> mapFailure (fun roll -> [(Wounds 0, roll |> List.head)])
//        Rule innerFn
//
//    let pipeRules (rule1:Rule<'a->'c>) (rule2:RuleResult<'a,'d>) : RuleResult<'c,'d> = 
//        let result = RuleResult.ruleresult {
//            let! result = rule2
//            let returned = match result with 
//                            | Ok (v,m) -> Rule.apply rule1 v
//                            | Bad (err) -> rule1
//            return returned
//        }
//        result
//
//
//
//    let D6R = makeD6 D6s
//    
//    let (<*>) = Rule.apply
//    let mh2 = makeHit D6R <*> Rule (WeaponSkill 4, WeaponSkill 4)
//
//    let mw2 = pipeRules (makeWounds D6R) mh2
