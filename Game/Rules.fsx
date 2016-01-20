//namespace Domain
//
////module Try1 =
////    
////    let makeHitAssault (shooter,target,weapon,d6Check)  =
////        make (getHitsAssault shooter.WeaponSkill target.WeaponSkill d6Check) attempt
////
////    let makeWoundAssault attempt =
////        let (shooter,target,weapon,d6Check,dieResult) = attempt
////        make (getWounds weapon.weaponStrength target.Toughness d6Check) (ignoreRight attempt)
////
////    let makeUnsavedWoundAssault attempt =
////        let (shooter,target,weapon,d6Check,dieResult) = attempt
////        make (getUnsavedWounds weapon.weaponArmorPen target.Saves d6Check) (ignoreRight attempt)
////
////    let subtractWound (shooter,target,weapon, d6Check,dieVal)  =
////        Success (shooter,{target with Wounds = target.Wounds-1})
////
////    let doAssault = 
////        makeHitAssault >=> makeWoundAssault >=> makeUnsavedWoundAssault >=> subtractWound
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

//module Try3 =
//type DiceRoll = seq<int>
//open System
//// Aliases for input, etc
//type Input = DiceRoll   // type alias
//type ParserLabel = string
//type ParserError = string
//
//
///// Stores information about the parser position for error messages
//type ParserPosition = int
//
//let nextDie d6s =
//    (Seq.tryHead d6s, Seq.tail d6s)
//type Result<'a> =
//    | Success of 'a
//    | Failure of ParserLabel * ParserError * 'a option
//
///// A Parser structure has a parsing function & label
//type Parser<'a> = {
//    parseFn : (Input -> Result<'a * Input>)
//    label:  ParserLabel 
//    }
//
//
///// Run the parser on a InputState
//let run parser input = 
//    // call inner function with input
//    parser.parseFn input
//
//        
//let printResult result =
//    match result with
//    | Success (value,input) -> 
//        printfn "%A" value
//    | Failure (label,error, _) -> 
//        printfn "%A err: %A" error label
//
//
//// =============================================
//// Label related
//// =============================================
//
///// get the label from a parser
//let getLabel parser = 
//    // get label
//    parser.label
//
///// update the label in the parser
//let setLabel parser newLabel = 
//    // change the inner function to use the new label
//    let newInnerFn input = 
//        let result = parser.parseFn input
//        match result with
//        | Success s ->
//            // if Success, do nothing
//            Success s 
//        | Failure (oldLabel,err,v) -> 
//            // if Failure, return new label
//            Failure (newLabel,err,v) 
//    // return the Parser
//    {parseFn=newInnerFn; label=newLabel}
//
///// infix version of setLabel
//let ( <?> ) = setLabel
//
//
//// =============================================
//// Standard combinators
//// =============================================
//
///// Match an input token if the predicate is satisfied
//let satisfy predicate label =
//    let innerFn input =
//        let charOpt, remainingInput = nextDie input 
//        match charOpt with
//        | None -> 
//            let err = "No more input"
//            let pos = 0
//            Failure (label,err,None)
//        | Some first -> 
//            if predicate first then
//                Success (first,remainingInput)
//            else
//                let err = sprintf "Failed to pass test, got: %A" first
//                let pos = 1
//                Failure (label,err,Some (first,remainingInput))
//    // return the parser
//    {parseFn=innerFn;label=label}
//
///// "bindP" takes a parser-producing function f, and a parser p
///// and passes the output of p into f, to create a new parser
//let bindP (f:'a->Parser<'b>) (p:Parser<'a>) : Parser<'b> =
//    let label = "unknown"
//    let innerFn input =
//        let result1 = run p input 
//        match result1 with
//        | Failure (label,err,_) -> 
//             result1
//        | Success (value1,remainingInput) ->
//            // apply f to get a new parser
//            let p2 = f value1
//            // run parser with remaining input
//            run p2 remainingInput
//            
//    {parseFn=innerFn; label=label}
//
///// Infix version of bindP
//let ( >>= ) p f = bindP f p
//
///// Lift a value to a Parser
//let returnP x = 
//    let label = sprintf "%A" x
//    let innerFn input =
//        // ignore the input and return x
//        Success (x,input)
//    // return the inner function
//    {parseFn=innerFn; label=label}
//
///// apply a function to the value inside a parser
//let mapP f = 
//    bindP (f >> returnP)
//
///// infix version of mapP
//let ( <!> ) = mapP
//
///// "piping" version of mapP
//let ( |>> ) x f = mapP f x
//
///// apply a wrapped function to a wrapped value
//let applyP fP xP =         
//    fP >>= (fun f -> 
//    xP >>= (fun x -> 
//        returnP (f x) ))
//
///// infix version of apply
//let ( <*> ) = applyP
//
///// lift a two parameter function to Parser World
//let lift2 f xP yP =
//    returnP f <*> xP <*> yP
//
///// Combine two parsers as "A andThen B"
//let andThen p1 p2 =         
//    let label = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
//    p1 >>= (fun p1Result -> 
//    p2 >>= (fun p2Result -> 
//        returnP (p1Result,p2Result) ))
//    <?> label
//
///// Infix version of andThen
//let ( .>>. ) = andThen
//
///// Combine two parsers as "A orElse B"
//let orElse p1 p2 =
//    let label = sprintf "%s orElse %s" (getLabel p1) (getLabel p2)
//    let innerFn input =
//        // run parser1 with the input
//        let result1 = run p1 input
//
//        // test the result for Failure/Success
//        match result1 with
//        | Success result -> 
//            // if success, return the original result
//            result1
//
//        | Failure _ -> 
//            // if failed, run parser2 with the input
//            let result2 = run p2 input
//
//            // return parser2's result
//            result2 
//
//    // return the inner function
//    {parseFn=innerFn; label=label}
//
///// Infix version of orElse
//let ( <|> ) = orElse
//
///// Choose any of a list of parsers
//let choice listOfParsers = 
//    List.reduce ( <|> ) listOfParsers 
//
//let rec sequence parserList =
//    // define the "cons" function, which is a two parameter function
//    let cons head tail = head::tail
//
//    // lift it to Parser World
//    let consP = lift2 cons
//
//    // process the list of parsers recursively
//    match parserList with
//    | [] -> 
//        returnP []
//    | head::tail ->
//        consP head (sequence tail)
//
///// (helper) match zero or more occurences of the specified parser
//let rec parseZeroOrMore parser input =
//    // run parser with the input
//    let firstResult = run parser input 
//    // test the result for Failure/Success
//    match firstResult with
//    | Failure (_,_,_) -> 
//        // if parse fails, return empty list
//        ([],input)  
//    | Success (firstValue,inputAfterFirstParse) -> 
//        // if parse succeeds, call recursively
//        // to get the subsequent values
//        let (subsequentValues,remainingInput) = 
//            parseZeroOrMore parser inputAfterFirstParse
//        let values = firstValue::subsequentValues
//        (values,remainingInput)  
//
///// matches zero or more occurences of the specified parser
//let many parser = 
//    let label = sprintf "many %s" (getLabel parser)
//    let rec innerFn input =
//        // parse the input -- wrap in Success as it always succeeds
//        Success (parseZeroOrMore parser input)
//    {parseFn=innerFn; label=label}
//
///// matches one or more occurences of the specified parser
//let many1 p =         
//    let label = sprintf "many1 %s" (getLabel p)
//
//    p      >>= (fun head -> 
//    many p >>= (fun tail -> 
//        returnP (head::tail) ))
//    <?> label
//
///// Parses an optional occurrence of p and returns an option value.
//let opt p = 
//    let label = sprintf "opt %s" (getLabel p)
//    let some = p |>> Some
//    let none = returnP None
//    (some <|> none) <?> label
//
///// Keep only the result of the left side parser
//let (.>>) p1 p2 = 
//    // create a pair
//    p1 .>>. p2 
//    // then only keep the first value
//    |> mapP (fun (a,b) -> a) 
//
///// Keep only the result of the right side parser
//let (>>.) p1 p2 = 
//    // create a pair
//    p1 .>>. p2 
//    // then only keep the second value
//    |> mapP (fun (a,b) -> b) 
//
///// Keep only the result of the middle parser
//let between p1 p2 p3 = 
//    p1 >>. p2 .>> p3 
//
///// Parses one or more occurrences of p separated by sep
//let sepBy1 p sep =
//    let sepThenP = sep >>. p            
//    p .>>. many sepThenP 
//    |>> fun (p,pList) -> p::pList
//
///// Parses zero or more occurrences of p separated by sep
//let sepBy p sep =
//    sepBy1 p sep <|> returnP []
//
//// =============================================
//// Standard parsers 
//// =============================================
//
//
//// ------------------------------
//// char and string parsing
//// ------------------------------
//            
/////// parse a char 
////let pchar charToMatch = 
////    // label is just the character
////    let label = sprintf "%c" charToMatch 
////
////    let predicate ch = (ch = charToMatch) 
////    satisfy predicate label 
////
/////// Choose any of a list of characters
////let anyOf listOfChars = 
////    let label = sprintf "anyOf %A" listOfChars 
////    listOfChars
////    |> List.map pchar // convert into parsers
////    |> choice
////    <?> label


////Tests 
//let checkD6 dPlus = fun d6 -> d6 >= dPlus
//
//
//let guardArmor = 
//    let label = "Guard armor check"
//    satisfy (checkD6 4) label
//let termieArmor =
//    let label = "Term armor check"
//    satisfy (checkD6 2) label
//
//let test parser =
//    let dicePool = 
//        let rnd = System.Random()
//        Seq.initInfinite (fun _ -> rnd.Next(1,7)) |> Seq.cache
//    run parser dicePool
//
//test (termieArmor .>>. guardArmor)


//Try 4

#r "C:\Users\Dan\Source\Repos\CodexParser\packages\FSharpx.Extras.1.10.3\lib\40\FSharpx.Extras.dll"


type Characteristic = int
    
type WeaponSkill = Characteristic
type BallisticSkill = Characteristic
type Strength = Characteristic
type Toughness = Characteristic
type Saves = Characteristic
type ArmorPen = Characteristic
type Attacks = Characteristic
type InvSaves = Characteristic
type Wounds = Characteristic

type Model = {
    Name       : string;
    WeaponSkill       : WeaponSkill;
    BallisticSkill    : BallisticSkill;
    Strength          : Strength;
    Toughness         : Toughness;
    Saves             : Saves;
    Wounds            : Wounds;
} 


type GameState = Map<string, Model>



#nowarn "1189"
   
/// Given a readonly state, produces a value
type Reader<'TState, 'T> = 'TState -> 'T
/// Produces a value together with additional state
type Writer<'TState, 'T> = 'TState * 'T
/// Given state, produces new state & a value
type State<'TState, 'T>  = 'TState -> 'TState * 'T

type ReaderUpdate = 
    | NoUpdate
    static member Unit = NoUpdate
    static member Combine(NoUpdate, NoUpdate) = NoUpdate
    static member Apply(s, NoUpdate) = s
/// Represents an update monad - given a state, produce 
/// value and an update that can be applied to the state
type UpdateMonad<'TState, 'TUpdate, 'T> = 
    UM of ('TState -> 'TUpdate * 'T)

/// Returns the value of 'Unit' property on the ^S type
let inline unit< ^S when ^S : (static member Unit : ^S)> () : ^S = (^S : (static member Unit : ^S) ()) 

/// Invokes Combine operation on a pair of ^S values
let inline (++)< ^S when ^S : (static member Combine : ^S * ^S -> ^S )> a b : ^S = (^S : (static member Combine : ^S * ^S -> ^S) (a, b)) 

/// Invokes Apply operation on state and update ^S * ^U
let inline apply< ^S, ^U when ^U : (static member Apply : ^S * ^U -> ^S )> s a : ^S = (^U : (static member Apply : ^S * ^U -> ^S) (s, a)) 

type UpdateBuilder() = 
    /// Returns the specified value, together
    /// with empty update obtained using 'unit'
    member inline x.Return(v) : UpdateMonad<'S, 'U, 'T> = 
        UM (fun s -> (unit(),v))

    /// Compose two update monad computations
    member inline x.Bind(UM u1, f:'T -> UpdateMonad<'S, 'U, 'R>) =  
        UM (fun s -> 
            // Run the first computation to get first update
            // 'u1', then run 'f' to get second computation
            let (u1, x) = u1 s
            let (UM u2) = f x
            // Apply 'u1' to original state & run second computation
            // then return result with combined state updates
            let (u2, y) = u2 (apply s u1)
            (u1 ++ u2, y))
    member x.Map(f,a) =
        //let fR = f >> x.Return
        x.Bind (a, f >> x.Return)
/// Instance of the computation builder
/// that defines the update { .. } block
let update = UpdateBuilder()
   
/// Trivial monoid of updates 

/// Read the current state (int) and return it as 'int'
let read = UM (fun (s) -> (NoUpdate, s))
/// Run computation and return the result 
let readRun (s) (UM f) = f s |> snd
   
/// Writer monad has no readable state
type WriterState = NoState

/// Updates of writer monad form a list
type WriterUpdate<'TLog> = 
    | Log of list<'TLog>
    /// Returns the empty log (monoid unit)
    static member Unit = Log []
    /// Combines two logs (operation of the monoid)
    static member Combine(Log a, Log b) = Log(List.append a b)
    /// Applying updates to state does not affect the state
    static member Apply(NoState, _) = NoState
   
/// Writes the specified value to the log 
let write v = UM (fun s -> (Log [v], ()))
/// Runs a "writer monad computation" and returns 
/// the log, together with the final result
let writeRun (UM f) = let (Log l, v) = f NoState in l, v

/// Extends UpdateBuilder to support additional syntax
type UpdateBuilder with
    /// Represents monadic computation that returns unit
    /// (e.g. we can now omit 'else' branch in 'if' computation)
    member inline x.Zero() = x.Return(())

    /// Delays a computation with (uncontrolled) side effects
    member inline x.Delay(f) = x.Bind(x.Zero(), f)

    /// Sequential composition of two computations where the
    /// first one has no result (returns a unit value)
    member inline x.Combine(c1, c2) = x.Bind(c1, fun () -> c2)

    /// Enable the 'return!' keyword to return another computation
    member inline x.ReturnFrom(m : UpdateMonad<'S, 'P, 'T>) = m

    /// Ensure that resource 'r' is disposed of at the end of the
    /// computation specified by the function 'f'
    member inline x.Using(r,f) = UM(fun s -> use rr = r in let (UM g) = f rr in g s)

    /// Support 'for' loop - runs body 'f' for each element in 'sq'
    member inline x.For(sq:seq<'V>, f:'V -> UpdateMonad<'S, 'P, unit>) = 
        let rec loop (en:System.Collections.Generic.IEnumerator<_>) = 
            if en.MoveNext() then x.Bind(f en.Current, fun _ -> loop en)
            else x.Zero()
        x.Using(sq.GetEnumerator(), loop)

    /// Supports 'while' loop - run body 'f' until condition 't' holds
    member inline x.While(t, f:unit -> UpdateMonad<'S, 'P, unit>) =
        let rec loop () = 
            if t() then x.Bind(f(), loop)
            else x.Zero()
        loop()
   
/// Wraps a state of type 'T
type StateState<'T> = State of 'T

/// Represents updates on state of type 'T
type StateUpdate<'T> = 
    | Set of 'T | SetNop
    /// Empty update - do not change the state
    static member Unit = SetNop
    /// Combine updates - return the latest (rightmost) 'Set' update
    static member Combine(a, b) = 
        match a, b with 
        | SetNop, v | v, SetNop -> v 
        | Set a, Set b -> Set b
    /// Apply update to a state - the 'Set' update changes the state
    static member Apply(s, p) = 
        match p with SetNop -> s | Set s -> State s

/// Set the state to the specified value
let set s = UM (fun _ -> (Set s,()))
/// Get the current state 
let get = UM (fun (State s) -> (SetNop, s))
/// Run a computation using a specified initial state
let setRun s (UM f) = f (State s) |> snd



          

let killModel = update {
    let! n = get
    let rm = (fun k -> Map.remove k n)
    return rm
}

let models:GameState =   [|"Hunter 1", {Name="Hunter 1"; WeaponSkill = 4;BallisticSkill   = 4;Strength         = 4;Toughness        = 4;Saves            = 4;Wounds           = 4;};
                         "Hunter 2",   {Name="Hunter 2"; WeaponSkill = 4;BallisticSkill   = 4;Strength         = 4;Toughness        = 4;Saves            = 4;Wounds           = 4;}; |] |> Map.ofArray

let select = update {
    let! (target:GameState) = read
    return target.Item("Hunter 1")
} 

let doGame modelToKill = update {
    let! (x:GameState) = get
    let! km = killModel
    let! r = modelToKill
    return km r.Name
} 
let doGame2 modelToKill = update {
    let! (x:GameState) = get
    let! km = killModel
    let! r = modelToKill
    return km r
} 
let result =  setRun models (doGame <| update { return  readRun models select })
let result2 =  setRun models (doGame2 <| update { return update.Map ((fun a -> a.Name),select) |>  readRun models  })
let x = update.Map ((fun a -> a.Name),select) |> readRun models 
