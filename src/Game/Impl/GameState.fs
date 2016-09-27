namespace GameImpl 

open FSharpx.Collections
open Domain.Board
open Microsoft.FSharp.Collections
open Domain

type GameStateComputation<'a> = 
    GameStateComputation of (GameState -> 'a * GameState)

/// Functions that work with GameStateComputation 
[<AutoOpen>]
module GameStateComputation = 

    let runT game state = 
        // pattern match against the Game
        // to extract the inner function
        let (GameStateComputation innerFn) = game 
        // run the inner function with the passed in state
        innerFn state

    let returnT x = 
        let innerFn state =
            (x,state)
        GameStateComputation innerFn 

    let bindT f xT = 
        let innerFn state =
            let x,state2 = runT xT state
            runT (f x) state2
        GameStateComputation innerFn 

    let getState = (fun s -> (s,s)) |> GameStateComputation
    let putState s = (fun _ -> ((),s)) |> GameStateComputation
    let eval m s = m s |> fst
    let exec m s = m s |> snd

    let toComputation f = 
        let innerFn state =
            let (result,newState) = f state
            (result,newState)
        GameStateComputation innerFn 

    let toUnitComputation f = 
        let f2 state = 
            (),f state
        toComputation f2

    // define a computation expression builder
    type GameBuilder() =
        member this.Return(x) = returnT x
        member this.ReturnFrom(x) = x
        member this.Bind(x,f) = bindT f x
        member this.Zero(x) = returnT ()

    // create an instance of the computation expression builder
    let game = GameBuilder()


    /// Inject a value into the monadic type
    let inline returnMB builder x = (^M: (member Return: 'b -> 'c) (builder, x))
    let inline bindM builder m f = (^M: (member Bind: 'd -> ('e -> 'c) -> 'c) (builder, m, f))
    let inline liftM builder f m =
        let inline ret x = returnMB builder (f x)
        bindM builder m ret

    /// Sequential application
    let inline applyM (builder1:^M1) (builder2:^M2) f m =
        bindM builder1 f <| fun f' ->
            bindM builder2 m <| fun m' ->
                returnMB builder2 (f' m') 
    open Operators
    
    /// Sequentially compose two actions, passing any value produced by the second as an argument to the first.
    let inline bind f m = game.Bind(m,f)
    /// Inject a value into the async type
    let inline returnM x = returnMB game x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM game m f
    /// Flipped >>=
    let inline (=<<) f m = bindM game m f
    /// Sequential application
    let inline (<*>) f m = applyM game game f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Flipped map
    let inline pipe m f = liftM game f m
    let inline pipe2 x y f = returnM f <*> x <*> y
    let inline pipe3 x y z f = returnM f <*> x <*> y <*> z
    /// Transforms an game value by using a specified mapping function.
    let inline map f m = pipe m f
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f x y = returnM f <*> x <*> y
    /// Infix map
    let inline (<!>) f m = pipe m f
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = pipe2 x y (fun _ z -> z)
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = pipe2 x y (fun z _ -> z)

    /// Sequentially compose two game actions, discarding any value produced by the first
    let inline (>>.) m f = bindM game m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    let inline flip f x y = f y x
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

    let inline optBindM f x = 
        Option.map f x |> defaultArg <| returnM None

module Option =
    let traverseGameA f opt =

        // define the applicative functions
        let (<*>) = GameStateComputation.ap
        let retn = GameStateComputation.returnM

        // loop through the option
        match opt with
        | None -> 
            // if empty, lift None to an Result
            retn None
        | Some x -> 
            // lift value to an Result
            (retn Some) <*> (f x) 
    
    let sequenceResultA x = traverseGameA id x


    let optionSequenceTuple opt =
        // define the applicative functions
        let (<*>) = ap 
        let retn = returnM 

        // right fold over the option
        let initState = retn None
        let folder x _ = (retn Some) <*> x 

        Option.foldBack folder opt initState 

    let traverseResultM f list =

        let (>>=) x f = bind f x
        let retn = returnM
        let cons head tail = head :: tail
        let initState = retn []
        let folder head tail = 
            f head >>= (fun h -> 
            tail >>= (fun t ->
            retn (cons h t) ))

        Option.foldBack folder list initState 

module GameState = 

    let tryFindUnit unitId =  pipe getState (fun gameState -> gameState.Players |> List.tryPick (fun p -> p.Units |> Map.tryFind unitId))
    let tryFindUnitByModel (model:Model) = 
        pipe getState (fun gameState -> gameState.Players |> List.choose(fun p -> p.Units |> Map.tryPick (fun _ u  -> u.UnitModels |> Map.tryFind model.Id |> Option.bind(fun _ -> Some u))) |> List.tryHead)
    let tryFindPlayer unit = 
         pipe getState (fun gameState -> 
                gameState.Players |> List.tryPick (fun p -> 
                                                            p.Units
                                                            |> Map.tryFind unit.Id
                                                            |> Option.bind (fun _ -> Some p)))
    let tryFindModel mId  = pipe getState (fun gameState -> gameState.Board.Models |> Map.tryFind mId)

    let rec removeFirst pred lst = 
        match lst with
        | h :: t when pred h -> t
        | h :: t -> h :: removeFirst pred t
        | _ -> []

    let replace xs x y = 
        let pred z = x = z
        y :: (removeFirst pred xs)
    
    let def x _ = Some x
    let defnot x _ = None
    let replaceUnitModels u (m:Model) nm = { u with UnitModels = Map.updateWithOrRemove nm m.Id u.UnitModels } 
    let replacePlayerUnits p u nu = { p with Units = Map.updateWithOrRemove nu u.Id p.Units }
    let replaceGameStatePlayers p np = 
        let innerFn gameState = 
            { gameState with Players = replace gameState.Players p np }
        toUnitComputation innerFn 
    let rec tryFindRule ruleApplication : GameStateComputation<Rule option> = game {
        let! gameState = getState 
        match ruleApplication with
        | UnitRule(rule,uId) -> 
            let name = rule.ToString()
            return! pipe (tryFindUnit uId) (Option.bind(fun u -> u.Rules |> Map.tryFind name))
        | ModelRule(rule,mId) ->
            let name = rule.ToString()
            return! pipe (tryFindModel mId) (Option.bind(fun m -> m.Model.Rules |> Map.tryFind name))
        | GameStateRule(rule) ->
            let name = rule.ToString()
            return (gameState.Rules |> Map.tryFind name)
        | Sequence(rule::_) ->
            return! tryFindRule rule  
        | Sequence([]) -> return None
    }
    let replaceUnitModelsInPlayer p u m nm = def (replaceUnitModels u m nm) |> replacePlayerUnits p u
    let replaceUnitModelsInGameState p u m nm = replaceUnitModelsInPlayer p u m nm |> replaceGameStatePlayers p 
    let updateModelInBoard (model:Model) newModel = 
        let innerFn gameState =
            {gameState with Board = {gameState.Board with Models = Map.updateWithOrRemove (Option.bind(fun mi -> Some mi.Model |> newModel |> Option.map(fun m -> {mi with Model = m}))) model.Id gameState.Board.Models}}
        toUnitComputation innerFn 

    let updatePlayerInGameState unit newUnit = game {
        let! foundPlayer = tryFindPlayer unit
        let newPlayer = foundPlayer |> Option.map (fun p -> replacePlayerUnits p unit newUnit)
        match foundPlayer, newPlayer with
        | (Some p, Some np) -> do! replaceGameStatePlayers p np
        | (_, _) -> ()  
    }
    let updateUnitInGameState (model:Model) newmodel  = game {
            let! foundUnit = tryFindUnitByModel model
            let newUnit = foundUnit |> Option.map (fun p -> replaceUnitModels p model newmodel)
            match foundUnit, newUnit with
            | (Some u, Some nu) ->  
                do! updatePlayerInGameState u (def nu) 
                do! updateModelInBoard model newmodel                                       
            | (_, _) -> ()   
        }
    let replaceRuleOnGameState replace  = 
        let innerFn gameState =
            printfn "before: %A" gameState.Rules 
            let newGameState = { gameState with Rules = gameState.Rules |> replace }
            printfn "after: %A" gameState.Rules
            newGameState
        toUnitComputation innerFn 
    let tryReplaceRuleOnGameState mapf rule  = game {
            let name = makeRule rule |> fst
            printfn "%s" name
            do! replaceRuleOnGameState (Map.updateWithOrRemove (mapf rule) name) 
        }
    let replaceRuleOnUnit (unit : Unit) replace = game {
            let newUnit = { unit with Rules = unit.Rules |> replace }
            do! updatePlayerInGameState unit (def newUnit) 
        }
    let tryReplaceRuleOnUnit mapf rule uid  = game {
        let name = makeRule rule |> fst
        let! unit = tryFindUnit uid
        match unit with
        | Some u -> do! replaceRuleOnUnit u (Map.updateWithOrRemove (mapf rule) name) 
        | None -> ()
    }
         
    let replaceRuleOnModel  (model : Model) replace  = game {
            let newmodel = { model with Rules = model.Rules |> replace }
            do! updateUnitInGameState model (def newmodel)
        }
    let tryReplaceRuleOnModel mapf rule mId  = game {
        let name = makeRule rule |> fst
        let! model = tryFindModel mId
        match model with
        | Some m -> do! replaceRuleOnModel m.Model (Map.updateWithOrRemove (mapf rule) name)
        | None -> ()
    }

    let forAllModels f newUnit  = game {
        let! gameState = getState
        let models = 
            [ for m in newUnit.UnitModels do
                yield runT (f (returnM m.Value)) gameState |> fst ]
            |> List.fold (fun acc m -> match Map.tryFind m.Model.Id acc with
                                        | Some _ -> Map.updateWithOrRemove (def m) m.Model.Id acc
                                        | None -> Map.add m.Model.Id m acc) gameState.Board.Models
        do! putState { gameState with Board = { gameState.Board with Models = models }}
    }


