namespace GameImpl
module WarhammerImpl = 
    open Domain.WarhammerDomain
    open FSharpx.Reader
    
    let other player =
        match player with
            | Player1 -> Player2
            | Player2 -> Player1
    let private gameWonBy gameState = 
        let maxPlayerInfo = gameState.Players |> List.sortBy (fun p -> p.Score)
        match maxPlayerInfo with 
            | p1::p2::_ when p1.Score > p2.Score -> Some p1.Player
            | _ -> None
    let private isEndCondition gameState = 
        (gameState.Game.Mission.EndCondition gameState)
    
    let rec removeFirst pred lst =
        match lst with
        | h::t when pred h -> t
        | h::t -> h::removeFirst pred t
        | _ -> []
//
//    let getDisplayInfo (gameState:GameState) =
//        {DisplayInfo.Board = gameState.Board}
    let rec isRunnable = function
        | Rule _ -> true
        | Nested (r,r2) -> 
            seq {yield r; yield r2} |> Seq.exists isRunnable
        | Overwritten (r,_) -> isRunnable r
        | DeactivatedUntilEndOfPhase _ -> false
        | DeactivatedUntilEndOfGame _ -> false
        | Description _ -> false
    let rec collectRules = function
        | Rule e -> [Rule e]
        | Nested (r,r2) -> 
            [r;r2] |> List.map collectRules |> List.collect id
        | Overwritten (r,_) -> collectRules r
        | DeactivatedUntilEndOfPhase _ -> []
        | DeactivatedUntilEndOfGame _ -> []
        | Description _ -> []

    let splitPt = function
                    | Top (x) -> Top, x
                    | Bottom (x) -> Bottom, x
    let splitGt = function
                   | Begin ->    (fun _ -> Begin)   , None
                   | One   x ->  One  , Some x
                   | Two   x ->  Two  , Some x
                   | Three x ->  Three, Some x
                   | Four  x ->  Four , Some x
                   | Five  x ->  Five , Some x
                   | Six   x ->  Six  , Some x
                   | Seven x ->  Seven, Some x
                   | End   ->    ( fun _ -> End), None

    let advancePhase gs = 
        let nextGt x =
            match x with 
            | Begin ->   (GameTurn.One   Phase.Begin)
            | One   _ -> (GameTurn.Two   Phase.Begin)
            | Two   _ -> (GameTurn.Three Phase.Begin)
            | Three _ -> (GameTurn.Four  Phase.Begin)
            | Four  _ -> (GameTurn.Five  Phase.Begin)
            | Five  _ -> (GameTurn.Six   Phase.Begin)
            | Six   _ -> (GameTurn.Seven Phase.Begin)
            | Seven _ -> (GameTurn.End)
            | End   ->   (GameTurn.End)

        let otherPt = function
            | Top(x) -> Phase.Begin |> (splitGt x |> fst) |> Bottom
            | Bottom(x) -> Top (nextGt x)
            
        let changePhase turn = 
            let (PtMaker,gt) = splitPt turn
            let (GtMaker,phase) = splitGt gt
            match phase with
                | Some Phase.Begin ->    PtMaker (GtMaker Phase.Movement )
                | Some Phase.Movement -> PtMaker (GtMaker Phase.Psychic  )
                | Some Phase.Psychic ->  PtMaker (GtMaker Phase.Shooting )
                | Some Phase.Shooting -> PtMaker (GtMaker Phase.Assault  )
                | Some Phase.Assault ->  PtMaker (GtMaker Phase.End      )
                | Some Phase.End  | None -> otherPt turn

        {gs with Game = {gs.Game with Turn = changePhase gs.Game.Turn}}
 
    let findPlayerOfUnit gameState unit = 
        gameState.Players 
            |> List.pick (fun p -> p.Units 
                                            |> List.tryFind(fun u -> u = unit)
                                            |> Option.bind (fun _ -> Some p))
    let replace xs x y = 
        let pred z = x = z
        y :: (removeFirst pred xs)
    let replaceUnit p u nu = {p with Units = replace p.Units u nu }
    let replacePlayer s p np = {s with Players = replace s.Players p np }
    
    let replacePlayer s p np = {s with Players = replace s.Players p np }

    let deployUnit  (widthRange:Range) (heightRange:Range)  unit gameState = reader {
            let! f = ask
            let player = findPlayerOfUnit gameState unit
            let newPlayer = replaceUnit player unit {unit with Deployment = Deployed}

            let newModels = unit.UnitModels 
                                |> List.map (fun m -> { Model= m
                                                        Player = player.Player
                                                        Position =  f widthRange heightRange m })
            let newGs = { gameState with Board = {gameState.Board with Models = newModels @ gameState.Board.Models}}
            return replacePlayer newGs player newPlayer
        }

    let rec eval runReader fs gameState  = 
        match fs with 
            | [] -> gameState
            | h::tail -> 
                    h
                        |> (function
                            | Rule(Function(EndPhase)) -> gameState |> advancePhase
                            | Rule(Function(Deploy(w,h,u))) -> gameState |> deployUnit w h u |> runReader
                            | _ -> gameState) 
                        |> eval runReader tail
//    let rec eval fs (gameState:GameState) = //reader {
//        //let! gs =
//         match fs with 
//                    | [] -> returnM gameState
//                    | h::tail -> reader {
//                                            //let! eval' = 
//                                                eval tail
//                                            let! gs = match h with
//                                                            | Rule(Function(EndPhase)) -> returnM (gameState |> advancePhase)
//                                                            //| Rule(Function(Deploy(w,h,u))) -> gameState |> deployUnit (w,h) u
//                                                            | _ -> returnM gameState
//                                            return gs//}
//       // return gs
//    }


    let updateUnit runReader rule unit gameState  = 
        ///TODO
        let foundPlayer = findPlayerOfUnit gameState unit


        let cUnit f (unit:Unit)  = {unit with Rules = replace unit.Rules f (DeactivatedUntilEndOfPhase f)}


        let newUnit = cUnit rule unit
        let newPlayer = replaceUnit foundPlayer unit newUnit
        let (gs, p, np) = (eval runReader (collectRules rule) gameState), foundPlayer, newPlayer 
        replacePlayer gs p np

    let endPhase = Rule(Function(EndPhase))

    let  availableRuleCapabilities player gs  = 
         gs.Players 
            |> List.filter (fun p -> p.Player = player) 
            |> List.collect (fun p -> p.Units)
            |> List.collect (fun u -> u.Rules |> List.filter isRunnable |> List.map (fun r -> r, Some u))

    let makeNextMoveInfo f (player:Player) gameState (rule,unit) =
        let capability() = f player unit rule gameState 
        match unit with 
            | Some u -> 
                UnitRule ({
                                Unit= u
                                Rule= rule 
                                Capability= capability
                            })
            | None ->
                EndRule {Rule=rule; Capability = capability}
        
    let moveResultFor player gs nextMoves = 
        match player with
        | Player1 -> Player1ToMove (gs, nextMoves)
        | Player2 -> Player2ToMove (gs, nextMoves)
    let makeMoveResultWithCapabilities f player newGameState rulesAndUnits = 
            match rulesAndUnits with 
            | [] -> None
            | rulesAndUnits -> 
                (endPhase, None) :: rulesAndUnits
                |> List.map (makeNextMoveInfo f player newGameState) 
                |> moveResultFor player newGameState |> Some 
    let moveResult gs playerMove currentPlayer  = 
        let result = gs 
                            |> availableRuleCapabilities currentPlayer 
                            |> makeMoveResultWithCapabilities playerMove currentPlayer gs
        match result with
            | Some ruleResult -> ruleResult
            | None -> playerMove currentPlayer None endPhase gs
    let rec playerMove runReader player unit thingToDo gameState = 
        let newGameState = 
                match unit with
                    | Some u -> gameState |> updateUnit runReader thingToDo u
                    | None -> gameState |> eval runReader [thingToDo]
        let newPlayer =
            match gameState.Game.Turn,newGameState.Game.Turn with
                | Top(_),Top(_) -> player
                | Top(_),Bottom(_) -> other player
                | Bottom(_),Top(_) -> other player 
                | Bottom(_),Bottom(_) -> player
        if isEndCondition newGameState then 
            match gameWonBy newGameState with
            | Some player -> GameWon (newGameState, player) 
            | None -> GameTied newGameState 
        else
            newPlayer |> moveResult newGameState (playerMove runReader)


    let newGame runReader () = 
        // create initial game state
        let gameState = Impl.ImplTest.initial
        moveResult gameState (playerMove runReader) Player1
        
        
    /// export the API to the application
    let api runReader = {
        NewGame = (newGame runReader)
        }
