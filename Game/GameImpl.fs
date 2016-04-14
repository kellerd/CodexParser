﻿namespace GameImpl

module WarhammerImpl = 
    open Domain.WarhammerDomain
    
    let other player = 
        player |> function 
        | Player1 -> Player2
        | Player2 -> Player1
    
    let private gameWonBy gameState = 
        let maxPlayerInfo = gameState.Players |> List.sortBy (fun p -> p.Score)
        match maxPlayerInfo with
        | p1 :: p2 :: _ when p1.Score > p2.Score -> Some p1.Player
        | _ -> None
    
    let private isEndCondition gameState = (gameState.Game.Mission.EndCondition gameState)
    let pick nextMoveInfos = nextMoveInfos |> List.tryHead
    
    let rec removeFirst pred lst = 
        match lst with
        | h :: t when pred h -> t
        | h :: t -> h :: removeFirst pred t
        | _ -> []
    
    //
    //    let getDisplayInfo (gameState:GameState) =
    //        {DisplayInfo.Board = gameState.Board}
    let rec isRunnable = 
        function 
        | Rule _ -> true
        | Nested(r, r2) -> 
            seq { 
                yield r
                yield r2
            }
            |> Seq.exists isRunnable
        | Overwritten(r, _) -> isRunnable r
        | DeactivatedUntilEndOfPhase _ -> false
        | DeactivatedUntilEndOfGame _ -> false
        | Description _ -> false
    
    let rec collectRules = 
        function 
        | Rule e -> [ Rule e ]
        | Nested(r, r2) -> 
            [ r; r2 ]
            |> List.map collectRules
            |> List.collect id
        | Overwritten(r, _) -> collectRules r
        | DeactivatedUntilEndOfPhase _ -> []
        | DeactivatedUntilEndOfGame _ -> []
        | Description _ -> []
    
    let splitPt = 
        function 
        | Top(x) -> Top, x
        | Bottom(x) -> Bottom, x
    
    let splitGt = 
        function 
        | Begin -> (fun _ -> Begin), None
        | One x -> One, Some x
        | Two x -> Two, Some x
        | Three x -> Three, Some x
        | Four x -> Four, Some x
        | Five x -> Five, Some x
        | Six x -> Six, Some x
        | Seven x -> Seven, Some x
        | End -> (fun _ -> End), None
    
    let replace xs x y = 
        let pred z = x = z
        y :: (removeFirst pred xs)
    
    let replacePlayerUnits p u nu = { p with Units = replace p.Units u nu }
    let replaceGameStatePlayers s p np = { s with Players = replace s.Players p np }
    
    let findPlayer gameState unit = 
        gameState.Players |> List.tryPick (fun p -> 
                                 p.Units
                                 |> List.tryFind (fun u -> u = unit)
                                 |> Option.bind (fun _ -> Some p))
    
    let updatePlayerInGameState unit newUnit gameState = 
        let foundPlayer = findPlayer gameState unit
        let newPlayer = foundPlayer |> Option.map (fun p -> replacePlayerUnits p unit newUnit)
        match gameState, foundPlayer, newPlayer with
        | (gs, Some p, Some np) -> replaceGameStatePlayers gs p np
        | (gs, _, _) -> gs
    
    let deployModels positionAsker ms p gameState = 
        [ for m in ms do
              yield { Model = m
                      Player = p
                      Position = gameState |> positionAsker } ]
    
    let deploy u gameState positionAsker = 
        let foundPlayer = findPlayer gameState u
        let newUnit = { u with Deployment = Deployed }
        match foundPlayer with
        | Some p -> 
            { gameState with Board = 
                                 { gameState.Board with Models = 
                                                            (deployModels positionAsker u.UnitModels p.Player gameState) 
                                                            @ gameState.Board.Models } } 
            |> updatePlayerInGameState u newUnit
        | None -> failwith "Couldn't find player"
    
    let advancePhase gs = 
        let nextGt x = 
            match x with
            | Begin -> (GameTurn.One Phase.Begin)
            | One _ -> (GameTurn.Two Phase.Begin)
            | Two _ -> (GameTurn.Three Phase.Begin)
            | Three _ -> (GameTurn.Four Phase.Begin)
            | Four _ -> (GameTurn.Five Phase.Begin)
            | Five _ -> (GameTurn.Six Phase.Begin)
            | Six _ -> (GameTurn.Seven Phase.Begin)
            | Seven _ -> (GameTurn.End)
            | End -> (GameTurn.End)
        
        let enableDeactivatedRules gameState = 
            let rec enableRule = 
                function 
                | Nested(r, r2) -> Nested(enableRule r, enableRule r2)
                | DeactivatedUntilEndOfPhase r -> r
                | r -> r
            
            let newUnit (unit : Unit) = { unit with Rules = List.map enableRule unit.Rules }
            let newGameState = 
                gameState.Players 
                |> List.fold 
                       (fun acc p -> 
                       p.Units |> List.fold (fun acc2 unit -> updatePlayerInGameState unit (newUnit unit) acc2) acc) 
                       gameState
            newGameState.Players
        
        let otherPt = 
            function 
            | Top(x) -> 
                Phase.Begin
                |> (splitGt x |> fst)
                |> Bottom
            | Bottom(x) -> Top(nextGt x)
        
        let changePhase turn = 
            let (PtMaker, gt) = splitPt turn
            let (GtMaker, phase) = splitGt gt
            match phase with
            | Some Phase.Begin -> PtMaker(GtMaker Phase.Movement)
            | Some Phase.Movement -> PtMaker(GtMaker Phase.Psychic)
            | Some Phase.Psychic -> PtMaker(GtMaker Phase.Shooting)
            | Some Phase.Shooting -> PtMaker(GtMaker Phase.Assault)
            | Some Phase.Assault -> PtMaker(GtMaker Phase.End)
            | Some Phase.End | None -> otherPt turn
        
        { gs with Game = { gs.Game with Turn = changePhase gs.Game.Turn }
                  Players = gs |> enableDeactivatedRules }
    
    let rec eval fs positionAsker u gameState = 
        match fs with
        | [] -> gameState
        | h :: tail -> 
            (h, u)
            |> (function 
            | Rule(Function(EndPhase)), _ -> advancePhase gameState
            | Rule(Function(Deploy)), Some unit -> deploy unit gameState positionAsker
            | _ -> gameState)
            |> eval tail positionAsker u
    
    let disableRuleOnUnit f (unit : Unit) positionAsker gameState = 
        let newUnit = { unit with Rules = replace unit.Rules f (DeactivatedUntilEndOfPhase f) }
        let newGameState = updatePlayerInGameState unit newUnit gameState
        eval (collectRules f) positionAsker (Some newUnit) newGameState
    
    let endPhase = Rule(Function(EndPhase))
    
    let availableRuleCapabilities player gs = 
        gs.Players
        |> List.filter (fun p -> p.Player = player)
        |> List.collect (fun p -> p.Units)
        |> List.collect (fun u -> 
               u.Rules
               |> List.filter isRunnable
               |> List.map (fun r -> r, Some u))
    
    let makeNextMoveInfo f (player : Player) gameState (rule, unit) = 
        let capability() = f player unit rule gameState
        match unit with
        | Some u -> 
            UnitRule({ Unit = u
                       Rule = rule
                       Capability = capability })
        | None -> 
            EndRule { Rule = rule
                      Capability = capability }
    
    let moveResultFor player gs nextMoves = 
        match player with
        | Player1 -> Player1ToMove(gs, nextMoves)
        | Player2 -> Player2ToMove(gs, nextMoves)
    
    let makeMoveResultWithCapabilities f player newGameState rulesAndUnits = 
        match rulesAndUnits with
        | [] -> None
        | rulesAndUnits -> 
            (endPhase, None) :: rulesAndUnits
            |> List.map (makeNextMoveInfo f player newGameState)
            |> moveResultFor player newGameState
            |> Some
    
    let moveResult gs playerMove currentPlayer = 
        let result = 
            gs
            |> availableRuleCapabilities currentPlayer
            |> makeMoveResultWithCapabilities playerMove currentPlayer gs
        match result with
        | Some ruleResult -> ruleResult
        | None -> playerMove currentPlayer None endPhase gs
    
    let rec playerMove positionAsker player unit thingToDo gameState = 
        let newGameState = 
            match unit with
            | Some u -> gameState |> disableRuleOnUnit thingToDo u positionAsker
            | None -> gameState |> eval [ thingToDo ] positionAsker None
        
        let newPlayer = 
            match gameState.Game.Turn, newGameState.Game.Turn with
            | Top(_), Top(_) -> player
            | Top(_), Bottom(_) -> other player
            | Bottom(_), Top(_) -> other player
            | Bottom(_), Bottom(_) -> player
        
        if isEndCondition newGameState then 
            match gameWonBy newGameState with
            | Some player -> GameWon(newGameState, player)
            | None -> GameTied newGameState
        else newPlayer |> moveResult newGameState (playerMove positionAsker)
    
    let newGame positionAsker () = 
        // create initial game state
        let gameState = Impl.ImplTest.initial
        moveResult gameState (playerMove positionAsker) Player1
    
    /// export the API to the application
    let api positionAsker = { NewGame = newGame positionAsker }
