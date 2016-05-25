﻿namespace GameImpl
module Map =
    let pickKeyOfItem item map = Map.pick (fun k ur -> if item = ur then Some k  else None) map
    let replace f x oldKey = (Map.remove oldKey >> Map.add oldKey (f x))
module WarhammerImpl = 
    open Domain.WarhammerDomain
    let other player = 
        player |> function 
        | Player1 -> Player2
        | Player2 -> Player1
    
    let private (|Leader|Tied|) gameState = 
        let maxPlayerInfo = gameState.Players |> List.sortBy (fun p -> p.Score)
        match maxPlayerInfo with
        | p1 :: p2 :: _ when p1.Score > p2.Score -> Leader p1.Player
        | _ -> Tied
    
    let private (|EndGame|_|) = function 
        | GameStateResult gameState -> if gameState.Game.Mission.EndCondition gameState then Some gameState else None
        | _ -> None
    let pick nextMoveInfos = nextMoveInfos |> function
                                                | [] -> None
                                                | x :: _ -> Some x
    
    let tryFindNewUnit gameState unitId = 
        gameState.Players |> List.tryPick (fun p -> unitId |> Option.bind(fun unitId -> p.Units |> Map.tryFind unitId))
    let tryFindPlayer gameState unit = 
        gameState.Players |> List.tryPick (fun p -> 
                                                     p.Units
                                                     |> Map.tryFind unit.Id
                                                     |> Option.bind (fun _ -> Some p))
    let findModel mId gameState  = gameState.Board.Models |> Map.find mId

    let rec removeFirst pred lst = 
        match lst with
        | h :: t when pred h -> t
        | h :: t -> h :: removeFirst pred t
        | _ -> []
    
    let collectRules (gs:GameState) = 
        let isActive = function
            ///Todo find nested rules -> Depends on how they are nested.
            | UnitApplication (r, ug) -> tryFindNewUnit gs (Some ug) |> Option.bind (fun u -> u.Rules |> Map.tryFind r.ToString)
            | ModelApplication (r, mg) -> findModel mg gs |> fun mi -> mi.Model.Rules |> Map.tryFind r.ToString
            | GameStateApplication r -> gs.Rules |> Map.tryFind r.ToString
        let rec collectRest = function
            | Function e -> [ Function e ]
            | Nested(r, r2) -> 
                [ r; r2 ]
                |> List.collect collectRest
            | Overwritten(r, _) -> collectRest r
            | DeactivatedUntilEndOfPhase _ -> []
            | DeactivatedUntilEndOfGame _ -> []
            | Description _ -> []
            | DeactivatedUntilEndOfPhaseOnFirstUse _ as r -> [r]
            | DeactivatedUntilEndOfGameOnFirstUse _ as r -> [r]
            | Characteristic _ -> []
            | ActiveWhen (ra,r) -> match isActive ra with
                                    | Some _ -> [r]
                                    | None -> []
        collectRest
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
    
    let replacePlayerUnits p u nu = { p with Units = Map.replace id nu u.Id p.Units }
    let replaceGameStatePlayers s p np = { s with Players = replace s.Players p np }

    let updatePlayerInGameState unit newUnit gameState = 
        let foundPlayer = tryFindPlayer gameState unit
        let newPlayer = foundPlayer |> Option.map (fun p -> replacePlayerUnits p unit newUnit)
        match gameState, foundPlayer, newPlayer with
        | (gs, Some p, Some np) -> replaceGameStatePlayers gs p np
        | (gs, _, _) -> gs

    let forAllModels f newUnit gameState = 
        [ for m in newUnit.UnitModels do
                yield f m.Value ]
        |> List.fold (fun acc m -> match Map.tryFind m.Model.Id acc with
                                    | Some _ -> Map.replace id m m.Model.Id acc
                                    | None -> Map.add m.Model.Id m acc) gameState.Board.Models
    let deploy u gameState  = 
        let foundPlayer = tryFindPlayer gameState u
        let newUnit = { u with Deployment = Deployed }
        match foundPlayer with
        | Some p -> 
            fun positionAsker -> 
                { gameState with Board = { gameState.Board with Models = forAllModels (fun m -> { Model = m
                                                                                                  Player = p.Player
                                                                                                  Position = gameState |> positionAsker }) newUnit gameState} }
                |> updatePlayerInGameState u newUnit
        | None -> failwith "Couldn't find player"

    let move u gameState maxMove  = 
        let pixelsInCircle radius position =  seq {
            for x in createSeq (position.X - radius) (position.X + radius) do
                for y in createSeq (position.Y - radius) (position.Y + radius) do
                    let newPos = {X=x;Y=y}
                    if x > 0<px> && y > 0<px> && position.FindDistance newPos <= radius then
                        yield newPos
        }    

        let createMove moveAsker =
            let newPosition m = 
                let ps = pixelsInCircle ((inch.ToPixels characterResolution maxMove / 1.<px> |> System.Math.Round |> int) * 1<px>) m.Position |> Seq.toArray
                let rec newPick ps = 
                    let (p:Position<px>) = moveAsker ps
                    if Seq.contains p ps then p
                    else newPick ps
                newPick ps

            match tryFindPlayer gameState u with
            | Some p -> 
                { gameState with Board = 
                                    { gameState.Board with Models = forAllModels (fun m -> { Model = m
                                                                                             Player = p.Player
                                                                                             Position = findModel m.Id gameState |> newPosition }) u gameState} }
            | None -> failwith "Couldn't find player"
        createMove

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
            
            let createNewUnit (unit : Unit) = { unit with Rules = Map.map (fun _ t -> enableRule t) unit.Rules }
            let newGameState = 
                gameState.Players 
                |> List.fold 
                       (fun acc p -> 
                       p.Units |> Map.fold (fun acc2 _ unit -> updatePlayerInGameState unit (createNewUnit unit) acc2) acc) 
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
    let replaceRuleOnUnit gameState (unit : Unit) replace = 
        let newUnit = { unit with Rules = unit.Rules |> replace }
        updatePlayerInGameState unit newUnit gameState
    let rec eval playerMove fs uId gameState = 
        //Continue more stuff after you ask the user to do something
        let (|>!) f g = 
            f >> g |> Asker 
        let either def op  = 
            match op with 
            | Some x -> x
            | None -> def
        match fs with
        | [] -> GameStateResult gameState
        | h :: tail -> 
            (h, tryFindNewUnit gameState uId )
            |> function 
                | Function(EndPhase), _ -> GameStateResult (advancePhase gameState)
                | Function(Deploy), Some unit -> deploy unit gameState |>! playerMove uId tail |> PositionAsker |> AskResult              
                | Function(Move maxMove), Some unit -> move unit gameState maxMove |>! playerMove uId tail |> MoveAsker |> AskResult
                | Function(SetCharacteristicUnit(name, newRule)), Some u -> 
                    u.Rules 
                    |> Map.tryFind name 
                    |> Option.map(fun r -> r |> Map.replace (Rule.CreateNested newRule) <| name
                                                |> replaceRuleOnUnit gameState u |> GameStateResult)
                    |> either (GameStateResult gameState)
                | DeactivatedUntilEndOfPhaseOnFirstUse(r) as dr, Some u -> 
                    let newGameState = 
                        u.Rules 
                        |> Map.pickKeyOfItem dr 
                        |> Map.replace DeactivatedUntilEndOfPhase dr
                        |> replaceRuleOnUnit gameState u 
                    eval playerMove (collectRules newGameState r) (Some u.Id) newGameState
                | DeactivatedUntilEndOfGameOnFirstUse(r) as dr, Some u ->  
                    let newGameState = 
                        u.Rules 
                        |> Map.pickKeyOfItem dr 
                        |> Map.replace DeactivatedUntilEndOfGame dr
                        |> replaceRuleOnUnit gameState u 
                    eval playerMove (collectRules newGameState r) (Some u.Id) newGameState
                | _ -> GameStateResult gameState
            |> function
                | GameStateResult gs -> eval playerMove tail uId gs
                | ask -> ask
    
    
    let endPhase = Function(EndPhase)
    
    let availableRuleCapabilities player gs = 
        gs.Players
        |> List.filter (fun p -> p.Player = player)
        |> List.collect (fun p -> p.Units |> Map.toList |> List.unzip |> snd)
        |> List.collect (fun u -> 
               u.Rules
               |> Map.toList
               |> List.map (fun (_,r) -> collectRules gs r, Some u.Id))
        |> List.filter (fst >> List.isEmpty >> not)

    let makeNextMoveInfo f player gameState (rules, uId) = 
        let capability() = f player uId rules gameState
        match uId with
        | Some u -> 
            UnitRule({ UnitId = u
                       UnitName = tryFindNewUnit gameState uId |> Option.fold (fun _ o -> o.UnitName) ""
                       },rules,capability)
        | None -> 
            EndRule (rules,capability)
    
    let gameResultFor player gs nextMoves  = 
        match player with
        | Player1 -> Player1ToMove(gs, nextMoves)
        | Player2 -> Player2ToMove(gs, nextMoves)
    
    let makegameResultWithCapabilities f player newGameState rulesAndUnits = 
        match rulesAndUnits with
        | [] -> None
        | rulesAndUnits -> 
            ((collectRules newGameState endPhase), None) :: rulesAndUnits
            |> List.map (makeNextMoveInfo f player newGameState)
            |> Next 
            |> gameResultFor player newGameState 
            |> Some
    
    let gameResult (gs:GameState) playerMove currentPlayer = 
        let result = 
            gs
            |> availableRuleCapabilities currentPlayer
            |> makegameResultWithCapabilities playerMove currentPlayer gs
        match result with
        | Some ruleResult -> ruleResult
        | None -> playerMove currentPlayer None (collectRules gs endPhase) gs
        
    let rec playerMove player uId thingsToDo (gameState:GameState) = 
        let evalResult = 
            eval (playerMove player) thingsToDo uId gameState
        
        let newPlayer = 
            match (gameState, evalResult) with
                | gs, GameStateResult newGs ->
                    match gs.Game.Turn, newGs.Game.Turn with
                    | Top(_), Top(_) -> player
                    | Top(_), Bottom(_) -> other player
                    | Bottom(_), Top(_) -> other player
                    | Bottom(_), Bottom(_) -> player
                | _ -> player
        
        match evalResult with
            | EndGame gs -> 
                match gs with 
                    | Leader player -> GameWon(gs, player)
                    | Tied -> GameTied gs
            | GameStateResult gs -> gameResult gs playerMove newPlayer
            | AskResult a -> Ask a |> gameResultFor newPlayer gameState

    
    let newGame  () = 
        // create initial game state
        let gameState = Impl.ImplTest.initial
        gameResult gameState playerMove Player1
    
    /// export the API to the application
    let api = { NewGame = newGame }
