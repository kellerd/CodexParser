namespace GameImpl
module WarhammerImpl = 
    open Domain.WarhammerDomain
    
    let other player = player |> function
        | Player1 -> Player2
        | Player2 -> Player1

    let private gameWonBy gameState = 
        let maxPlayerInfo = gameState.Players |> List.sortBy (fun p -> p.Score)
        match maxPlayerInfo with 
            | p1::p2::_ when p1.Score > p2.Score -> Some p1.Player
            | _ -> None
    let private isEndCondition gameState = 
        (gameState.Game.Mission.EndCondition gameState)
    let pick nextMoveInfos = 
        nextMoveInfos |> List.tryHead

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
        | Overwritten (r,r2) -> isRunnable r
        | DeactivatedUntilEndOfPhase _ -> false
        | DeactivatedUntilEndOfGame _ -> false
        | Description _ -> false
    let rec collectRules = function
        | Rule e -> [Rule e]
        | Nested (r,r2) -> 
            [r;r2] |> List.map collectRules |> List.collect id
        | Overwritten (r,r2) -> collectRules r
        | DeactivatedUntilEndOfPhase _ -> []
        | DeactivatedUntilEndOfGame _ -> []
        | Description _ -> []

    let advanceRound gs = 
        match gs.Game.Turn with 
            | Top x -> Bottom x
            | Bottom GameTurn.Begin -> Top GameTurn.One
            | Bottom GameTurn.One ->   Top GameTurn.Two
            | Bottom GameTurn.Two ->   Top GameTurn.Three
            | Bottom GameTurn.Three -> Top GameTurn.Four
            | Bottom GameTurn.Four ->  Top GameTurn.Five
            | Bottom GameTurn.Five ->  Top GameTurn.Six
            | Bottom GameTurn.Six ->   Top GameTurn.Seven
            | Bottom GameTurn.Seven -> Top GameTurn.End
            | Bottom GameTurn.End ->   Top GameTurn.End
    let advancePhase gs = 
        match gs.Game.Phase with
            | Phase.Begin -> {gs with Game = {gs.Game with Phase = Phase.Movement} }
            | Phase.Movement -> {gs with Game = {gs.Game with Phase = Phase.Psychic}  }
            | Phase.Psychic -> {gs with Game = {gs.Game with Phase = Phase.Shooting}  }
            | Phase.Shooting -> {gs with Game = {gs.Game with Phase = Phase.Assault}  }
            | Phase.Assault -> {gs with Game = {gs.Game with Phase = Phase.End}       }
            | Phase.End -> {gs with Game = {gs.Game with Phase = Phase.Begin; Turn = advanceRound gs}         }
    let rec eval fs gameState = 
        match fs with 
            | [] -> gameState
            | h::tail -> 
                    h
                        |> (function
                            | Rule(Function(EndPhase)) -> gameState |> advancePhase
                            | _ -> gameState) 
                        |> eval tail


    let updateUnit f (unit:Unit) gameState  = 
        ///TODO
        let foundPlayer = gameState.Players |> List.tryPick (fun p -> p.Units 
                                                                    |> List.tryFind(fun u -> u = unit)
                                                                    |> Option.bind (fun _ -> Some p))
        let replace xs x y = 
            let pred z = x = z
            y :: (removeFirst pred xs)

        let cUnit f (unit:Unit)  = {unit with Rules = replace unit.Rules f (DeactivatedUntilEndOfPhase f)}
        let cPlayer p u nu = {p with Units = replace p.Units u nu }
        let cState s p np = {s with Players = replace s.Players p np }

        let newUnit = cUnit f unit
        let newPlayer = foundPlayer |> Option.map (fun p-> cPlayer p unit newUnit)
        match (eval (collectRules f) gameState), foundPlayer, newPlayer with
            | (gs, Some p, Some np) -> cState gs p np
            | (gs, _, _) -> gs

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
        let newPlayer = 
            match currentPlayer, gs.Game.Phase with
                | None, _ -> Player1
                | Some p, Phase.Begin -> other p
                | Some p, _ -> p
        let result = gs 
                            |> availableRuleCapabilities newPlayer 
                            |> makeMoveResultWithCapabilities playerMove newPlayer gs
        match result with
            | Some ruleResult -> ruleResult
            | None -> playerMove newPlayer None endPhase gs
    let rec playerMove player unit thingToDo gameState = 
        let newGameState = 
                match unit with
                    | Some u -> gameState |> updateUnit thingToDo u
                    | None -> gameState |> eval [thingToDo]
//        let displayInfo = getDisplayInfo newGameState 
        if isEndCondition newGameState then 
            match gameWonBy newGameState with
            | Some player -> GameWon (newGameState, player) 
            | None -> GameTied newGameState 
        else
            Some player |> moveResult newGameState playerMove 


    let newGame() = 
        // create initial game state
        let gameState = Impl.ImplTest.initial
        moveResult gameState playerMove None
        
        
    /// export the API to the application
    let api = {
        NewGame = newGame 
        }
