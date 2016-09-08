namespace GameImpl

module GameLoop = 
    open Domain
    open Domain.Board
    open Domain.Game
    open GameImpl.RulesImpl
    open Microsoft.FSharp.Collections
    let other player = 
        player |> function 
        | Player1 -> Player2
        | Player2 -> Player1

    let gameResultFor player gs nextMoves  = 
        match player with
        | Player1 -> Player1ToMove(gs, nextMoves)
        | Player2 -> Player2ToMove(gs, nextMoves)
    
    let doNextTick gameState playerMove player = 
        let predicate = activeRules gameState
        let activeRules = 
            availableRules predicate player gameState 
        let moveCap() = playerMove player activeRules gameState
        moveCap |> Next |> gameResultFor player gameState 

    let private (|Leader|Tied|) gameState = 
        let maxPlayerInfo = gameState.Players |> List.sortBy (fun p -> p.Score)
        match maxPlayerInfo with
        | p1 :: p2 :: _ when p1.Score > p2.Score -> Leader p1.Player
        | _ -> Tied  

    let private (|EndGame|_|) = function 
        | GameStateResult gameState -> 
            match gameState with
            | Active (ActiveWhen(Rule(GameStateRule(Domain.EndGame)), Function(GameStateRule(Noop)))) _ -> Some gameState
            | _ -> None 
        | _ -> None   
         
    let rec moveNextPlayer player gameState evalResult  = 
        let newPlayer = 
            match (gameState, evalResult) with
                | Active (ActiveWhen(Rule(GameStateRule(Domain.PlayerTurn(Top))), Function(GameStateRule(Noop)))) _,
                  GameStateResult (Active (ActiveWhen(Rule(GameStateRule(Domain.PlayerTurn(Bottom))), Function(GameStateRule(Noop)))) _) 
                | Active (ActiveWhen(Rule(GameStateRule(Domain.PlayerTurn(Bottom))), Function(GameStateRule(Noop)))) _, 
                  GameStateResult (Active (ActiveWhen(Rule(GameStateRule(Domain.PlayerTurn(Top))), Function(GameStateRule(Noop)))) _) -> other player
                | _ -> player
        
        match evalResult with
            | EndGame gs -> 
                match gs with 
                    | Leader player -> GameWon(gs, player)
                    | Tied -> GameTied gs
            | GameStateResult gs -> doNextTick gs playerMove newPlayer
            | AskResult a -> a.Map(moveNextPlayer newPlayer gameState) |> Ask |> gameResultFor newPlayer gameState
   
    and  playerMove player (rules:RuleApplication list) gameState = 
        let evalResult = 
            match rules with
            | [] -> GameStateResult gameState
            | rules -> eval rules gameState 
        moveNextPlayer player gameState evalResult 

    let newGame  () = 
        // create initial game state
        let gameState = Impl.ImplTest.initial
        doNextTick gameState playerMove Player1
    
    /// export the API to the application
    let api = { NewGame = newGame }
