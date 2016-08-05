namespace GameImpl

module GameLoop = 
    open Domain
    open Domain.Board
    open Domain.Game
    open GameImpl.GameState
    open GameImpl.RulesImpl
    open Microsoft.FSharp.Collections
    let other player = 
        player |> function 
        | Player1 -> Player2
        | Player2 -> Player1
       
    type AvailableRulesMap<'a> = {GameStateMap : GameState->RuleApplication->'a
                                  UnitMap : GameState->Unit->RuleApplication->'a 
                                  ModelMap: GameState->ModelInfo->RuleApplication->'a }

    let optionalRulesMap = {GameStateMap = (fun gs r -> {gs with Rules = activateRule r gs.Rules })
                            UnitMap = (fun gs item r -> replaceRuleOnUnit item (activateRule r) gs)
                            ModelMap = (fun gs item r -> replaceRuleOnModel item.Model (activateRule r) gs )}
    let activeRulesMap = {GameStateMap = (fun _ _ -> ())
                          UnitMap = (fun _ _ _ -> ())
                          ModelMap = (fun _ _ _ -> ())}
    let availableRules predicate (mapper:AvailableRulesMap<'a>) player gs = 
        let captureRules =  Map.toSeq >> Seq.choose predicate >> Seq.toList

        let gameRules = captureRules gs.Rules |> List.map (fun r -> r, mapper.GameStateMap gs r)
        
        let unitRules = 
            gs.Players
            |> List.filter (fun p -> p.Player = player)
            |> List.map (fun p -> p.Units) 
            |> List.exactlyOne
            |> Map.toList
            |> List.collect (fun (_,item) ->  captureRules item.Rules |> List.map (fun r -> r, mapper.UnitMap gs item r ))

        let modelRules = 
            gs.Board.Models 
            |> Map.filter (fun _ item -> item.Player = player) 
            |> Map.toList
            |> List.collect (fun (_,item) -> captureRules item.Model.Rules |> List.map (fun r -> r,mapper.ModelMap gs item r))

        match gameRules @ unitRules @ modelRules with
        | [] -> 
            let ra = GameStateRule(EndPhase)
            [ra,mapper.GameStateMap {gs with Rules = gs.Rules.Add(EndPhase.ToString(),Function(ra))} ra]
        | rules -> rules

        
    let makeNextMoveInfo f player (ruleApplication,gameState) = 
        let capability () = 
            let predicate = activeRules gameState
            let activeRules = 
                availableRules predicate activeRulesMap player gameState 
                |> List.map fst
            f player activeRules gameState
        match ruleApplication with
            | UnitRule (_, uguid) as ra -> UnitRuleInfo({UnitId=uguid; UnitName=""; Rule=Function(ra)}), capability
            | ModelRule (_, mguid) as ra -> ModelRuleInfo({ModelId=mguid; Rule=Function(ra)}), capability
            | GameStateRule _ as ra -> GameStateRuleInfo(Function(ra)), capability
            | Sequence _ as ra -> GameStateRuleInfo(Function(ra)), capability
    
    let gameResultFor player gs nextMoves  = 
        match player with
        | Player1 -> Player1ToMove(gs, nextMoves)
        | Player2 -> Player2ToMove(gs, nextMoves)
    
    let makeResultWithCapabilities f player currentState rules= 
        rules |> List.map (makeNextMoveInfo f player) |> Next |> gameResultFor player currentState
    
    let doNextTick gs playerMove currentPlayer = 
        let predicate = optionalRules gs
        gs
            |> availableRules predicate optionalRulesMap currentPlayer
            |> makeResultWithCapabilities playerMove currentPlayer gs


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
