namespace GameImpl
module Map =
    let pickKeyOfItem item map = Map.pick (fun k ur -> if item = ur then Some k  else None) map
    let replace f x oldKey = (Map.add oldKey (f x))
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

    let tryFindUnit gameState unitId = 
        gameState.Players |> List.tryPick (fun p -> p.Units |> Map.tryFind unitId)
    let tryFindPlayer gameState unit = 
        gameState.Players |> List.tryPick (fun p -> 
                                                     p.Units
                                                     |> Map.tryFind unit.Id
                                                     |> Option.bind (fun _ -> Some p))
    let tryFindModel gameState mId  = gameState.Board.Models |> Map.tryFind mId

    let rec removeFirst pred lst = 
        match lst with
        | h :: t when pred h -> t
        | h :: t -> h :: removeFirst pred t
        | _ -> []

    let rec (|Optional|_|) r = 
        match r with 
        | ActiveWhen(_, Optional rule) -> Some rule
        | UserActivated rule -> Some rule
        | Overwritten(Optional overwrite, _) -> Some overwrite
        | ActiveWhen(_) -> None
        | Function(_) -> None
        | Description(_) -> None
        | Overwritten(_) -> None
    let rec (|Active|_|) r gameState = 
        match r with 
        | ActiveWhen(logic, innerRule) -> contains gameState logic innerRule |> Option.bind (fun _ -> match gameState with Active innerRule rule -> Some rule | _ -> None)
        | UserActivated (userActivated) ->  match gameState with Active userActivated rule -> Some rule | _ -> None
        | Function app -> Some app
        | Description _ -> Some (GameStateRule Noop)
        | Overwritten (overwrite,_) -> match gameState with Active overwrite rule -> Some rule | _ -> None

    and findInRuleList (gameState:GameState) ruleApplication rl = 
        rl |> Option.bind (Map.tryFindKey (fun k foundRule -> match ruleApplication with 
                                                                | GameStateRule impl ->  k = impl.ToString && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | ModelRule (impl, _) ->  k = impl.ToString && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | UnitRule(impl, _) ->  k = impl.ToString && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                ))
    and contains (gameState:GameState) logic rule = 
            match logic with 
            | Rule ruleApplication -> 
                let result = match ruleApplication with
                                | GameStateRule _ ->  Some gameState.Rules
                                | ModelRule (_, mId) ->  tryFindModel gameState mId |> Option.map (fun m -> m.Model.Rules)
                                | UnitRule(_, uId) -> tryFindUnit gameState uId |>  Option.map (fun u -> u.Rules)
                match result |> findInRuleList gameState ruleApplication  with 
                    | Some _ -> Some rule
                    | None -> None
            | Not expr -> match contains gameState expr rule with
                          | Some _ -> None
                          | None -> Some rule
            | Logical (expr,op,expr2) ->  match op, contains gameState expr rule, contains gameState expr2 rule with
                                          | And, Some _, Some _ -> Some rule
                                          | And, _, _ -> None
                                          | Or, Some _, _ -> Some rule
                                          | Or, _, Some _ -> Some rule
                                          | Or, None _, None _ -> None
     
    let private (|EndGame|_|) = function 
        | GameStateResult gameState -> 
            match gameState with
            | Active (ActiveWhen(Rule(GameStateRule(Domain.WarhammerDomain.EndGame)), Function(GameStateRule(Noop)))) _ -> Some gameState
            | _ -> None 
        | _ -> None
//    let rec (|Active|NotActive|Optional|) (gameState:GameState,r:Rule) =
//        
//
//        
//            
//        match r with 
////        | UnitRule (_) -> [Active r]
////        | ModelRule of ModelRuleImpl * ModelGuid
////        | GameStateRule of GameRuleImpl
////        | Nested(Active(r),Active(r)) 
////        | Overwritten of Rule * Rule 
////        | OnceUntil of LogicalExpression  * Rule
//        | UserActivated rule -> Optional rule
//        | Function(_) -> Active r
//        | ActiveWhen(logic, rule) -> contains logic rule
//        | Description(_) -> Active r

//    let collectRules (gs:GameState) = 
//        let isActive = function
//            ///Todo find nested rules -> Depends on how they are nested.
//            | UnitApplication (r, ug) -> tryFindUnit gs ug |> Option.bind (fun u -> u.Rules |> Map.tryFind r.ToString)
//            | ModelApplication (r, mg) -> findModel mg gs |> fun mi -> mi.Model.Rules |> Map.tryFind r.ToString
//            | GameStateApplication r -> gs.Rules |> Map.tryFind r.ToString
//        let rec collectRest = function
//            | Function e -> [ Function e ]
//            | Nested(r, r2) -> 
//                [ r; r2 ]
//                |> List.collect collectRest
//            | Overwritten(r, _) -> collectRest r
//            | DeactivatedUntilEndOfPhase _ -> []
//            | DeactivatedUntilEndOfGame _ -> []
//            | Description _ -> []
//            | DeactivatedUntilEndOfPhaseOnFirstUse _ as r -> [r]
//            | DeactivatedUntilEndOfGameOnFirstUse _ as r -> [r]
//            | Characteristic _ -> []
//            | ActiveWhen (ra,r) -> match isActive ra with
//                                    | Some _ -> [r]
//                                    | None -> []
//        collectRest
//    let splitPt = 
//        function 
//        | Top(x) -> Top, x
//        | Bottom(x) -> Bottom, x
//    
//    let splitGt = 
//        function 
//        | Begin -> (fun _ -> Begin), None
//        | One x -> One, Some x
//        | Two x -> Two, Some x
//        | Three x -> Three, Some x
//        | Four x -> Four, Some x
//        | Five x -> Five, Some x
//        | Six x -> Six, Some x
//        | Seven x -> Seven, Some x
//        | End -> (fun _ -> End), None
    
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
    let replaceRuleOnUnit gameState (unit : Unit) replace = 
        let newUnit = { unit with Rules = unit.Rules |> replace }
        updatePlayerInGameState unit newUnit gameState

    let forAllModels f newUnit gameState = 
        [ for m in newUnit.UnitModels do
                yield f m.Value ]
        |> List.fold (fun acc m -> match Map.tryFind m.Model.Id acc with
                                    | Some _ -> Map.replace id m m.Model.Id acc
                                    | None -> Map.add m.Model.Id m acc) gameState.Board.Models
    let deploy uId gameState  = 
        let foundUnit = tryFindUnit gameState uId
        let foundPlayer = foundUnit |> Option.bind (tryFindPlayer gameState)
        match foundPlayer, foundUnit with
        | Some p, Some u -> 
            let pa positionAsker =
                let newRule = Function(UnitRule(DeploymentState(Deployed), uId))
                let newGs = 
                    u.Rules 
                    |> Map.tryFind (DeploymentState.ToString()) |> defaultArg <| newRule 
                    |> Map.replace (Rule.Overwrite newRule) <| DeploymentState.ToString() 
                    |> replaceRuleOnUnit gameState u
                let newUnit = tryFindUnit newGs uId |> defaultArg <| u
                { newGs with Board = { newGs.Board with Models = forAllModels (fun m -> { Model = m; Player = p.Player; Position = newGs |> positionAsker }) newUnit newGs} }
            pa
        | None, _ -> failwith "Couldn't find player"
        | _, None -> failwith "Couldn't find unit"

    let move uId gameState maxMove  = 
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

            let foundUnit = tryFindUnit gameState uId
            let foundPlayer = foundUnit |> Option.bind (tryFindPlayer gameState)
            match foundPlayer, foundUnit with
            | Some p, Some u -> 
                { gameState with Board = 
                                    { gameState.Board with Models = forAllModels (fun m -> { Model = m
                                                                                             Player = p.Player
                                                                                             Position = tryFindModel gameState m.Id |> Option.get |> newPosition }) u gameState} }
            | None, _ -> failwith "Couldn't find player"
            | _, None -> failwith "Couldn't find unit"
        createMove

//    let advancePhase gs = 
//        let nextGt x = 
//            match x with
//            | Begin -> (Turn.One Phase.Begin)
//            | One _ -> (Turn.Two Phase.Begin)
//            | Two _ -> (Turn.Three Phase.Begin)
//            | Three _ -> (Turn.Four Phase.Begin)
//            | Four _ -> (Turn.Five Phase.Begin)
//            | Five _ -> (Turn.Six Phase.Begin)
//            | Six _ -> (Turn.Seven Phase.Begin)
//            | Seven _ -> (Turn.End)
//            | End -> (Turn.End)
//        
//        let enableDeactivatedRules gameState = 
//            let rec enableRule = 
//                function 
//                | Nested(r, r2) -> Nested(enableRule r, enableRule r2)
//                | DeactivatedUntilEndOfPhase r -> r
//                | r -> r
//            
//            let createNewUnit (unit : Unit) = { unit with Rules = Map.map (fun _ t -> enableRule t) unit.Rules }
//            let newGameState = 
//                gameState.Players 
//                |> List.fold 
//                       (fun acc p -> 
//                       p.Units |> Map.fold (fun acc2 _ unit -> updatePlayerInGameState unit (createNewUnit unit) acc2) acc) 
//                       gameState
//            newGameState.Players
//        
//        let otherPt = 
//            function 
//            | Top(x) -> 
//                Phase.Begin
//                |> (splitGt x |> fst)
//                |> Bottom
//            | Bottom(x) -> Top(nextGt x)
//        
//        let changePhase turn = 
//            let (PtMaker, gt) = splitPt turn
//            let (GtMaker, phase) = splitGt gt
//            match phase with
//            | Some Phase.Begin -> PtMaker(GtMaker Phase.Movement)
//            | Some Phase.Movement -> PtMaker(GtMaker Phase.Psychic)
//            | Some Phase.Psychic -> PtMaker(GtMaker Phase.Shooting)
//            | Some Phase.Shooting -> PtMaker(GtMaker Phase.Assault)
//            | Some Phase.Assault -> PtMaker(GtMaker Phase.End)
//            | Some Phase.End | None -> otherPt turn
//        
//        { gs with Game = { gs.Game with Turn = changePhase gs.Game.Turn }
//                  Players = gs |> enableDeactivatedRules }


    
    let rec eval playerMove rule gameState = 
        let (|>!)  f g =
            f >> g |> Asker
        //Continue more stuff after you ask the user to do something

        rule |> function 
//                | Function(GameStateApplication(EndPhase)) -> GameStateResult (advancePhase gameState)
                | Function(UnitRule(Deploy,uId)) -> deploy uId gameState |>! playerMove None |> PositionAsker |> AskResult              
                | Function(UnitRule(Move maxMove,uId)) -> move uId gameState maxMove |>! playerMove None |> MoveAsker |> AskResult
                | Function(UnitRule(SetCharacteristicUnit(name, newRule), uId)) -> 
                    uId |> tryFindUnit gameState
                    |> Option.bind (fun u -> u.Rules 
                                                |> Map.tryFind name
                                                |> Option.map (fun r -> r |> Map.replace (Rule.Overwrite newRule) <| name
                                                                          |> replaceRuleOnUnit gameState u |> GameStateResult))
                    |> defaultArg <| (GameStateResult gameState)
                | UserActivated r -> eval playerMove r gameState
                | ActiveWhen (_,r) -> eval playerMove r gameState
                | Description _ -> GameStateResult gameState
                | Overwritten (r, _) -> eval playerMove r gameState
//                | _ -> GameStateResult gameState
//                | DeactivatedUntilEndOfPhaseOnFirstUse(r) as dr -> 
//                    let newGameState = 
//                        u.Rules 
//                        |> Map.pickKeyOfItem dr 
//                        |> Map.replace DeactivatedUntilEndOfPhase dr
//                        |> replaceRuleOnUnit gameState u 
//                    eval playerMove (collectRules newGameState r) (Some u.Id) newGameState
//                | DeactivatedUntilEndOfGameOnFirstUse(r) as dr ->  
//                    let newGameState = 
//                        u.Rules 
//                        |> Map.pickKeyOfItem dr 
//                        |> Map.replace DeactivatedUntilEndOfGame dr
//                        |> replaceRuleOnUnit gameState u 
//                    eval playerMove (collectRules newGameState r) (Some u.Id) newGameState
//                | _ -> GameStateResult gameState
    
    
    let availableRules player gs = 
        let optionalRules =  Map.toSeq >> Seq.choose (fun (_,r) -> match r,gs with Optional _, Active r _ -> Some r | _ -> None) >> Seq.toList
        let gameRules = optionalRules gs.Rules |> List.map GameStateRuleInfo
        
        let unitRules = 
            gs.Players
            |> List.filter (fun p -> p.Player = player)
            |> List.map (fun p -> p.Units) 
            |> List.exactlyOne
            |> Map.toList
            |> List.collect (fun (k,item) ->  optionalRules item.Rules |> List.map (fun r -> UnitRuleInfo { UnitId = k; UnitName = item.UnitName; Rule = r }))

        let modelRules = 
            gs.Board.Models 
            |> Map.filter (fun _ item -> item.Player = player) 
            |> Map.toList
            |> List.collect (fun (k,item) -> optionalRules item.Model.Rules |> List.map (fun r -> ModelRuleInfo { ModelId = k; Rule = r }))

        gameRules @ unitRules @ modelRules
        
    let makeNextMoveInfo f player gameState ruleInfo = 
        let capability rule () = f player rule gameState
        match ruleInfo with
                        | GameStateRuleInfo ri -> (ruleInfo, Some ri |> capability)
                        | UnitRuleInfo ri -> (ruleInfo, Some ri.Rule |> capability)
                        | ModelRuleInfo ri -> (ruleInfo, Some ri.Rule |> capability)
    
    let gameResultFor player gs nextMoves  = 
        match player with
        | Player1 -> Player1ToMove(gs, nextMoves)
        | Player2 -> Player2ToMove(gs, nextMoves)
    
    let makeResultWithCapabilities f player newGameState rules = 
        let rules = 
            if List.isEmpty rules then
                [GameStateRuleInfo(UserActivated(Function(GameStateRule(EndPhase))))]
            else
                rules
        rules |> List.map (makeNextMoveInfo f player newGameState) |> Next |> gameResultFor player newGameState
    
    let gameResult gs playerMove currentPlayer = 
        gs
            |> availableRules currentPlayer
            |> makeResultWithCapabilities playerMove currentPlayer gs
        
    let rec playerMove player rule gameState = 
        
        let evalResult = 
            match rule with
            | Some rule -> eval (playerMove player) rule gameState
            | None _ -> GameStateResult gameState
        
        let newPlayer = 
            match (gameState, evalResult) with
                | Active (ActiveWhen(Rule(GameStateRule(Domain.WarhammerDomain.PlayerTurn(Top))), Function(GameStateRule(Noop)))) _,
                  GameStateResult (Active (ActiveWhen(Rule(GameStateRule(Domain.WarhammerDomain.PlayerTurn(Bottom))), Function(GameStateRule(Noop)))) _) 
                | Active (ActiveWhen(Rule(GameStateRule(Domain.WarhammerDomain.PlayerTurn(Bottom))), Function(GameStateRule(Noop)))) _, 
                  GameStateResult (Active (ActiveWhen(Rule(GameStateRule(Domain.WarhammerDomain.PlayerTurn(Top))), Function(GameStateRule(Noop)))) _) -> other player
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
