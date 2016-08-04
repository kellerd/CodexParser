namespace GameImpl

module RulesImpl = 
    open Domain.WarhammerDomain
    open Domain
    open Domain.Board
    open GameImpl.GameState
    open Microsoft.FSharp.Collections
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
    and tryFindInRuleList (gameState:GameState) ruleApplication rl = 
        rl |> Option.bind (Map.tryFindKey (fun k foundRule -> match ruleApplication with 
                                                                | GameStateRule impl ->  k = impl.ToString() && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | ModelRule (impl, _) ->  k = impl.ToString() && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | UnitRule(impl, _) ->  k = impl.ToString() && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | Sequence(impl::_) -> k = impl.ToString() && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | Sequence([]) -> false
                                                                ))
    and contains (gameState:GameState) logic rule = 
            match logic with 
            | Rule ruleApplication -> 
                let rec tryFind = function 
                                | GameStateRule _ ->  Some gameState.Rules
                                | ModelRule (_, mId) ->  tryFindModel gameState mId |> Option.map (fun m -> m.Model.Rules)
                                | UnitRule(_, uId) -> tryFindUnit gameState uId |>  Option.map (fun u -> u.Rules)
                                | Sequence(impl::_) -> tryFind impl
                                | Sequence([]) -> None
                match ruleApplication|> tryFind |> tryFindInRuleList gameState ruleApplication  with 
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
    let optionalRules gs (_,r) = match r,gs with Optional _, Active r ra -> Some ra | _ -> None
    let activeRules gs (_,r) = match gs with Active r ra -> Some ra | _ -> None

    let activateRule r rules = 
        let matchName = function
            | UnitRule(r,_) -> r.ToString()
            | ModelRule(r,_) -> r.ToString()
            | GameStateRule(r) -> r.ToString()
            | Sequence(r::_) -> r.ToString()
            | Sequence([]) -> ""
        let name = matchName r
        let original = Map.tryFind name rules
        match original with 
        | Some original ->
            Map.replace (Rule.overwrite original) (Function(r)) name rules
        | None -> rules 
    let deploy uId gameState  = 
        let foundUnit = tryFindUnit gameState uId
        let foundPlayer = foundUnit |> Option.bind (tryFindPlayer gameState)
        match foundPlayer, foundUnit with
        | Some p, Some u -> 
            let pa positionAsker =
                let name = DeploymentState(Deployed).ToString()
                let newRule = Function(UnitRule(DeploymentState(Deployed), uId))
                let newGs = tryReplaceRuleOnUnit name (Rule.overwrite newRule) uId gameState
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

    let advancePhase gs = 

        let nextGameTurn = function
            | Begin -> One Phase.Begin
            | One _ -> Two Phase.Begin
            | Two _ -> Three Phase.Begin
            | Three _ -> Four Phase.Begin
            | Four _ -> Five Phase.Begin
            | Five _ -> Six Phase.Begin
            | Six _ -> Seven Phase.Begin
            | Seven _ -> End
            | End -> End
        let splitRound = 
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
        let otherPlayerTurn = function
            | Top,x -> Bottom, Phase.Begin |> (splitRound x|> fst) 
            | Bottom,x -> Top,nextGameTurn x
        let changeRound (round,turn) = 
            let (GtMaker, phase) = splitRound round
            let (newTurn,newRound) =
                match phase with
                | Some Phase.Begin -> turn, GtMaker Phase.Movement
                | Some Phase.Movement ->  turn, GtMaker Phase.Psychic
                | Some Phase.Psychic ->  turn, GtMaker Phase.Shooting
                | Some Phase.Shooting ->  turn, GtMaker Phase.Assault
                | Some Phase.Assault ->  turn, GtMaker Phase.End
                | Some Phase.End | None -> otherPlayerTurn (turn,round)
                |> (fun (turn,round) -> Function(GameStateRule(PlayerTurn(turn))), Function(GameStateRule(GameRound(round))))
            [replaceRuleOnGameState <| Map.replace id newTurn (PlayerTurn(Top).ToString()) ;
            replaceRuleOnGameState <| Map.replace id newRound (GameRound(Begin).ToString())]
            |> List.reduce (>>)
        match Map.tryFind (GameRound(Begin).ToString()) gs.Rules,Map.tryFind (PlayerTurn(Top).ToString()) gs.Rules with
        | Some(Function(GameStateRule(GameRound(round)))), Some(Function(GameStateRule(PlayerTurn(turn)))) -> changeRound(round,turn) gs
        | _ -> failwith <| sprintf "Couldn't find game round or playerturn %A" gs.Rules

    let rec revert ruleApplication gameState = 
        match ruleApplication with 
        | UnitRule(rule,uId) -> 
            let name = rule.ToString()
            tryReplaceRuleOnUnit name Rule.unoverwrite uId gameState
        | ModelRule(rule,mId) ->
            let name = rule.ToString()
            tryReplaceRuleOnModel name Rule.unoverwrite mId gameState
        | GameStateRule(rule) ->
            let name = rule.ToString()
            tryReplaceRuleOnGameState name Rule.unoverwrite gameState
        | Sequence(rule::_) ->
            revert rule gameState
        | Sequence([]) -> gameState
    let rec deactivateUntil activateWhen ruleApplication gameState  =
        let makeNewRule ruleApplication oldRule = Overwritten(ActiveWhen(activateWhen,Function(GameStateRule(Revert(ruleApplication)))),oldRule)
        match ruleApplication with 
        | UnitRule(rule,uId) -> 
            let name = rule.ToString()
            tryReplaceRuleOnUnit name (makeNewRule ruleApplication) uId gameState
        | ModelRule(rule,mId) ->
            let name = rule.ToString()
            tryReplaceRuleOnModel name (makeNewRule ruleApplication) mId gameState
        | GameStateRule(rule) ->
            let name = rule.ToString()
            tryReplaceRuleOnGameState name (makeNewRule ruleApplication) gameState
        | Sequence(rule::tail) ->
            deactivateUntil activateWhen rule gameState
        | Sequence([]) -> gameState

    let deploy uId gameState  = 
        let foundUnit = tryFindUnit gameState uId
        let foundPlayer = foundUnit |> Option.bind (tryFindPlayer gameState)
        match foundPlayer, foundUnit with
        | Some p, Some u -> 
            let pa positionAsker =
                let name = DeploymentState(Deployed).ToString()
                let newRule = Function(UnitRule(DeploymentState(Deployed), uId))
                let newGs = tryReplaceRuleOnUnit name (Rule.overwrite newRule) uId gameState
                let newUnit = tryFindUnit newGs uId |> defaultArg <| u
                { newGs with Board = { newGs.Board with Models = forAllModels (fun m -> { Model = m; Player = p.Player; Position = newGs |> positionAsker }) newUnit newGs} }
            pa
        | None, _ -> failwith "Couldn't find player"
        | _, None -> failwith "Couldn't find unit"
    

    //let assault = .... //May have to change this to adding a Targetted rule to target, which adds a Melee to model 
    let melee attacks toHit target mId gameState = 
        let foundModel = tryFindUnit gameState mId
        let foundTarget = tryFindUnit gameState target
        match foundTarget, foundModel with 
        | Some t, Some m ->
            let rollForHits diceAsker = 
                // let rolls = Seq.init attacks (fun _ -> diceAsker()) |> Seq.filter ((>=) toHit)
                0
            rollForHits
    
        | None, _ -> failwith "Couldn't find target"
        | _, None -> failwith "Couldn't find model"
    let rec eval rules gameState = 
        match rules with
        | [] -> GameStateResult gameState
        | rule::rest -> 
            rule |> function 
                | UnitRule(Deploy,uId) -> deploy uId gameState >> eval rest |> Asker  |> PositionAsker |> AskResult              
                | UnitRule(Move maxMove,uId) -> move uId gameState maxMove >> eval rest |> Asker  |> MoveAsker |> AskResult
                | UnitRule(SetCharacteristicUnit(name, newRule), uId) -> tryReplaceRuleOnUnit name (Rule.overwrite newRule) uId gameState |> eval  rest
                | GameStateRule(GameRound(_))    -> eval  rest gameState
                | GameStateRule(PlayerTurn(_))   -> eval  rest gameState
                | UnitRule(DeploymentState(_),_) -> eval  rest gameState
                | UnitRule(UCharacteristic(_),_) -> eval  rest gameState 
                | GameStateRule(Noop)            -> eval  rest gameState
                | GameStateRule(EndPhase) -> advancePhase gameState |> eval rest
                | GameStateRule(Revert(ruleApplication)) -> revert ruleApplication gameState |> eval rest
                | GameStateRule(DeactivateUntil(activateWhen,ruleApplication)) -> deactivateUntil activateWhen ruleApplication gameState |> eval rest
                | Sequence(rules) -> eval (rules @ rest) gameState
                | ModelRule(Melee(attacks,toHit,target),mId) -> melee attacks target mId 
                | xs -> failwith <| sprintf "%A" xs
    
    