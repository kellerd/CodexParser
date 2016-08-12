﻿namespace GameImpl

module RulesImpl = 
    open Domain.WarhammerDomain
    open Domain
    open Domain.Board
    open Domain.Game
    open GameImpl.GameState
    open Microsoft.FSharp.Collections
    open FSharpx.Collections
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
        Map.updateWith (Rule.overwrite (Function(r)) >> Some) name rules

    let deploy uId gameState  = 
        let foundUnit = tryFindUnit gameState uId
        let foundPlayer = foundUnit |> Option.bind (tryFindPlayer gameState)
        match foundPlayer, foundUnit with
        | Some p, Some u -> 
            let pa positionAsker =
                let newRule = UnitRule(DeploymentState(Deployed), uId)
                let newGs = tryReplaceRuleOnUnit (newRule.ToString()) (Rule.overwrite (Function(newRule)) >> Some) uId gameState
                let newUnit = tryFindUnit newGs uId |> defaultArg <| u
                [newRule],{ newGs with Board = { newGs.Board with Models = forAllModels (fun m -> { Model = m; Player = p.Player; Position = newGs |> positionAsker }) newUnit newGs} }
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
        let changeRound (round,turn) gs = 
            let (GtMaker, phase) = splitRound round
           
            let mapReplace rule = Map.updateWith (def (Function(GameStateRule(rule)))) (rule.ToString())
            let doGameState rule = replaceRuleOnGameState <| mapReplace rule
            
            let (rules,gameStateChanges) =
                match phase with
                | Some Phase.Begin -> turn, GtMaker Phase.Movement
                | Some Phase.Movement ->  turn, GtMaker Phase.Psychic
                | Some Phase.Psychic ->  turn, GtMaker Phase.Shooting
                | Some Phase.Shooting ->  turn, GtMaker Phase.Assault
                | Some Phase.Assault ->  turn, GtMaker Phase.End
                | Some Phase.End | None -> otherPlayerTurn (turn,round)
                |> (fun (turn,round) -> [PlayerTurn(turn); GameRound(round)])
                |> List.map (fun r -> GameStateRule(r), doGameState r)
                |> List.unzip 
            rules,(gameStateChanges |> List.reduce (>>)) gs
        match Map.tryFind (GameRound(Begin).ToString()) gs.Rules,Map.tryFind (PlayerTurn(Top).ToString()) gs.Rules with
        | Some(Function(GameStateRule(GameRound(round)))), Some(Function(GameStateRule(PlayerTurn(turn)))) -> changeRound(round,turn) gs
        | _ -> failwith <| sprintf "Couldn't find game round or playerturn %A" gs.Rules

    let rec revert ruleApplication gameState = 
        match ruleApplication with 
        | UnitRule(rule,uId) -> 
            let name = rule.ToString()
            tryReplaceRuleOnUnit name (Rule.unoverwrite >> Some) uId gameState
        | ModelRule(rule,mId) ->
            let name = rule.ToString()
            tryReplaceRuleOnModel name (Rule.unoverwrite >> Some) mId gameState
        | GameStateRule(rule) ->
            let name = rule.ToString()
            tryReplaceRuleOnGameState name (Rule.unoverwrite >> Some) gameState
        | Sequence(rule::_) ->
            revert rule gameState 
        | Sequence([]) -> gameState
    let rec remove ruleApplication gameState = 
        match ruleApplication with 
        | UnitRule(rule,uId) -> 
            let name = rule.ToString()
            tryReplaceRuleOnUnit name (fun _ -> None) uId gameState
        | ModelRule(rule,mId) ->
            let name = rule.ToString()
            tryReplaceRuleOnModel name (fun _ -> None) mId gameState
        | GameStateRule(rule) ->
            let name = rule.ToString()
            tryReplaceRuleOnGameState name (fun _ -> None) gameState
        | Sequence(rule::_) ->
            remove rule gameState 
        | Sequence([]) -> gameState
    let rec deactivateUntil activateWhen ruleApplication gameState  =
        let makeNewRule ruleApplication oldRule = Overwritten(ActiveWhen(activateWhen,Function(GameStateRule(Revert(ruleApplication)))),oldRule)
        match ruleApplication with 
        | UnitRule(rule,uId) -> 
            let name = rule.ToString()
            tryReplaceRuleOnUnit name (makeNewRule ruleApplication >> Some) uId gameState
        | ModelRule(rule,mId) ->
            let name = rule.ToString()
            tryReplaceRuleOnModel name (makeNewRule ruleApplication >> Some) mId gameState
        | GameStateRule(rule) ->
            let name = rule.ToString()
            tryReplaceRuleOnGameState name (makeNewRule ruleApplication >> Some) gameState
        | Sequence(rule::_) ->
            deactivateUntil activateWhen rule gameState
        | Sequence([]) -> gameState

    //let assault = .... //May have to change this to adding a Targetted rule to target, which adds a Melee to model 
    let melee attacks toHit target mId gameState = 
        let rollForHits diceAsker = 
            let hits = Seq.init attacks (fun _ -> diceAsker()) |> Seq.filter (fun (DiceRoll x) -> x >= toHit) |> Seq.length
            let newRule = ModelRule(MeleeHits(hits,target),mId)
            [newRule], tryReplaceRuleOnModel (MeleeHits.ToString()) (def (Function(newRule))) mId gameState
        rollForHits
    let woundTable str tough =
         match str - tough with 
            | 0 -> 4
            | 1 -> 3
            | -1 -> 5
            | -2 | -3 -> 6
            | x when x > 0 -> 2
            | _ -> 0
    let armourTable saves pen =
        match pen - saves with
        | x when x > 0 -> saves
        | _ -> 2

    let hitAssaultTable ws wsOpponent =
         match ws,wsOpponent with
            | x,y when x > y -> 3
            | x,y when y > x * 2 -> 5
            | _ -> 4
    let meleeHits hits uId mId gameState =
        let foundModel = tryFindModel gameState mId
        let foundTarget = tryFindUnit gameState uId
        match foundModel,foundTarget with
        | Some m, Some u -> 
            let rollForWounds diceAsker = 
                let avgToughness = 
                    u.UnitModels
                    |> Map.map (fun _ um -> um.Rules |> Map.filter (fun k _ -> k = Toughness.ToString())) 
                    |> Map.values
                    |> Seq.collect (Map.values)
                    |> Seq.choose(fun r -> match gameState with | Active r (ModelRule(Toughness(CharacteristicValue(ra)),mId)) -> Some ra | _ -> None)
                    |> Seq.maxmode 0
                let str = 
                    m.Model.Rules |> Map.values
                    |> Seq.choose(fun r -> match gameState with | Active r (ModelRule(Strength(CharacteristicValue(ra)),mId)) -> Some ra | _ -> None)
                    |> Seq.maxmode 0
                let toWound = woundTable str avgToughness
                let wounds = Seq.init hits (fun _ -> diceAsker()) |> Seq.filter (fun (DiceRoll x) -> x >= toWound) |> Seq.length
                let newRule = ModelRule(MeleeWounds(wounds,uId),mId)
                [newRule],tryReplaceRuleOnModel (MeleeWounds.ToString()) (def (Function(newRule))) mId gameState
            rollForWounds
        | _ -> failwith <| sprintf "Not found %A or %A" uId mId
    let meleeWounds wounds uId mId gameState = 
        let foundModel = tryFindModel gameState mId
        let foundTarget = tryFindUnit gameState uId
        match foundModel,foundTarget with
        | Some m, Some u -> 
            let rollForWounds diceAsker getNextTarget = 
                let save (um:UnitModel) = 
                    um.Rules 
                    |> Map.values
                    |> Seq.collect (Map.values)
                    |> Seq.choose(fun r -> match gameState with | Active r (ModelRule(Saves(CharacteristicValue(ra)),mId)) 
                                                                | Active r (ModelRule(InvSaves(CharacteristicValue(ra)),mId))
                                                                | Active r (ModelRule(CoverSaves(CharacteristicValue(ra)),mId))  -> Some ra 
                                                                | _ -> None)
                    |> Seq.sort 
                    |> Seq.tryNth 0
                    |> defaultArg <| 7
                let penetration = 3
                let saved = diceAsker() >= armourTable (u.UnitModels |> getNextTarget |> save) penetration   
                
                // let str = 
                //     m.Model.Rules |> Map.values
                //     |> Seq.choose(fun r -> match gameState with | Active r (ModelRule(Strength(CharacteristicValue(ra)),mId)) -> Some ra | _ -> None)
                //     |> Seq.maxmode 0
                // let toWound = woundTable str avgToughness
                // let wounds = Seq.init hits (fun _ -> diceAsker()) |> Seq.filter (fun (DiceRoll x) -> x >= toWound) |> Seq.length
                // let newRule = ModelRule(MeleeWounds(wounds,uId),mId)
                // [newRule],tryReplaceRuleOnModel (MeleeWounds.ToString()) (def (Function(newRule))) mId gameState
            rollForWounds
        | _ -> failwith <| sprintf "Not found %A or %A" uId mId
    let rec eval rules gameState = 
        match rules with
        | [] -> GameStateResult gameState
        | rule::rest -> 
            let eval' rest (newRules,gameState) = eval (rest @ newRules) gameState //Things that trigger recalculation
            rule |> function 
                | UnitRule(Deploy,uId) -> deploy uId gameState >> eval' rest |> Asker  |> PositionAsker |> AskResult              
                | UnitRule(Move maxMove,uId) -> move uId gameState maxMove >> eval rest |> Asker  |> MoveAsker |> AskResult
                | UnitRule(SetCharacteristicUnit(name, newRule), uId) -> tryReplaceRuleOnUnit name (Rule.overwrite newRule >> Some) uId gameState |> eval  rest
                | GameStateRule(EndPhase) -> advancePhase gameState |> eval' rest
                | GameStateRule(Remove(ruleApplication)) -> remove ruleApplication gameState |> eval rest
                | GameStateRule(Revert(ruleApplication)) -> revert ruleApplication gameState |> eval rest
                | GameStateRule(DeactivateUntil(activateWhen,ruleApplication)) -> deactivateUntil activateWhen ruleApplication gameState |> eval rest
                | Sequence(rules) -> eval (rules @ rest) gameState
                | ModelRule(Melee(attacks,toHit,target),mId) -> melee attacks toHit target mId gameState >> eval' rest |> Asker |> DiceRollAsker |> AskResult
                | ModelRule(MeleeHits(hits,uId),mId) -> meleeHits hits uId mId gameState >> eval' rest |> Asker |> DiceRollAsker |> AskResult
                | ModelRule(MeleeWounds(wounds,uId),mId) -> meleeWounds wounds uId mId gameState >> eval' rest |> Asker |> DiceRollAsker |> AskResult
                | ModelRule(WeaponSkill   (_),_) -> eval rest gameState 
                | ModelRule(BallisticSkill(_),_) -> eval rest gameState 
                | ModelRule(Strength      (_),_) -> eval rest gameState 
                | ModelRule(Toughness     (_),_) -> eval rest gameState 
                | ModelRule(Wounds        (_),_) -> eval rest gameState 
                | ModelRule(Initiative    (_),_) -> eval rest gameState 
                | ModelRule(Attacks       (_),_) -> eval rest gameState 
                | ModelRule(Leadership    (_),_) -> eval rest gameState 
                | ModelRule(InvSaves      (_),_) -> eval rest gameState 
                | ModelRule(Saves         (_),_) -> eval rest gameState 
                | GameStateRule(EndGame) -> remove (GameStateRule(PlayerTurn(Bottom))) gameState |> eval rest 
                | GameStateRule(EndTurn) -> eval rest gameState // Maybe Split EndPhase and End Turn
                | GameStateRule(GameRound(_))    -> eval  rest gameState
                | GameStateRule(PlayerTurn(_))   -> eval  rest gameState
                | UnitRule(DeploymentState(_),_) -> eval  rest gameState
                | GameStateRule(Noop)            -> eval  rest gameState

   