namespace GameImpl

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
        | Nested(Optional r,rules) -> 
            let newRules = List.map (function Optional r -> r | r -> r) rules
            (r,newRules)|> Nested |> Some      
        | Nested(r,rules) -> 
            let newRules = List.map (function Optional r -> r,1 | r -> r,0) rules
            let isOp = List.tryFind (snd >> (=) 2) newRules
            if isOp.IsSome then
                (r,(newRules |> List.map fst))|> Nested |> Some
            else
                None
    let rec (|Active|_|) r gameState = 
        match r with 
        | ActiveWhen(logic, innerRule) -> contains gameState logic innerRule |> Option.bind (fun _ -> match gameState with Active innerRule rule -> Some rule | _ -> None)
        | UserActivated (userActivated) ->  match gameState with Active userActivated rule -> Some rule | _ -> None
        | Function app -> Some app
        | Description _ -> Some (GameStateRule Noop)
        | Overwritten (overwrite,_) -> match gameState with Active overwrite rule -> Some rule | _ -> None
        | Nested(r, rules) -> 
            let matchFst = match gameState with Active r rule -> Some rule | _ -> None
            match matchFst with 
            | Some _ -> matchFst
            | None -> List.tryPick(fun r -> match gameState with Active r rule -> Some rule | _ -> None) rules

    and tryFindInRuleList (gameState:GameState) ruleApplication rl = 
        rl |> Option.bind (Map.tryFindKey (fun k foundRule -> match ruleApplication with 
                                                                | GameStateRule impl ->  k = impl.ToString() && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | ModelRule (impl, _) ->  k = impl.ToString() && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | UnitRule(impl, _) ->  k = impl.ToString() && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | Sequence(impl::_) -> k = impl.ToString() && match gameState with Active foundRule r -> r = ruleApplication | _ -> false
                                                                | Sequence([]) -> false
                                                                ))
    and contains gameState (logic:LogicalExpression) (rule:Rule)  = 
        match logic with 
        | Rule ruleApplication -> 
            let rec tryFind = function 
                            | GameStateRule _ ->  Some gameState.Rules
                            | ModelRule (_, mId) ->  eval (runT (tryFindModel mId)) gameState |> (Option.map (fun m -> m.Model.Rules))
                            | UnitRule(_, uId) -> eval (runT (tryFindUnit uId)) gameState |>  (Option.map (fun u -> u.Rules))
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
    let optionalAndActiveRules gs (_,r) = match r,gs with Optional _, Active r ra -> Some ra | _ -> None
    let activeRulesButNotOptional gs (_,r) = 
        match gs with 
        | Active r ra -> 
            match r with 
            | Optional _ -> None
            | _ -> Some ra
        | _ -> None
    let activeRules gs (_,r) = match gs with Active r ra -> Some ra | _ -> None
    let rec (|MR|_|) = function
        | UnitRule(_) -> None
        | GameStateRule(_) -> None
        | ModelRule(mri,_) -> Some mri
        | ModelRule(_) -> None
        | Sequence([]) -> None
        | Sequence((MR r)::_) -> Some r
        | Sequence(_::tail) -> List.tryPick (function MR r -> Some r | _ -> None) tail

    let activateRule r rules = 
        let matchName = function
            | UnitRule(r,_) -> r.ToString()
            | ModelRule(r,_) -> r.ToString()
            | GameStateRule(r) -> r.ToString()
            | Sequence(r::_) -> r.ToString()
            | Sequence([]) -> ""
        let name = matchName r
        Map.updateWithOrRemove (overriteOrNew (Function(r))) name rules

    let deploy uId = game { 

        let! foundUnit = tryFindUnit uId
        let! foundPlayer = optBindM tryFindPlayer foundUnit
        match foundPlayer, foundUnit with
        | Some p, Some u -> 
            let pa positionAsker = game {
                let newRule = UnitRule(DeploymentState(Deployed), uId)
                do! tryReplaceRuleOnUnit Rule.overriteOrNew (newRule |> Function) uId
                let! newUnit = tryFindUnit uId
                match newUnit with 
                | Some u -> 
                    let! newGs = getState
                    let askPosition m = { Model = m; Player = p.Player; Position = positionAsker newGs }
                    do! forAllModels (map askPosition) u
                | None -> ()
            }
            return pa
    }

    let mapPositions moveAsker maxMove (p:PlayerInfo) = 
        let innerFn (m:Model) = game {
            let getPositions m = 
                let ps = pixelsInCircle maxMove m.Position |> Seq.toArray
                let rec newPick ps = 
                    let (p:Position<px>) = moveAsker ps
                    if Seq.contains p ps then p
                    else newPick ps
                newPick ps 
            let! newValue = tryFindModel m.Id 
            let newValue' = newValue |> Option.get |> getPositions 
            return { Model = m; Player = p.Player; Position = newValue' }
            }
        bind innerFn
    let move uId maxMove  = game {
        let! foundUnit = tryFindUnit uId
        let! foundPlayer = optBindM tryFindPlayer foundUnit
        match foundPlayer, foundUnit with
            | Some p, Some u ->
                let createMove moveAsker = game {
                    let newPosition = mapPositions moveAsker maxMove p   
                    do! forAllModels newPosition u
                }
                return createMove
    }

    let advancePhase = game { 

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
            let (gtMaker, phase) = splitRound round
           
            let mapReplace rule = Map.updateWithOrRemove (def (Function(GameStateRule(rule)))) (rule.ToString())
            let doGameState rule = replaceRuleOnGameState <| mapReplace rule
            
            let gameStateChanges =
                match phase with
                | Some Phase.Begin -> turn, gtMaker Phase.Movement
                | Some Phase.Movement ->  turn, gtMaker Phase.Psychic
                | Some Phase.Psychic ->  turn, gtMaker Phase.Shooting
                | Some Phase.Shooting ->  turn, gtMaker Phase.Assault
                | Some Phase.Assault ->  turn, gtMaker Phase.End
                | Some Phase.End | None -> otherPlayerTurn (turn,round)
                |> (fun (turn,round) -> [PlayerTurn(turn); GameRound(round)])
                |> List.map (fun r -> game { do! doGameState r } )
            sequence gameStateChanges
        let! gs = getState
        match Map.tryFind (GameRound(Begin).ToString()) gs.Rules,Map.tryFind (PlayerTurn(Top).ToString()) gs.Rules with
        | Some(Function(GameStateRule(GameRound(round)))), Some(Function(GameStateRule(PlayerTurn(turn)))) -> return! changeRound(round,turn)
        | _ -> return []
    }
    let rec revert ruleApplication = game { 
        let! rule = tryFindRule ruleApplication 
        match ruleApplication,rule with 
        | UnitRule(_,uId),Some rule -> 
            do! tryReplaceRuleOnUnit Rule.unoverwriteOrNew rule uId 
        | ModelRule(_,mId),Some rule ->
            do! tryReplaceRuleOnModel Rule.unoverwriteOrNew rule mId 
        | GameStateRule(_),Some rule ->
            do! tryReplaceRuleOnGameState Rule.unoverwriteOrNew rule 
        | Sequence(rule::_),Some _ ->
            return! revert rule  
        | Sequence([]),_ 
        | _,None -> ()
    }
    let rec remove ruleApplication = game {
        let! rule = tryFindRule ruleApplication 
        match ruleApplication,rule with 
        | UnitRule(_,uId),Some rule -> 
            do! tryReplaceRuleOnUnit defnot rule uId 
        | ModelRule(_,mId),Some rule ->
            do! tryReplaceRuleOnModel defnot rule mId 
        | GameStateRule(_),Some rule ->
            do! tryReplaceRuleOnGameState defnot rule 
        | Sequence(rule::_),Some _ ->
            return! remove rule  
        | Sequence([]),_ 
        | _,None -> ()
    }
    let repeat times name rule = game {
        if times > 0 then
          do! tryReplaceRuleOnGameState def (rule |> Rule.afterRunRepeat (times - 1) name)         
    }
    let rec deactivateUntil activateWhen ruleApplication = game {
        let makeNewRule rule oldRule = 
            Option.map(function oldRule -> Overwritten(ActiveWhen(activateWhen,rule),oldRule)) oldRule
        match ruleApplication with 
        | UnitRule(_,uId) -> 
            do! tryReplaceRuleOnUnit makeNewRule (Function(GameStateRule(Revert(ruleApplication)))) uId 
        | ModelRule(_,mId) ->
            do! tryReplaceRuleOnModel makeNewRule (Function(GameStateRule(Revert(ruleApplication)))) mId 
        | GameStateRule(_) ->
            do! tryReplaceRuleOnGameState makeNewRule (Function(GameStateRule(Revert(ruleApplication)))) 
        | Sequence(rule::_) ->
            return! deactivateUntil activateWhen rule 
        | Sequence([]) -> ()
    }

    let avgToughness u gameState = 
        u.UnitModels
        |> Map.map (fun _ um -> um.Rules |> Map.filter (fun k _ -> k = Toughness.ToString())) 
        |> Map.values
        |> Seq.collect (Map.values)
        |> Seq.choose(fun r -> match gameState with | Active r (ModelRule(Toughness(CharacteristicValue(ra)),_)) -> Some ra | _ -> None)
        |> Seq.maxmode 0
    let modelStrength m gameState = 
        m.Model.Rules 
        |> Map.tryFind (Strength.ToString())
        |> Option.bind(fun r -> match gameState with | Active r (ModelRule(Strength(CharacteristicValue(ra)),_)) -> Some ra | _ -> None)
        |> defaultArg <| 0
    let doDice = Function(GameStateRule(RollDice)) |> Rule.afterRunRemove
    let diceRoll rule f = game {
        let! gameState = getState
        return rule |> Rule.afterIfRemove (goodRolls D6 (f gameState))
    }
    let multipleFromDiceRollM rule f times mId = game {
        let text = rule |> Rule.textFromRuleApplication
        let! newRule = 
            pipe (diceRoll rule f) (Rule.afterRunRepeat (times - 1) text)
        
        do! tryReplaceRuleOnModel def doDice mId
        do! tryReplaceRuleOnModel def newRule mId
    }
    let multipleFromDiceRollU rule f times uId = game {
        let text = rule |> Rule.textFromRuleApplication
        let! newRule = 
            pipe (diceRoll rule f) (Rule.afterRunRepeat (times - 1) text)

        do! tryReplaceRuleOnUnit def doDice uId 
        do! tryReplaceRuleOnUnit def newRule uId
    }
        
    let toWound m u gameState = woundTable (modelStrength m gameState) (avgToughness u gameState)

    //let assault = .... //May have to change this to adding a Targetted rule to target, which adds a Melee to model 
    let melee attacks toHit target mId = game { 
        let newRule = ModelRule(MeleeHit(1,target),mId)
        do! multipleFromDiceRollM newRule (fun _ -> toHit) attacks mId 
        return newRule
    }
    let meleeHits hits uId mId = game {
        let! foundModel = tryFindModel mId
        let! foundTarget = tryFindUnit uId
        let! gameState = getState
        match foundModel,foundTarget with
        | Some m, Some u -> 
            let newWounds = [1,[]] //List of hits,weapon profiles
            
            let newRule =  
                u.Rules
                |> Map.tryFind (WoundPool.ToString())
                |> Option.bind (fun r -> match gameState with 
                                            | Active r (UnitRule(WoundPool(wounds,mId),uId)) -> Some(UnitRule(WoundPool((List.append wounds newWounds),mId),uId))
                                            | _ -> None)
                |> defaultArg <|  UnitRule(WoundPool(newWounds,mId),uId)

            do! tryReplaceRuleOnModel defnot (Function(GameStateRule(DiceRolled(DiceRoll(1))))) mId
            do! multipleFromDiceRollU newRule (toWound m u) hits uId
        | _ -> failwith <| sprintf "Not found %A or %A" uId mId
    }
    let woundPool profiles attackingModelId uId = game {

        let profiles' = 
            profiles 
            |> List.groupBy (snd) 
            |> List.map (fun (profile,woundProfiles) -> woundProfiles |> Seq.sumBy fst, profile)
        let dosortedIndexes = 
            profiles'
            |> SupplySortedWeaponProfiles 
            |> GameStateRule 
            |> Function 
            |> Rule.afterRunRemove
        let newPool = 
            UnitRule(SortedWoundPool(profiles', attackingModelId),uId)  
            |> Rule.afterIfRemove (Rule(GameStateRule(SortedWeaponProfiles([]))))

        do! tryReplaceRuleOnGameState def dosortedIndexes
        do! tryReplaceRuleOnUnit def newPool uId
    }
    let save (um:Model) gameState = 
        um.Rules 
        |> Map.choose(fun _ r -> match gameState with
                                 | Active r (ModelRule(Saves(CharacteristicValue(ra)),mId)) 
                                 | Active r (ModelRule(InvSaves(CharacteristicValue(ra)),mId))
                                 | Active r (ModelRule(CoverSaves(CharacteristicValue(ra)),mId))  -> Some ra 
                                 | _ -> None)
        |> Map.values
        |> Seq.sort 
        |> Seq.tryNth 0
        |> defaultArg <| 7
        |> DiceRoll
    let collectPenetrations  = List.tryPick(function | MR(ArmourPenetration(ra)) -> ra | _ -> None)

    let sortedWoundPool (profiles: list<int * WeaponProfile>) target mId = game { 
        let! gameState = getState
        let r = 
            gameState.Rules 
            |> Map.tryPick(fun _ r -> match gameState with 
                                      | Active r (GameStateRule(SortedWeaponProfiles(ra))) -> Some ra 
                                      | _ -> None)
        match r with
        | Some sortedProfiles  -> 
            let sortedProfiles' = List.zip profiles sortedProfiles |> List.sortBy snd 
            match sortedProfiles' with 
            | [] -> ()
            | ((times,profile),sortNum)::rest ->
                let newProfile,newSorted = List.unzip rest |> fun (pr,srt) -> 
                                                                if times > 0 then
                                                                    (times-1,profile)::pr,sortNum::srt
                                                                else
                                                                    pr,srt
                let newRule = Function(UnitRule(Save(profile,mId),target)) |> Rule.afterRunRemove
                
                do! tryReplaceRuleOnGameState def (Function(GameStateRule(SortedWeaponProfiles(newSorted))))
                do! tryReplaceRuleOnUnit def (Function(UnitRule(SortedWoundPool(newProfile, mId),target))) target
                do! tryReplaceRuleOnUnit def newRule target
        | x -> failwith <| sprintf "Not found - weapon profile sort %A" x
    }
    let pickClosest mId uId = game {//TODO change impl to something. Probably have to roll position into GameState
        let! foundUnit = tryFindUnit uId 
        let! foundModel = tryFindModel mId
        let! foundAssaulters = optBindM (fun m -> tryFindUnitByModel m.Model) foundModel
        match (foundAssaulters,foundUnit) with
        | Some _,Some u ->
            return u.UnitModels |> Map.values |> Seq.head |> Some
        | _ -> return None
    }
    let saveWound profile mId uId = game {
        let! picked = pickClosest mId uId 
        match picked with 
        | Some m -> 
            let newRule = ModelRule(Unsaved(profile),m.Id)
            do! multipleFromDiceRollM newRule (save m) 1 mId
        | _ -> failwith <| sprintf "Not found model in %A" uId
    }
    let unsavedWound _ mId = game {
        let! foundModel = tryFindModel mId
        let! gameState = getState
        let wounds = foundModel |> Option.bind (fun m ->  
                                            m.Model.Rules 
                                            |> Map.values
                                            |> Seq.tryPick(fun r -> match gameState with | Active r (ModelRule(Wounds(CharacteristicValue(w)),_)) -> Some w 
                                                                                         | _ -> None))
        let! foundUnit =  optBindM (fun m -> tryFindUnitByModel m.Model) foundModel
        let! foundPlayer = optBindM (tryFindPlayer) foundUnit
        match wounds, foundUnit, foundPlayer,foundModel with
        | Some wounds, Some _, Some _, Some _ ->
            let woundRule  = Function(ModelRule(Wounds(wounds - 1 |> CharacteristicValue),mId))
            let newRule = (SetCharacteristic(woundRule),mId) |> ModelRule 
            do! tryReplaceRuleOnGameState def (newRule |> Function |> Rule.afterRunRemove) 
        | _ -> failwith <| sprintf "Not found %A "  mId
    }
    
    // let removeIfZeroCharacteristic  mId = 
    //     tryFindModel mId 
    //     >>= fun foundModel -> 
    //             optBindM (fun m -> tryFindUnitByModel m.Model) foundModel
    //             >>= fun foundUnit -> 
    //                 optBindM tryFindPlayer foundUnit
    //                 >>= fun foundPlayer ->            
    //                         match foundUnit, foundPlayer,foundModel with
    //                         | Some u, Some p, Some m ->
    //                             replaceUnitModelsInGameState p u m.Model (defnot None)
    //                         | _ -> failwith <| sprintf "Not found %A "  mId
    
    let removeIfZeroCharacteristic  mId = game {
        let! foundModel = tryFindModel mId
        let! foundUnit =  optBindM (fun m -> tryFindUnitByModel m.Model) foundModel
        let! foundPlayer = optBindM tryFindPlayer foundUnit
        match foundUnit, foundPlayer,foundModel with
        | Some u, Some p, Some m ->
            do! replaceUnitModelsInGameState p u m.Model (defnot None)
        | _ -> failwith <| sprintf "Not found %A "  mId
    }
    let supplySortedWeapons profiles profileMaker = game {
        let newRule = profileMaker profiles |> SortedWeaponProfiles |> GameStateRule 
        do! tryReplaceRuleOnGameState def (newRule |> Function)
    }
    let rollDice diceRoller = game {
        let newRule = diceRoller() |> DiceRolled |> GameStateRule
        do! tryReplaceRuleOnGameState def (newRule |> Function) 
    }
    let availableRules predicate player gs = 
        let captureRules =  Map.toSeq >> Seq.choose predicate >> Seq.toList

        let gameRules = captureRules gs.Rules
        
        let unitRules = 
            gs.Players
            |> List.filter (fun p -> p.Player = player)
            |> List.map (fun p -> p.Units) 
            |> List.exactlyOne
            |> Map.toList
            |> List.collect (fun (_,item) ->  captureRules item.Rules)

        let modelRules = 
            gs.Board.Models 
            |> Map.filter (fun _ item -> item.Player = player) 
            |> Map.toList
            |> List.collect (fun (_,item) -> captureRules item.Model.Rules)

        gameRules @ unitRules @ modelRules |> List.rev

    let collect = game {
        let! gameState = getState
        let predicate = optionalAndActiveRules gameState
        let currentPlayer = 
            gameState.Rules 
            |> Map.pick(fun _ r -> match gameState with | Active r (GameStateRule(PlayerTurn(Top))) -> Some Player1
                                                           | _ ->  Some Player2)
            
        let rules  = 
            gameState
            |> availableRules predicate currentPlayer
        let rulesApplication raPicker = 
            match rules |> List.tryItem (raPicker rules) with
            | Some ra ->
                game {
                    let newRule = Activate ra |> GameStateRule |> Function |> Rule.afterRunRemove
                    do! tryReplaceRuleOnGameState def newRule 
                }
            | _ -> game {return ()}
        return rulesApplication
    }

    let rec activate ra = game {
        match ra with
        | GameStateRule(_) -> 
            let! gameState = getState
            do! putState {gameState with GameState.Rules = activateRule ra gameState.Rules} 
        | UnitRule(_, uId) ->
                let! unit = tryFindUnit uId
                match unit with 
                | Some u -> do! replaceRuleOnUnit u (activateRule ra)
                | None -> ()
        | ModelRule(_, mId) -> 
                let! model = tryFindModel mId
                match model with 
                | Some m -> do! replaceRuleOnModel m.Model (activateRule ra)
                | None -> ()
        | Sequence(first::_) -> return! activate first 
        | Sequence([]) -> ()
    }
    let rec eval rules gameState = 
        match rules with
        | [] -> GameStateResult gameState
        | rule::rest -> 
            //Things that trigger recalculation
            //Could change this to infinite doublly linked list. 
            // current @ rest @ begin  
            // Modify rest, prepend to reset, append to rest
            let eval' rest (newRules,gameState) = eval (rest @ newRules) gameState 
            let runT' f gameState =
                let (f',s) = runT f gameState
                let flipRunT result = runT result s |> snd |> eval rest
                f' >> flipRunT
            let runT f gameState =
                (runT f gameState)  |> snd |> eval rest
            rule |> function 
                | UnitRule(Deploy,uId) -> runT' (deploy uId) gameState |> Asker  |> PositionAsker |> AskResult              
                | UnitRule(Move maxMove,uId) -> runT' (move uId maxMove) gameState |> Asker  |> MoveAsker |> AskResult
                | GameStateRule(RollDice) -> runT' (returnM rollDice) gameState |> Asker |> DiceRollAsker |> AskResult
                | GameStateRule(SupplySortedWeaponProfiles(profiles)) -> runT' (returnM (supplySortedWeapons profiles)) gameState |> Asker |> SortedWoundPoolAsker |> AskResult
                | ModelRule(SetCharacteristic(newRule), uId) -> runT (tryReplaceRuleOnModel Rule.overriteOrNone newRule uId) gameState
                | GameStateRule(EndPhase) -> runT (advancePhase) gameState 
                | GameStateRule(Remove(ruleApplication)) -> runT (remove ruleApplication) gameState 
                | GameStateRule(Revert(ruleApplication)) -> runT (revert ruleApplication) gameState 
                | GameStateRule(Repeat(times,name,rule)) -> runT (repeat times name rule) gameState 
                | GameStateRule(Activate(ra)) -> runT (activate ra) gameState 
                | GameStateRule(DeactivateUntil(activateWhen,ruleApplication)) -> runT (deactivateUntil activateWhen ruleApplication) gameState
                | Sequence(rules) -> eval (rules @ rest) gameState
                | ModelRule(Melee(attacks,toHit,target),mId) -> runT (melee attacks toHit target mId) gameState 
                | ModelRule(MeleeHit(hits,uId),mId) -> runT (meleeHits hits uId mId) gameState  
                | UnitRule(WoundPool(profiles,mId),uId) -> runT (woundPool profiles mId uId) gameState 
                | UnitRule(SortedWoundPool(profiles,mId),uId) -> runT (sortedWoundPool profiles mId uId) gameState 
                | UnitRule(Save(profile,mId),uId) -> runT (saveWound profile mId uId) gameState
                | ModelRule(Unsaved(profile),mId) -> runT (unsavedWound profile mId) gameState 
                | GameStateRule(EndGame) -> runT (remove (GameStateRule(PlayerTurn(Bottom)))) gameState 
                | ModelRule(RemoveOnZeroCharacteristic, mId) -> runT (removeIfZeroCharacteristic mId)  gameState
                | GameStateRule(CollectUserActivated) -> runT' (collect) gameState |> Asker  |> PerformAsker |> AskResult
                | GameStateRule(EndTurn) // Maybe Split EndPhase and End Turn
                | GameStateRule(DiceRolled(_))  
                | GameStateRule(SortedWeaponProfiles(_))  
                | ModelRule(WeaponSkill   (_),_)
                | ModelRule(BallisticSkill(_),_)
                | ModelRule(Strength      (_),_)
                | ModelRule(Toughness     (_),_)
                | ModelRule(Wounds        (_),_)
                | ModelRule(Initiative    (_),_)
                | ModelRule(Attacks       (_),_)
                | ModelRule(Leadership    (_),_)
                | ModelRule(InvSaves      (_),_)
                | ModelRule(Saves         (_),_)
                | ModelRule(CoverSaves         (_),_)
                | ModelRule(ArmourPenetration(_),_)
                | GameStateRule(GameRound(_))    
                | GameStateRule(PlayerTurn(_))  
                | UnitRule(DeploymentState(_),_) 
                | GameStateRule(Noop)            -> eval  rest gameState
    