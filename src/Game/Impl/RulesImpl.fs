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

    let deploy uId gameState  = 
        let foundUnit = tryFindUnit gameState uId
        let foundPlayer = foundUnit |> Option.bind (tryFindPlayer gameState)
        match foundPlayer, foundUnit with
        | Some p, Some u -> 
            let pa positionAsker =
                let newRule = UnitRule(DeploymentState(Deployed), uId)
                let newGs = tryReplaceRuleOnUnit Rule.overriteOrNew (newRule |> Function) uId gameState
                let newUnit = tryFindUnit newGs uId |> defaultArg <| u
                [newRule],{ newGs with Board = { newGs.Board with Models = forAllModels (fun m -> { Model = m; Player = p.Player; Position = newGs |> positionAsker }) newUnit newGs} }
            pa
        | None, _ -> failwith "Couldn't find player"
        | _, None -> failwith "Couldn't find unit"

    let move uId gameState maxMove  = 
        let createMove moveAsker =
            let newPosition m = 
                let ps = pixelsInCircle maxMove m.Position |> Seq.toArray
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
            let (gtMaker, phase) = splitRound round
           
            let mapReplace rule = Map.updateWithOrRemove (def (Function(GameStateRule(rule)))) (rule.ToString())
            let doGameState rule = replaceRuleOnGameState <| mapReplace rule
            
            let (rules,gameStateChanges) =
                match phase with
                | Some Phase.Begin -> turn, gtMaker Phase.Movement
                | Some Phase.Movement ->  turn, gtMaker Phase.Psychic
                | Some Phase.Psychic ->  turn, gtMaker Phase.Shooting
                | Some Phase.Shooting ->  turn, gtMaker Phase.Assault
                | Some Phase.Assault ->  turn, gtMaker Phase.End
                | Some Phase.End | None -> otherPlayerTurn (turn,round)
                |> (fun (turn,round) -> [PlayerTurn(turn); GameRound(round)])
                |> List.map (fun r -> GameStateRule(r), doGameState r)
                |> List.unzip 
            rules,(gameStateChanges |> List.reduce (>>)) gs
        match Map.tryFind (GameRound(Begin).ToString()) gs.Rules,Map.tryFind (PlayerTurn(Top).ToString()) gs.Rules with
        | Some(Function(GameStateRule(GameRound(round)))), Some(Function(GameStateRule(PlayerTurn(turn)))) -> changeRound(round,turn) gs
        | _ -> failwith <| sprintf "Couldn't find game round or playerturn %A" gs.Rules

    let rec revert ruleApplication gameState = 
        let rule = tryFindRule ruleApplication gameState
        match ruleApplication,rule with 
        | UnitRule(_,uId),Some rule -> 
            tryReplaceRuleOnUnit Rule.unoverwriteOrNew rule uId gameState
        | ModelRule(_,mId),Some rule ->
            tryReplaceRuleOnModel Rule.unoverwriteOrNew rule mId gameState
        | GameStateRule(_),Some rule ->
            tryReplaceRuleOnGameState Rule.unoverwriteOrNew rule gameState
        | Sequence(rule::_),Some _ ->
            revert rule gameState 
        | Sequence([]),_ 
        | _,None -> gameState
    let rec remove ruleApplication gameState = 
        let rule = tryFindRule ruleApplication gameState
        match ruleApplication,rule with 
        | UnitRule(_,uId),Some rule -> 
            tryReplaceRuleOnUnit defnot rule uId gameState
        | ModelRule(_,mId),Some rule ->
            tryReplaceRuleOnModel defnot rule mId gameState
        | GameStateRule(_),Some rule ->
            tryReplaceRuleOnGameState defnot rule gameState
        | Sequence(rule::_),Some _ ->
            remove rule gameState 
        | Sequence([]),_ 
        | _,None -> gameState
    let repeat times name rule gameState = 
        if times > 0 then
            tryReplaceRuleOnGameState def (rule |> Rule.afterRunRepeat (times - 1) name) gameState
        else
            gameState
    let rec deactivateUntil activateWhen ruleApplication gameState  =
        let makeNewRule rule oldRule = 
            Option.map(function oldRule -> Overwritten(ActiveWhen(activateWhen,rule),oldRule)) oldRule
        match ruleApplication with 
        | UnitRule(_,uId) -> 
            tryReplaceRuleOnUnit makeNewRule (Function(GameStateRule(Revert(ruleApplication)))) uId gameState
        | ModelRule(_,mId) ->
            tryReplaceRuleOnModel makeNewRule (Function(GameStateRule(Revert(ruleApplication)))) mId gameState
        | GameStateRule(_) ->
            tryReplaceRuleOnGameState makeNewRule (Function(GameStateRule(Revert(ruleApplication)))) gameState
        | Sequence(rule::_) ->
            deactivateUntil activateWhen rule gameState
        | Sequence([]) -> gameState

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
    let diceRoll rule f gameState =
        rule
        |> Rule.afterIfRemove (goodRolls D6 (f gameState))
    let multipleFromDiceRollM rule f times mId gameState  = 
        let text = rule |> Rule.textFromRuleApplication
        let newRule = 
            diceRoll rule f gameState
            |> Rule.afterRunRepeat (times - 1) text

        gameState
        |> tryReplaceRuleOnModel def doDice mId
        |> tryReplaceRuleOnModel def newRule mId
    let multipleFromDiceRollU rule f times uId gameState  = 
        let text = rule |> Rule.textFromRuleApplication
        let newRule = 
            diceRoll rule f gameState
            |> Rule.afterRunRepeat (times - 1) text

        gameState
        |> tryReplaceRuleOnUnit def doDice uId
        |> tryReplaceRuleOnUnit def newRule uId
        
    let toWound m u gameState = woundTable (modelStrength m gameState) (avgToughness u gameState)

    //let assault = .... //May have to change this to adding a Targetted rule to target, which adds a Melee to model 
    let melee attacks toHit target mId gameState = 
        let newRule = ModelRule(MeleeHit(1,target),mId)
        [newRule],multipleFromDiceRollM newRule (fun _ -> toHit) attacks mId gameState

    let meleeHits hits uId mId gameState =
        let foundModel = tryFindModel gameState mId
        let foundTarget = tryFindUnit gameState uId
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

            gameState
            |> tryReplaceRuleOnModel defnot (Function(GameStateRule(DiceRolled(DiceRoll(1))))) mId
            |> multipleFromDiceRollU newRule (toWound m u) hits uId
        | _ -> failwith <| sprintf "Not found %A or %A" uId mId
        
    let woundPool profiles attackingModelId uId gameState =

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

        gameState
        |> tryReplaceRuleOnGameState def dosortedIndexes
        |> tryReplaceRuleOnUnit def newPool uId

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

    let sortedWoundPool (profiles: list<int * WeaponProfile>) target mId gameState = 
        let r = 
            gameState.Rules 
            |> Map.tryPick(fun _ r -> match gameState with 
                                      | Active r (GameStateRule(SortedWeaponProfiles(ra))) -> Some ra 
                                      | _ -> None)
        match r with
        | Some sortedProfiles  -> 
            let sortedProfiles' = List.zip profiles sortedProfiles |> List.sortBy snd 
            match sortedProfiles' with 
            | [] -> gameState
            | ((times,profile),sortNum)::rest ->
                let newProfile,newSorted = List.unzip rest |> fun (pr,srt) -> 
                                                                if times > 0 then
                                                                    (times-1,profile)::pr,sortNum::srt
                                                                else
                                                                    pr,srt
                let newRule = Function(UnitRule(Save(profile,mId),target)) |> Rule.afterRunRemove
                gameState
                |> tryReplaceRuleOnGameState def (Function(GameStateRule(SortedWeaponProfiles(newSorted))))
                |> tryReplaceRuleOnUnit def (Function(UnitRule(SortedWoundPool(newProfile, mId),target))) target
                |> tryReplaceRuleOnUnit def newRule target
        | x -> failwith <| sprintf "Not found - weapon profile sort %A" x
    let pickClosest mId uId gameState = //TODO change impl to something. Probably have to roll position into GameState
        let foundUnit = tryFindUnit gameState uId 
        let foundModel = tryFindModel gameState mId
        let foundAssaulters =  Option.bind (fun m -> tryFindUnitByModel gameState m.Model) foundModel
        match (foundAssaulters,foundUnit) with
        | Some _,Some u ->
            u.UnitModels |> Map.values |> Seq.head |> Some
        | _ -> None
    let saveWound profile mId uId gameState = 
        let picked = pickClosest  mId uId gameState
        match picked with 
        | Some m -> 
            let newRule = ModelRule(Unsaved(profile),m.Id)
            
            [newRule],gameState |> multipleFromDiceRollM newRule (save m) 1 mId
        | _ -> failwith <| sprintf "Not found model in %A" uId

    let unsavedWound _ mId gameState =
        let foundModel = tryFindModel gameState mId
        let wounds = foundModel |> Option.bind (fun m ->  
                                            m.Model.Rules 
                                            |> Map.values
                                            |> Seq.tryPick(fun r -> match gameState with | Active r (ModelRule(Wounds(CharacteristicValue(w)),_)) -> Some w 
                                                                                         | _ -> None))
        let foundUnit =  Option.bind (fun m -> tryFindUnitByModel gameState m.Model) foundModel
        let foundPlayer = Option.bind (tryFindPlayer gameState) foundUnit
        match wounds, foundUnit, foundPlayer,foundModel with
        | Some wounds, Some _, Some _, Some _ ->
            let woundRule  = Function(ModelRule(Wounds(wounds - 1 |> CharacteristicValue),mId))
            let newRule = (SetCharacteristic(woundRule),mId) |> ModelRule 
            [newRule],tryReplaceRuleOnGameState def (newRule |> Function |> Rule.afterRunRemove) gameState
        | _ -> failwith <| sprintf "Not found %A "  mId
    let removeIfZeroCharacteristic  mId gameState = 
        let foundModel = tryFindModel gameState mId
        let foundUnit =  Option.bind (fun m -> tryFindUnitByModel gameState m.Model) foundModel
        let foundPlayer = Option.bind (tryFindPlayer gameState) foundUnit
        match foundUnit, foundPlayer,foundModel with
        | Some u, Some p, Some m ->
            replaceUnitModelsInGameState gameState p u m.Model (defnot None)
        | _ -> failwith <| sprintf "Not found %A "  mId
    let supplySortedWeapons profiles gameState profileMaker = 
        let newRule = profileMaker profiles |> SortedWeaponProfiles |> GameStateRule 
        [newRule],tryReplaceRuleOnGameState def (newRule |> Function) gameState
    let rollDice gameState diceRoller =
        let newRule = diceRoller() |> DiceRolled |> GameStateRule
        [newRule],tryReplaceRuleOnGameState def (newRule |> Function) gameState

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

    let collect gameState = 
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
                let newRule = Activate ra |> GameStateRule |> Function |> Rule.afterRunRemove
                tryReplaceRuleOnGameState def newRule gameState
            | _ -> gameState
        rulesApplication

    let rec activate ra gameState = 
        match ra with
        | GameStateRule(_) -> {gameState with GameState.Rules = activateRule ra gameState.Rules}
        | UnitRule(_, uId) ->
            tryFindUnit gameState uId |> Option.map (fun u -> replaceRuleOnUnit u (activateRule ra) gameState) |> defaultArg <| gameState
        | ModelRule(_, mId) -> 
            tryFindModel gameState mId |> Option.map (fun m -> replaceRuleOnModel m.Model (activateRule ra) gameState) |> defaultArg <| gameState
        | Sequence(first::_) -> activate first gameState
        | Sequence([]) -> gameState

    let rec eval rules gameState = 
        match rules with
        | [] -> GameStateResult gameState
        | rule::rest -> 
            //Things that trigger recalculation
            //Could change this to infinite doublly linked list. 
            // current @ rest @ begin  
            // Modify rest, prepend to reset, append to rest
            let eval' rest (newRules,gameState) = eval (rest @ newRules) gameState 
            rule |> function 
                | UnitRule(Deploy,uId) -> deploy uId gameState >> eval' rest |> Asker  |> PositionAsker |> AskResult              
                | UnitRule(Move maxMove,uId) -> move uId gameState maxMove >> eval rest |> Asker  |> MoveAsker |> AskResult
                | GameStateRule(RollDice) -> rollDice gameState >> eval' rest |> Asker |> DiceRollAsker |> AskResult
                | GameStateRule(SupplySortedWeaponProfiles(profiles)) -> supplySortedWeapons profiles gameState >> eval' rest |> Asker |> SortedWoundPoolAsker |> AskResult
                | ModelRule(SetCharacteristic(newRule), uId) -> tryReplaceRuleOnModel Rule.overriteOrNone newRule uId gameState |> eval  rest
                | GameStateRule(EndPhase) -> advancePhase gameState |> eval' rest
                | GameStateRule(Remove(ruleApplication)) -> remove ruleApplication gameState |> eval rest
                | GameStateRule(Revert(ruleApplication)) -> revert ruleApplication gameState |> eval rest
                | GameStateRule(Repeat(times,name,rule)) -> repeat times name rule gameState |> eval rest
                | GameStateRule(Activate(ra)) -> activate ra gameState |> eval rest
                | GameStateRule(DeactivateUntil(activateWhen,ruleApplication)) -> deactivateUntil activateWhen ruleApplication gameState |> eval rest
                | Sequence(rules) -> eval (rules @ rest) gameState
                | ModelRule(Melee(attacks,toHit,target),mId) -> melee attacks toHit target mId gameState |> eval' rest
                | ModelRule(MeleeHit(hits,uId),mId) -> meleeHits hits uId mId gameState |> eval rest 
                | UnitRule(WoundPool(profiles,mId),uId) -> woundPool profiles mId uId gameState |> eval rest
                | UnitRule(SortedWoundPool(profiles,mId),uId) -> sortedWoundPool profiles mId uId gameState |> eval rest 
                | UnitRule(Save(profile,mId),uId) -> saveWound profile mId uId gameState |> eval' rest
                | ModelRule(Unsaved(profile),mId) -> unsavedWound profile mId gameState |> eval' rest
                | GameStateRule(EndGame) -> remove (GameStateRule(PlayerTurn(Bottom))) gameState |> eval rest 
                | ModelRule(RemoveOnZeroCharacteristic, mId) -> removeIfZeroCharacteristic mId gameState |> eval rest
                | GameStateRule(CollectUserActivated) -> collect gameState >> eval rest |> Asker  |> PerformAsker |> AskResult
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
    