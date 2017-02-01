namespace GameImpl

module RulesImpl = 
    open Domain.WarhammerDomain
    open Domain
    open Domain.Tabletop
    open Domain.Game
    open GameImpl.GameState
    open Microsoft.FSharp.Collections
    open FSharpx.Collections

    let (&&>>) f g x y=
            f x y && g x y 
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
    
    let nextGameRound = function
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
        | Bottom,x -> Top,nextGameRound x
    let calculateNextPhase (turn,round) =
        let (gtMaker, phase) = splitRound round
        match phase with
        | Some Phase.Begin -> turn, gtMaker Phase.Movement
        | Some Phase.Movement ->  turn, gtMaker Phase.Psychic
        | Some Phase.Psychic ->  turn, gtMaker Phase.Shooting
        | Some Phase.Shooting ->  turn, gtMaker Phase.Assault
        | Some Phase.Assault ->  turn, gtMaker Phase.End
        | Some Phase.End 
        | None -> otherPlayerTurn (turn,round)

    let rec (|Active|_|) r gameState = 
        match r with 
        | ActiveWhen(logic, innerRule) -> match compareLogic gameState logic, gameState with 
                                          | TBool true, Active innerRule _ -> Some r | _ -> None
        | UserActivated (userActivated) ->  match gameState with Active userActivated _ -> Some r | _ -> None
        | Function _ -> Some r
        | Description _ -> Some r
        | Overwritten (overwrite,_) -> match gameState with Active overwrite _ -> Some r | _ -> None
        | Nested(r, rules) -> 
            let matchFst = match gameState with Active r _ -> Some r | _ -> None
            match matchFst with 
            | Some _ -> matchFst
            | None -> List.tryPick(fun r -> match gameState with Active r _ -> Some r | _ -> None) rules
    and (|ApplyTo|_|) gameState (x:Apply<'a>) =  
        match x with 
        | Applied a -> Some a
        | NotApplied s ->
            gameState.Rules.TryFind(GameRuleImpl.Applications(Map.empty<string,TRule>).ToString())
            |> Option.bind (evalR gameState >> function 
                | TApplicationMap(map) -> map |> Map.tryFind s |> Option.bind (TRule.get) 
                | _ -> None)
    and (|ApplicationTrule|_|) gameState x =  
        match x with 
        | Applied a -> Some a
        | NotApplied s ->
            gameState.Rules.TryFind(GameRuleImpl.Applications(Map.empty<string,TRule>).ToString())
            |> Option.bind (evalR gameState >> function 
                | TApplicationMap(map) -> map |> Map.tryFind s
                | _ -> None)
    and (|ApplyToLogical|) gameState logicalRule =
        let rec applyPrimary primary = 
            match primary with
            | Equation (pe,op,pe2) -> Equation(applyPrimary pe,op,applyPrimary pe2)
            | Apply(ApplicationTrule gameState trule) -> Apply(Applied trule)
            | primary -> primary
        match logicalRule with
        | Logical(ApplyToLogical gameState logic, op, ApplyToLogical gameState logic2) -> Logical(logic,op,logic2)
        | Not(ApplyToLogical gameState logic) -> Not logic
        | Literal primary -> Literal (applyPrimary primary)
        | logical -> logical 

    and eval gameState = function 
        | GameStateRule impl ->  evalG gameState impl 
        | ModelRule (impl, _) -> evalM impl 
        | UnitRule(impl, _) ->  evalU  impl 
        | Sequence(impls) ->  List.map (eval gameState) impls |> TList
    and evalR gameState = function
        | Function (app) -> eval gameState app
        | UserActivated _ -> TUnit
        | ActiveWhen (logic,innerRule) -> if compareLogic gameState logic = TBool true  then evalR gameState innerRule
                                          else TUnit 
        | Description _ -> TUnit
        | Overwritten (r1,_) -> evalR gameState r1
        | Nested (head,tail) -> List.map (evalR gameState) (head::tail) |> TList
    and evalG gameState = function
        | Noop       -> TUnit
        | EndPhase   -> 
            match Map.tryFind (GameRound(Begin).ToString()) gameState.Rules,Map.tryFind (PlayerTurn(Top).ToString()) gameState.Rules with
            | Some(Function(GameStateRule(GameRound(round)))), Some(Function(GameStateRule(PlayerTurn(turn)))) -> calculateNextPhase (turn,round) |> snd |> TRound
            | _ -> TUnit
        | EndTurn    -> 
            match Map.tryFind (GameRound(Begin).ToString()) gameState.Rules,Map.tryFind (PlayerTurn(Top).ToString()) gameState.Rules with
            | Some(Function(GameStateRule(GameRound(round)))), Some(Function(GameStateRule(PlayerTurn(turn)))) -> otherPlayerTurn (turn,round) |> fst |> TPlayerTurn
            | _ -> TUnit
        | EndGame    -> TUnit
        | Board (boardDimensions)-> TBoardDimensions (boardDimensions)
        | SortedWeaponProfiles (_)-> TUnit
        | PlayerTurn (playerTurn)-> TPlayerTurn (playerTurn)
        | GameRound (round) -> TRound (round)
        | DeactivateUntil _ -> TUnit
        | Revert _ -> TUnit
        | Remove _ -> TUnit
        | Activate _ -> TUnit
        | AddOrReplace _-> TUnit
        | Overwrite _ -> TUnit
        | Repeat _ -> TUnit
        | CollectUserActivated -> TUnit
        | Applications(map) -> TApplicationMap(map)
        | Supply _ -> TUnit
        | Application (_,t) -> t
        | Unapply _ -> TUnit
    and evalU  = function 
        | Move (inches)-> TMeasurement (inches)
        | DeploymentState (deploymentType)-> TDeploymentState (deploymentType)
        | Deploy -> TUnit
        | WoundPool ( profiles, mid) -> TList [ (profiles |> List.collect (fun (count,profile) -> [TCount count; TWeaponProfile profile]) |> TList);TSpecialTarget(mid)]
        | SortedWoundPool( profiles, mid) -> TList [ (profiles |> List.collect (fun (count,profile) -> [TCount count; TWeaponProfile profile]) |> TList);TSpecialTarget(mid)]
        | Save (profile, mid) -> TList [TWeaponProfile(profile); TSpecialTarget(mid)]
    and evalM = function
        | WeaponSkill     cv 
        | BallisticSkill  cv 
        | Strength        cv 
        | Toughness       cv 
        | Wounds          cv 
        | Initiative      cv 
        | Attacks         cv 
        | Leadership      cv 
        | InvSaves        cv 
        | CoverSaves      cv 
        | Saves           cv -> TCharacteristicValue cv
        | ModelPosition   pos -> TPosition pos
        | SetCharacteristic rule -> TRule(rule)
        | Unsaved weaponProfile -> TWeaponProfile weaponProfile
        | RemoveOnZeroCharacteristic -> TUnit
        | ArmourPenetration armourPen -> TArmourPen armourPen
        | Melee (i,Applied uid) -> TList [TCharacteristicValue(CharacteristicValue(i));TTarget(uid)]
        | Melee (_,NotApplied _) -> TUnit
        | MeleeHit (i,Applied uid) -> TList [TCharacteristicValue(CharacteristicValue(i));TTarget(uid)]
        | MeleeHit (_,NotApplied _) -> TUnit
    and tryFindInRuleList tryFindKeyPredicate = Option.bind (Map.tryFindKey tryFindKeyPredicate)
    and compareLogic (gameState:GameState) logic : TRule = 
        let rec (|EvalLit|_|) gameState primaryExpression = 
            match primaryExpression with 
            | Equation(EvalLit gameState x,op,EvalLit gameState y) -> 
                TRule.Compare(x,op,y)
                |> TBool
                |> Some
            | Apply (ApplicationTrule gameState x) -> Some x
            | Evaluation ruleApplication -> eval gameState ruleApplication |> Some
            | Equation _ -> None
            | Apply _ -> None
        let exists ruleApplication k _ = 
            match ruleApplication with 
            | GameStateRule impl ->  k = impl.ToString() 
            | ModelRule (impl, _)  ->  k = impl.ToString() 
            | UnitRule(impl, _)  ->  k = impl.ToString() 
            | Sequence(impl::_)  -> k = impl.ToString() 
            | _ -> false
        match logic with 
        | Exists ruleApplication -> tryFindRuleList gameState ruleApplication |> tryFindInRuleList (exists ruleApplication) |> Option.map (fun _ -> TBool true) |> defaultArg <| TBool false
//        | Matches ruleApplication -> tryFindRuleList gameState ruleApplication |> tryFindInRuleList (exists ruleApplication &&>> matches ruleApplication) |> Option.map (fun _ -> rule)
        | Not expr -> compareLogic gameState expr |> TRule.not'
        | Logical (expr,op,expr2) ->  
            match op, compareLogic gameState expr, compareLogic gameState expr2 with
            | And, TBool true, TBool true ->  TBool true
            | And, _, _ -> TBool false
            | Or, TBool true, _ | Or, _,  TBool true ->  TBool true
            | Or, TBool false, TBool false -> TBool false
            | _ -> TBool false
        | Literal(EvalLit gameState result) -> result
        | Literal(_) -> failwith "Could not compare logic" 

//        | Literal()
//        | Literal(Applied x, op, Applied y)
//        | Literal(ApplicationTrule gameState x, op, ApplicationTrule gameState y)
//        | Literal(ApplicationTrule gameState x, op, Applied y)
//        | Literal(Applied x, op,ApplicationTrule gameState y)  -> if (x,op,y) |> TRule.Compare then Some rule else None
//        | Literal(NotApplied _,_,_)
//        | Literal(_,_,NotApplied _) -> None
    let optionalAndActiveRules gs (_,r) = 
        match r,gs with 
        | Optional _, Active r ra -> Some ra
        | _ -> None
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


    let deploy uId gameState  = 
        let foundUnit = tryFindUnit gameState uId
        match foundUnit with
        | Some u -> 
            let pa positionAsker =
                let newRule = 
                    AddOrReplace(UnitList(uId), Function(UnitRule(DeploymentState(Deployed), uId)))
                    |> GameStateRule 
                    |> Function 
                    |> Rule.afterRunRemove GameStateList
                    
                newRule :: 
                    forAllModels (fun (m:Model) -> 
                    (ModelList(m.Id),(ModelPosition(gameState |> positionAsker),m.Id)
                    |> ModelRule
                    |> Function)
                    |> AddOrReplace 
                    |> GameStateRule
                    |> Function
                    |> Rule.afterRunRemove GameStateList) u
                
            pa
        | None -> failwith "Couldn't find unit" 

    let move uId gameState maxMove  = 
        let createMove moveAsker =
            let newPosition position = 

                let ps = pixelsInCircle maxMove position |> Seq.toArray
                let rec newPick ps = 
                    let (p:Position<px>) = moveAsker ps
                    if Seq.contains p ps then p
                    else newPick ps
                newPick ps

            let foundUnit = tryFindUnit gameState uId
            match foundUnit with
            | Some u -> 
                forAllModels (fun (m:Model) -> 
                    let pos =  
                        m.Rules
                        |> Map.pick (fun _ r -> match gameState with 
                                                    | Active r (Function(ModelRule(ModelPosition(pos),_))) -> Some pos
                                                    | _ -> None)
                    (ModelList(m.Id),(ModelPosition(pos |> newPosition ),m.Id) |> ModelRule |> Function)
                    |> AddOrReplace |> GameStateRule |> Function|> Rule.afterRunRemove GameStateList) u
            | None -> failwith "Couldn't find unit"
        createMove


    let advancePhase gs = 
        let advance turn round =
            calculateNextPhase (turn,round)
            |> (fun (turn,round) -> [PlayerTurn(turn); GameRound(round)])
            |> List.map (fun r -> GameStateRule(AddOrReplace(GameStateList,Function(GameStateRule(r))))  |> Function |> afterRunRemove GameStateList)
        match Map.tryFind (GameRound(Begin).ToString()) gs.Rules,Map.tryFind (PlayerTurn(Top).ToString()) gs.Rules with
        | Some(Function(GameStateRule(GameRound(round)))), Some(Function(GameStateRule(PlayerTurn(turn)))) -> advance turn round
        | _ -> failwith <| sprintf "Couldn't find game round or playerturn %A" gs.Rules

    let repeat times name rule = 
        if times > 0 then
            [GameStateRule(AddOrReplace(GameStateList,(rule |> Rule.afterRunRepeat (times - 1) name)))
                    |> Function
                    |> Rule.afterRunRemove GameStateList]
        else
            []

    let avgToughness (u:Unit) gameState = 
        u.UnitModels
        |> Map.choose (fun _ um -> um.Rules |> Map.tryPick (fun _ r -> match gameState with | Active r (Function(ModelRule(Toughness(CharacteristicValue(ra)),_))) -> Some ra | _ -> None)) 
        |> Map.values
        |> Seq.maxmode 0
    let avgWS (u:Unit) gameState = 
        u.UnitModels
        |> Map.choose (fun _ um -> um.Rules |> Map.tryPick (fun _ r -> match gameState with | Active r (Function(ModelRule(WeaponSkill(CharacteristicValue(ra)),_))) -> Some ra | _ -> None)) 
        |> Map.values
        |> Seq.maxmode 0
    let modelWS (m:Model) gameState = 
        m.Rules
        |> Map.tryPick(fun _ r -> match gameState with | Active r (Function(ModelRule(WeaponSkill(CharacteristicValue(ra)),_))) -> Some ra | _ -> None)
        |> defaultArg <| 0
    let modelStrength (m:Model) gameState = 
        m.Rules
        |> Map.tryPick(fun _ r -> match gameState with | Active r (Function(ModelRule(Strength(CharacteristicValue(ra)),_))) -> Some ra | _ -> None)
        |> defaultArg <| 0
    let rollDice rule dPlus = 
            let dieRollNeeded = dPlus |> TDiceRoll |> Applied |> Apply
            let notApplied = dieRollNeeded.ToString() |> NotApplied |> Apply
            let gt = Equation(notApplied, Gt, dieRollNeeded)
            let eq = Equation(notApplied, Eq, dieRollNeeded)
            let logic = Logical(Literal(gt), Or, Literal(eq))
            Rule.activeWhen logic rule
    
    let toHit m u gameState = hitAssaultTable (modelWS m gameState) (avgWS u gameState)    
    let toWound m u gameState = woundTable (modelStrength m gameState) (avgToughness u gameState)

    //let assault = .... //May have to change this to adding a Targetted rule to target, which adds a Melee to model 
    let melee attacks target mId (gameState:GameState) = 
        let foundModel = tryFindModel gameState mId
        let foundTarget = tryFindUnit gameState target
        match foundModel,foundTarget with
        | Some m, Some u -> 
            let rule = 
                Function(ModelRule(MeleeHit(1,Applied target),mId))
                |> Rule.after (fun _ -> Unapply "TDiceRoll")
            let meleehits = toHit m u gameState |> rollDice rule
            Function(Sequence[GameStateRule(Supply "TDiceRoll");GameStateRule(AddOrReplace(ModelList mId, meleehits))])
            |> Rule.many (ModelList mId) attacks
            
        | _ -> failwith <| sprintf "Not found %A or %A" target mId
    let meleeHits hits uId mId gameState =
        let foundModel = tryFindModel gameState mId
        let foundTarget = tryFindUnit gameState uId
        match foundModel,foundTarget with
        | Some m, Some u -> 
            let newWounds = [1,[]] //List of hits,weapon profiles
            
            let rule =  
                u.Rules
                |> Map.tryPick (fun _ r -> match gameState with 
                                            | Active r (Function(UnitRule(WoundPool(wounds,mId),uId))) -> Some(UnitRule(WoundPool((List.append wounds newWounds),mId),uId))
                                            | _ -> None)
                |> defaultArg <|  UnitRule(WoundPool(newWounds,mId),uId)
                |> Function
            toWound m u gameState 
            |> rollDice rule 
            |> Rule.many (ModelList mId) hits            
        | _ -> failwith <| sprintf "Not found %A or %A" uId mId
//        
//    let woundPool profiles attackingModelId uId =
//        let profiles' = 
//            profiles 
//            |> List.groupBy (snd) 
//            |> List.map (fun (profile,woundProfiles) -> woundProfiles |> Seq.sumBy fst, profile)
//        let dosortedIndexes = 
//            (SortedWeaponProfiles([]).ToString(), (profiles' |> List.collect(fun (count,profile) -> [TCount count;TWeaponProfile profile]) |> TList))
//            |> Supply 
//            |> GameStateRule 
//            |> Function 
//            |> Rule.afterRunRemove GameStateList
//        let newPool = 
//            UnitRule(SortedWoundPool(profiles', attackingModelId),uId)
//            |> Function  
//            |> Rule.afterIfRemove (Matches(GameStateRule(SortedWeaponProfiles([]))))
//
//        [
//            GameStateRule(AddOrReplace(GameStateList, dosortedIndexes)) |> Function |> afterRunRemove GameStateList
//            GameStateRule(AddOrReplace(UnitList uId, newPool)) |> Function |> afterRunRemove GameStateList
//        ]
//
//    let save (um:Model) gameState = 
//        um.Rules 
//        |> Map.choose(fun _ r -> match gameState with
//                                 | Active r (Function(ModelRule(Saves(CharacteristicValue(ra)),mId)) )
//                                 | Active r (Function(ModelRule(InvSaves(CharacteristicValue(ra)),mId)))
//                                 | Active r (Function(ModelRule(CoverSaves(CharacteristicValue(ra)),mId)))  -> Some ra 
//                                 | _ -> None)
//        |> Map.values
//        |> Seq.sort 
//        |> Seq.tryNth 0
//        |> defaultArg <| 7
//        |> DiceRoll
//    let collectPenetrations  = List.tryPick(function | MR(ArmourPenetration(ra)) -> ra | _ -> None)
//
//    let sortedWoundPool (profiles: list<int * WeaponProfile>) target mId gameState = 
//        let r = 
//            gameState.Rules 
//            |> Map.tryPick(fun _ r -> match gameState with 
//                                      | Active r (Function(GameStateRule(SortedWeaponProfiles(ra)))) -> Some ra 
//                                      | _ -> None)
//        match r with
//        | Some sortedProfiles  -> 
//            let sortedProfiles' = List.zip profiles sortedProfiles |> List.sortBy snd 
//            match sortedProfiles' with 
//            | [] -> []
//            | ((times,profile),sortNum)::rest ->
//                let newProfile,newSorted = List.unzip rest |> fun (pr,srt) -> 
//                                                                if times > 0 then
//                                                                    (times-1,profile)::pr,sortNum::srt
//                                                                else
//                                                                    pr,srt
//                let newRule = Function(UnitRule(Save(profile,mId),target)) |> Rule.afterRunRemove (UnitList target)
//                [
//                    GameStateRule(AddOrReplace(GameStateList, (Function(GameStateRule(SortedWeaponProfiles(newSorted)))))) |> Function |> Rule.afterRunRemove GameStateList
//                    GameStateRule(AddOrReplace(UnitList target, (Function(UnitRule(SortedWoundPool(newProfile, mId),target))))) |> Function |> Rule.afterRunRemove (UnitList target)
//                    GameStateRule(AddOrReplace(UnitList target, newRule)) |> Function |> Rule.afterRunRemove (UnitList target)
//                ]
//        | x -> failwith <| sprintf "Not found - weapon profile sort %A" x
//    let pickClosest mId uId gameState = //TODO change impl to something. Probably have to roll position into GameState
//        let foundUnit = tryFindUnit gameState uId 
//        let foundModel = tryFindModel gameState mId
//        let foundAssaulters =  Option.bind (fun m -> tryFindUnitByModel gameState m) foundModel
//        match (foundAssaulters,foundUnit) with
//        | Some _,Some u ->
//            u.UnitModels |> Map.values |> Seq.head |> Some
//        | _ -> None
//    let saveWound profile mId uId gameState = 
//        let picked = pickClosest  mId uId gameState
//        match picked with 
//        | Some m -> 
//            let newRule = ModelRule(Unsaved(profile),m.Id) |> Function
//            multipleFromDiceRoll newRule (save m) 1 gameState
//        | _ -> failwith <| sprintf "Not found model in %A" uId
//
//    let unsavedWound _ mId gameState =
//        let foundModel = tryFindModel gameState mId
//        let wounds = foundModel |> Option.bind (fun m ->  
//                                            m.Rules
//                                            |> Map.values
//                                            |> Seq.tryPick(fun r -> match gameState with | Active r (Function(ModelRule(Wounds(CharacteristicValue(w)),_))) -> Some w 
//                                                                                         | _ -> None))
//        let foundUnit =  Option.bind (fun m -> tryFindUnitByModel gameState m) foundModel
//        let foundPlayer = Option.bind (tryFindPlayer gameState) foundUnit
//        match wounds, foundUnit, foundPlayer,foundModel with
//        | Some wounds, Some _, Some _, Some _ ->
//            let woundRule  = Function(ModelRule(Wounds(wounds - 1 |> CharacteristicValue),mId))
//            let newRule = (SetCharacteristic(woundRule),mId) |> ModelRule 
//            [GameStateRule(AddOrReplace(ModelList mId, (newRule |> Function |> Rule.afterRunRemove (ModelList mId)))) |> Function |> Rule.afterRunRemove GameStateList]
//        | _ -> failwith <| sprintf "Not found %A "  mId
//    let removeIfZeroCharacteristic  mId gameState = 
//        let foundModel = tryFindModel gameState mId
//        let foundUnit =  Option.bind (fun m -> tryFindUnitByModel gameState m) foundModel
//        let foundPlayer = Option.bind (tryFindPlayer gameState) foundUnit
//        match foundUnit, foundPlayer,foundModel with
//        | Some u, Some p, Some m ->
//
//            replaceUnitModelsInGameState gameState p u m (defnot None)
//        | _ -> failwith <| sprintf "Not found %A "  mId
    let supplySortedWeapons profiles profileMaker = 
        let newRule = profileMaker profiles |> SortedWeaponProfiles |> GameStateRule 
        [GameStateRule(AddOrReplace(GameStateList,(newRule |> Function))) |> Function |> afterRunRemove GameStateList]

    let availableRules predicate gs = 
        let captureRules =  Map.toSeq >> Seq.choose predicate >> Seq.toList

        let gameRules = captureRules gs.Rules
        
        let units = 
            gs.Players
            |> List.collect (fun p -> p.Units |> Map.toList) 

        let unitRules = 
            units 
            |> List.collect (fun (_,item) ->  captureRules item.Rules)

        let modelRules = 
            units
            |> List.collect(fun (_,item) -> item.UnitModels 
                                            |> Map.toList 
                                            |> List.collect (fun(_,item) -> captureRules item.Rules))

        modelRules @ unitRules @ gameRules

    let collect gameState = 
        let predicate = optionalAndActiveRules gameState
        let rules  = gameState |> availableRules predicate 
        let rulesApplication raPicker = 
            rules |> List.tryItem (raPicker rules) 
            |> Option.map (fun rule ->
                let newRule = Activate(rule) |> GameStateRule |> Function |> Rule.afterRunRemove GameStateList
                [GameStateRule(AddOrReplace(GameStateList,newRule)) |> Function |> afterRunRemove GameStateList]
                )
            |> defaultArg <| []
        rulesApplication
    let supply = 
        let application applicationSupplier =
            applicationSupplier () |> GameStateRule  |> Function |> afterRunRemove GameStateList |> List.singleton
        application
    let unapply key gameState = 
        gameState.Rules
        |> Map.tryPick(fun _ r -> match gameState with | Active r (Function(GameStateRule(Applications(map)))) -> Some map | _ -> None)
        |> Option.map(fun map -> [AddOrReplace(GameStateList, Function(GameStateRule(Applications(map |> Map.remove key)))) |> GameStateRule |> Function |> Rule.afterRunRemove GameStateList])
        |> defaultArg <| []
    let apply key trule gameState = 
        gameState.Rules
        |> Map.tryPick(fun _ r -> match gameState with | Active r (Function(GameStateRule(Applications(map)))) -> Some map | _ -> None)
        |> Option.map(fun map -> [AddOrReplace(GameStateList, Function(GameStateRule(Applications(map |> Map.add key trule)))) |> GameStateRule |> Function |> Rule.afterRunRemove GameStateList])
        |> defaultArg <| []
    let runRules rules gameState = 
        //Things that trigger recalculation Could change this to infinite doublly linked list. 
        // current @ rest @ begin  
        // Modify rest, prepend to reset, append to rest
        let rec runRule gameState rule =
            match gameState,rule with 
            | GameStateResult gameState', ActiveWhen(logic, innerRule) -> 
                if compareLogic gameState' logic = TBool true then runRule gameState innerRule 
                else gameState
            | GameStateResult _, UserActivated (_) -> gameState
            | GameStateResult _, Function app -> runApplication gameState app
            | GameStateResult _, Description _ -> gameState
            | GameStateResult _, Overwritten (overwrite,_) -> runRule gameState overwrite 
            | GameStateResult _, Nested(r, rules) -> List.fold runRule gameState (r::rules)
            | AskResult a, rule -> a.Map(fun a -> runRule a rule) |> AskResult 
        and runApplication gameState ruleApplication =
            match (gameState,ruleApplication) with
            | GameStateResult _, Sequence(rules) -> List.fold runApplication gameState rules
            | GameStateResult gameState, GameStateRule(impl) -> runGameImpl gameState impl
            | GameStateResult gameState, ModelRule(impl,mId) -> runModelImpl mId gameState impl 
            | GameStateResult gameState, UnitRule(impl,uId) -> runUnitImpl uId gameState impl 
            | AskResult a, ruleApplication -> a.Map(fun a -> runApplication a ruleApplication) |> AskResult 
        and runGameImpl gameState = function 
            | AddOrReplace(GameStateList, newRule) -> tryReplaceRuleOnGameState def newRule gameState |> GameStateResult
            | AddOrReplace(ModelList mId, newRule) -> tryReplaceRuleOnModel def newRule mId gameState |> GameStateResult
            | AddOrReplace(UnitList uId, newRule) -> tryReplaceRuleOnUnit def newRule uId gameState |> GameStateResult
            | Overwrite(GameStateList, newRule) -> tryReplaceRuleOnGameState overriteOrNone newRule gameState |> GameStateResult
            | Overwrite(ModelList mId, newRule) -> tryReplaceRuleOnModel overriteOrNone newRule mId gameState |> GameStateResult
            | Overwrite(UnitList uId, newRule) -> tryReplaceRuleOnUnit overriteOrNone newRule uId gameState |> GameStateResult
            | Remove(GameStateList, oldRule) -> tryReplaceRuleOnGameState defnot oldRule gameState |> GameStateResult
            | Remove(ModelList mId, oldRule) ->  tryReplaceRuleOnModel defnot oldRule mId gameState |> GameStateResult
            | Remove(UnitList uId, oldRule) -> tryReplaceRuleOnUnit defnot oldRule uId gameState |> GameStateResult
            | Revert(GameStateList, rule) -> tryReplaceRuleOnGameState Rule.unoverwriteOrNone rule gameState |> GameStateResult
            | Revert(ModelList mId, rule) -> tryReplaceRuleOnModel Rule.unoverwriteOrNone rule mId gameState |> GameStateResult
            | Revert(UnitList uId, rule) -> tryReplaceRuleOnUnit Rule.unoverwriteOrNone rule uId gameState |> GameStateResult
            | Activate(UserActivated (rule)) -> runRule (GameStateResult gameState) rule 
            | Activate(_) -> failwith "Dont know how to run this."
            | DeactivateUntil(ApplyToLogical gameState activateWhen, UnitList uId, rule) -> tryReplaceRuleOnUnit (Rule.activeWhen activateWhen >> overriteOrNone) (Function(GameStateRule(Revert(UnitList uId, rule)))) uId gameState |> GameStateResult
            | DeactivateUntil(ApplyToLogical gameState activateWhen, ModelList mId, rule) -> tryReplaceRuleOnModel (Rule.activeWhen activateWhen >> overriteOrNone) (Function(GameStateRule(Revert(ModelList mId, rule)))) mId gameState |> GameStateResult
            | DeactivateUntil(ApplyToLogical gameState activateWhen, GameStateList, rule)-> tryReplaceRuleOnGameState (Rule.activeWhen activateWhen >> overriteOrNone) (Function(GameStateRule(Revert(GameStateList, rule)))) gameState  |> GameStateResult
            | Repeat(times,name,rule) -> repeat times name rule |> List.fold runRule (GameStateResult gameState) 
            | EndPhase -> advancePhase gameState |> List.fold runRule (GameStateResult gameState) 
            | EndGame -> runGameImpl gameState (Remove(GameStateList,Function(GameStateRule(PlayerTurn(Bottom)))))
//            | SupplySortedWeaponProfiles(profiles) -> supplySortedWeapons profiles >> List.fold runRule (GameStateResult gameState) |> Asker |> SortedWoundPoolAsker |> AskResult
//            | RollDice -> rollDice 
            | CollectUserActivated -> collect gameState >> List.fold runRule (GameStateResult gameState)  |> Asker  |> PerformAsker |> AskResult
            | Supply "TDiceRoll" -> supply >> List.fold runRule (GameStateResult gameState)  |> Asker |> DiceRollAsker |> AskResult
            | Unapply s -> unapply s gameState |> List.fold runRule (GameStateResult gameState)
            | Application (s,trule) -> apply s trule gameState |> List.fold runRule (GameStateResult gameState)
            | EndTurn // Maybe Split EndPhase and End Turn
            | SortedWeaponProfiles(_)  
            | GameRound(_)
            | PlayerTurn(_)
            | Board(_) 
            | Applications(_)
            | Noop            -> gameState |> GameStateResult
        and runModelImpl mId gameState = function
            | SetCharacteristic(newRule) -> tryReplaceRuleOnModel Rule.overriteOrNone newRule mId gameState  |> GameStateResult
            | Melee(attacks,(ApplyTo gameState target)) -> melee attacks target mId gameState |> List.fold runRule (GameStateResult gameState) 
            | MeleeHit(hits,(ApplyTo gameState target)) -> meleeHits hits target mId gameState  |> List.fold runRule (GameStateResult gameState) 
//            | Unsaved(profile) -> unsavedWound profile mId gameState |> List.fold runRule (GameStateResult gameState) 
//            | RemoveOnZeroCharacteristic -> removeIfZeroCharacteristic mId gameState |> GameStateResult
            | WeaponSkill   (_)
            | BallisticSkill(_)
            | ModelPosition(_)
            | Strength      (_)
            | Toughness     (_)
            | Wounds        (_)
            | Initiative    (_)
            | Attacks       (_)
            | Leadership    (_)
            | InvSaves      (_)
            | Saves         (_)
            | CoverSaves         (_)
            | ArmourPenetration(_)  -> gameState |> GameStateResult
        and runUnitImpl uId gameState = function
            | Deploy -> deploy uId gameState >> List.fold runRule (GameStateResult gameState)  |> Asker  |> PositionAsker |> AskResult              
            | Move maxMove -> move uId gameState maxMove >> List.fold runRule (GameStateResult gameState)  |> Asker  |> MoveAsker |> AskResult
            | WoundPool(profiles,mId) -> printfn "Wound pool!"; (GameStateResult gameState)   
//            | SortedWoundPool(profiles,mId) -> sortedWoundPool profiles mId uId gameState |> List.fold runRule (GameStateResult gameState)  
//            | Save(profile,mId) -> saveWound profile mId uId gameState |> List.fold runRule (GameStateResult gameState) 
            | DeploymentState(_) -> gameState |> GameStateResult 
        List.fold runRule (GameStateResult gameState) rules