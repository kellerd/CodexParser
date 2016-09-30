namespace GameImpl 
module GameState = 
    open FSharpx.Collections
    open Domain.Board
    open Microsoft.FSharp.Collections
    open Domain

    let tryFindUnit gameState unitId = 
        gameState.Players |> List.tryPick (fun p -> p.Units |> Map.tryFind unitId)
    let tryFindUnitByModel gameState (model:Model) = 
        gameState.Players |> List.choose(fun p -> p.Units |> Map.tryPick (fun _ u  -> u.UnitModels |> Map.tryFind model.Id |> Option.bind(fun _ -> Some u))) |> List.tryHead
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

    let replace xs x y = 
        let pred z = x = z
        y :: (removeFirst pred xs)
    
    let def x _ = Some x
    let defnot x _ = None
    let replaceUnitModels u (m:Model) nm = { u with UnitModels = Map.updateWithOrRemove nm m.Id u.UnitModels } 
    let replacePlayerUnits p u nu = { p with Units = Map.updateWithOrRemove nu u.Id p.Units }
    let replaceGameStatePlayers s p np = { s with Players = replace s.Players p np }
    let rec tryFindRule ruleApplication gameState =
        match ruleApplication with
        | UnitRule(rule,uId) -> 
            let name = rule.ToString()
            tryFindUnit gameState uId |> Option.bind(fun u -> u.Rules |> Map.tryFind name)
        | ModelRule(rule,mId) ->
            let name = rule.ToString()
            tryFindModel gameState mId |> Option.bind(fun m -> m.Model.Rules |> Map.tryFind name)
        | GameStateRule(rule) ->
            let name = rule.ToString()
            gameState.Rules |> Map.tryFind name
        | Sequence(rule::_) ->
            tryFindRule rule gameState 
        | Sequence([]) -> None
    let replaceUnitModelsInPlayer p u m nm = def (replaceUnitModels u m nm) |> replacePlayerUnits p u
    let replaceUnitModelsInGameState s p u m nm = replaceUnitModelsInPlayer p u m nm |> replaceGameStatePlayers s p 
    let updateModelInBoard (model:ModelInfo option) (newModel:ModelInfo option -> ModelInfo option) gameState =
        match model with 
        | Some model -> { gameState with Board = { gameState.Board with Models = Map.updateWithOrRemove newModel model.Model.Id gameState.Board.Models } }) model
        | None -> gameState
    let updatePlayerInGameState unit newUnit gameState = 
        let foundPlayer = Option.bind (tryFindPlayer gameState) unit
        let newPlayer = Option.map2 (fun p unit -> replacePlayerUnits p unit newUnit) foundPlayer unit
        match gameState, foundPlayer, newPlayer with
        | (gs, Some p, Some np) -> replaceGameStatePlayers gs p np
        | (gs, _, _) -> gs   
    let updateUnitInGameState (model:Model option) newModel gameState = 
        let foundUnit = Option.bind (tryFindUnitByModel gameState) model
        let newUnit = Option.map2 (fun p model -> replaceUnitModels p model newModel) foundUnit model
        let nmFunc = (Option.bind(fun mi -> Some mi.Model |> newModel |> Option.map(fun m -> {mi with Model = m})))
        let modelInfo = GameState.
        match newUnit with
        | (Some nu) -> updatePlayerInGameState foundUnit (def nu) gameState |> updateModelInBoard (model) nmFunc
        | (_) -> gameState
    let replaceRuleOnGameState  replace (gameState:GameState)  = 
        let newGameState = { gameState with Rules = gameState.Rules |> replace }
        newGameState
    let tryReplaceRuleOnGameState mapf rule gameState = 
        let name = makeRule rule |> fst
        replaceRuleOnGameState (Map.updateWithOrRemove (mapf rule) name) gameState
    let replaceRuleOnUnit  (unit : Unit) replace gameState = 
        let newUnit = { unit with Rules = unit.Rules |> replace }
        updatePlayerInGameState (Some unit) (def newUnit) gameState
    let tryReplaceRuleOnUnit mapf rule uid gameState = 
        let name = makeRule rule |> fst
        tryFindUnit gameState uid
        |> Option.map (fun u -> replaceRuleOnUnit u (Map.updateWithOrRemove (mapf rule) name) gameState)
        //|> defaultArg <| gameState
        |> Option.get 
    let replaceRuleOnModel  (model : Model) replace gameState = 
        let newmodel = { model with Rules = model.Rules |> replace }
        updateUnitInGameState (Some model) (def newmodel) gameState
    let tryReplaceRuleOnModel mapf rule mId gameState = 
        let name = makeRule rule |> fst
        tryFindModel gameState mId
        |> Option.map (fun m -> replaceRuleOnModel m.Model (Map.updateWithOrRemove (mapf rule) name) gameState)
        |> Option.get 
    let forAllModels f newUnit = 
        [ for m in newUnit.UnitModels do
                yield f m.Value ]