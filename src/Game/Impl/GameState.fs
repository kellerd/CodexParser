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
    let defnot x _ = x
    let replaceUnitModels u (m:Model) nm = { u with UnitModels = Map.updateWith nm m.Id u.UnitModels } 
    let replacePlayerUnits p u nu = { p with Units = Map.updateWith nu u.Id p.Units }
    let replaceGameStatePlayers s p np = { s with Players = replace s.Players p np }
 
    let replaceUnitModelsInPlayer p u m nm = def (replaceUnitModels u m nm) |> replacePlayerUnits p u
    let replaceUnitModelsInGameState s p u m nm = replaceUnitModelsInPlayer p u m nm |> replaceGameStatePlayers s p 

    let updatePlayerInGameState unit newUnit gameState = 
        let foundPlayer = tryFindPlayer gameState unit
        let newPlayer = foundPlayer |> Option.map (fun p -> replacePlayerUnits p unit newUnit)
        match gameState, foundPlayer, newPlayer with
        | (gs, Some p, Some np) -> replaceGameStatePlayers gs p np
        | (gs, _, _) -> gs   
    let updateUnitInGameState (model:Model) newmodel gameState = 
        let foundUnit = tryFindUnitByModel gameState model
        let newUnit = foundUnit |> Option.map (fun p -> replaceUnitModels p model newmodel)
        match gameState, foundUnit, newUnit with
        | (gs, Some u, Some nu) -> updatePlayerInGameState u (def nu) gs
        | (gs, _, _) -> gs   
    let replaceRuleOnGameState  replace (gameState:GameState)  = 
        let newGameState = { gameState with Rules = gameState.Rules |> replace }
        newGameState
    let tryReplaceRuleOnGameState updateType rule gameState = 
        let name = makeRule rule |> fst
        replaceRuleOnGameState (Map.updateWith (updateType rule) name) gameState
    let replaceRuleOnUnit  (unit : Unit) replace gameState = 
        let newUnit = { unit with Rules = unit.Rules |> replace }
        updatePlayerInGameState unit (def newUnit) gameState
    let tryReplaceRuleOnUnit updateType rule uid gameState = 
        let name = makeRule rule |> fst
        tryFindUnit gameState uid
        |> Option.map (fun u -> replaceRuleOnUnit u (Map.updateWith (updateType rule) name) gameState)
        |> defaultArg <| gameState
    let replaceRuleOnModel  (model : Model) replace gameState = 
        let newmodel = { model with Rules = model.Rules |> replace }
        updateUnitInGameState model (def newmodel) gameState
    let tryReplaceRuleOnModel updateType rule mId gameState = 
        let name = makeRule rule |> fst
        tryFindModel gameState mId
        |> Option.map (fun m -> replaceRuleOnModel m.Model (Map.updateWith (updateType rule) name) gameState)
        |> defaultArg <| gameState
    let forAllModels f newUnit gameState = 
        [ for m in newUnit.UnitModels do
                yield f m.Value ]
        |> List.fold (fun acc m -> match Map.tryFind m.Model.Id acc with
                                    | Some _ -> Map.updateWith (def m) m.Model.Id acc
                                    | None -> Map.add m.Model.Id m acc) gameState.Board.Models