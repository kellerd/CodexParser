namespace GameImpl 
module GameState = 
    open Domain.WarhammerDomain
    open Microsoft.FSharp.Collections
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
    
    
    let replaceUnitModels u (m:Model) nm = { u with UnitModels = Map.replace id nm m.Id u.UnitModels }
    let replacePlayerUnits p u nu = { p with Units = Map.replace id nu u.Id p.Units }
    let replaceGameStatePlayers s p np = { s with Players = replace s.Players p np }
 
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
        | (gs, Some u, Some nu) -> updatePlayerInGameState u nu gs
        | (gs, _, _) -> gs   
    let replaceRuleOnGameState  replace (gameState:GameState)  = 
        let newGameState = { gameState with Rules = gameState.Rules |> replace }
        newGameState
    let tryReplaceRuleOnGameState name mapf gameState = 
        gameState.Rules 
            |> Map.tryFind name
            |> Option.map (fun r -> r |> Map.replace mapf <| name
                                      |> replaceRuleOnGameState <| gameState)
                    |> defaultArg <| gameState
    let replaceRuleOnUnit gameState (unit : Unit) replace = 
        let newUnit = { unit with Rules = unit.Rules |> replace }
        updatePlayerInGameState unit newUnit gameState
    let tryReplaceRuleOnUnit name mapf uid gameState = 
        tryFindUnit gameState uid 
                    |> Option.bind (fun u -> u.Rules 
                                                |> Map.tryFind name
                                                |> Option.map (fun r -> r |> Map.replace mapf <| name
                                                                          |> replaceRuleOnUnit gameState u))
                    |> defaultArg <| gameState
    let replaceRuleOnModel gameState (model : Model) replace = 
        let newmodel = { model with Rules = model.Rules |> replace }
        updateUnitInGameState model newmodel gameState
    let tryReplaceRuleOnModel name mapf mId gameState = 
        tryFindModel gameState mId 
                    |> Option.bind (fun m -> m.Model.Rules 
                                                |> Map.tryFind name
                                                |> Option.map (fun r -> r |> Map.replace mapf <| name
                                                                          |> replaceRuleOnModel gameState m.Model))
                    |> defaultArg <| gameState
    let forAllModels f newUnit gameState = 
        [ for m in newUnit.UnitModels do
                yield f m.Value ]
        |> List.fold (fun acc m -> match Map.tryFind m.Model.Id acc with
                                    | Some _ -> Map.replace id m m.Model.Id acc
                                    | None -> Map.add m.Model.Id m acc) gameState.Board.Models