
    
namespace GameImpl
module Map =
    let pickKeyOfItem item map = Map.pick (fun k ur -> if item = ur then Some k  else None) map
    let replace f x oldKey = (Map.remove oldKey >> Map.add oldKey (f x))
module WarhammerImpl = 
    open Domain.WarhammerDomain
    open System    
    let other player = 
        player |> function 
        | Player1 -> Player2
        | Player2 -> Player1
    
    let private (|Leader|Tied|) gameState = 
        let maxPlayerInfo = gameState.Players |> List.sortBy (fun p -> p.Score)
        match maxPlayerInfo with
        | p1 :: p2 :: _ when p1.Score > p2.Score -> Leader p1.Player
        | _ -> Tied
    
    let private (|EndGame|_|) = function 
        | GameState gameState -> if gameState.Game.Mission.EndCondition gameState then Some gameState else None
        | _ -> None
    let pick nextMoveInfos = nextMoveInfos |> function
                                                | [] -> None
                                                | x :: _ -> Some x
    
    let rec removeFirst pred lst = 
        match lst with
        | h :: t when pred h -> t
        | h :: t -> h :: removeFirst pred t
        | _ -> []
    
    //
    //    let getDisplayInfo (gameState:GameState) =
    //        {DisplayInfo.Board = gameState.Board}
    let rec isRunnable = 
        function 
        | Function _ -> true
        | Nested(r, r2) -> 
            seq { 
                yield r
                yield r2
            }
            |> Seq.exists isRunnable
        | Overwritten(r, _) -> isRunnable r
        | DeactivatedUntilEndOfPhase _ -> false
        | DeactivatedUntilEndOfGame _ -> false
        | Description _ -> false
        | DeactivatedUntilEndOfPhaseOnFirstUse _ -> true
        | DeactivatedUntilEndOfGameOnFirstUse _ -> true
        | Characteristic _ -> false
    
    let rec collectRules = 
        function 
        | Function e -> [ Function e ]
        | Characteristic _ -> []
        | Nested(r, r2) -> 
            [ r; r2 ]
            |> List.map collectRules
            |> List.collect id
        | Overwritten(r, _) -> collectRules r
        | DeactivatedUntilEndOfPhase _ -> []
        | DeactivatedUntilEndOfGame _ -> []
        | Description _ -> []
        | DeactivatedUntilEndOfPhaseOnFirstUse r -> [DeactivatedUntilEndOfPhaseOnFirstUse r]
        | DeactivatedUntilEndOfGameOnFirstUse r -> [DeactivatedUntilEndOfGameOnFirstUse r]
    
    let splitPt = 
        function 
        | Top(x) -> Top, x
        | Bottom(x) -> Bottom, x
    
    let splitGt = 
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
    
    let replace xs x y = 
        let pred z = x = z
        y :: (removeFirst pred xs)
    
    let replacePlayerUnits p u nu = { p with Units = Map.replace id nu u.Id p.Units }
    let replaceGameStatePlayers s p np = { s with Players = replace s.Players p np }
    let tryFindNewUnit (unitId: Guid option) gameState = 
        gameState.Players |> List.tryPick (fun p -> unitId |> Option.bind(fun unitId -> p.Units |> Map.tryFind unitId))
    let tryFindPlayer gameState unit = 
        gameState.Players |> List.tryPick (fun p -> 
                                                     p.Units
                                                     |> Map.tryFind unit.Id
                                                     |> Option.bind (fun _ -> Some p))
    let findModel (model:Model) gameState  = 
        gameState.Board.Models |> Map.find model.Id
    let updatePlayerInGameState unit newUnit gameState = 
        let foundPlayer = tryFindPlayer gameState unit
        let newPlayer = foundPlayer |> Option.map (fun p -> replacePlayerUnits p unit newUnit)
        match gameState, foundPlayer, newPlayer with
        | (gs, Some p, Some np) -> replaceGameStatePlayers gs p np
        | (gs, _, _) -> gs

    let forAllModels f newUnit gameState = 
        [ for m in newUnit.UnitModels do
                yield f m.Value ]
        |> List.fold (fun acc m -> match Map.tryFind m.Model.Id acc with
                                    | Some _ -> Map.replace id m m.Model.Id acc
                                    | None -> Map.add m.Model.Id m acc) gameState.Board.Models
    let deploy u gameState positionAsker = 
        let foundPlayer = tryFindPlayer gameState u
        let newUnit = { u with Deployment = Deployed }
        match foundPlayer with
        | Some p -> 
            { gameState with Board = { gameState.Board with Models = forAllModels (fun m -> { Model = m
                                                                                              Player = p.Player
                                                                                              Position = gameState |> positionAsker }) newUnit gameState} }
            |> updatePlayerInGameState u newUnit
        | None -> failwith "Couldn't find player"

    let move u gameState maxMove moveAsker = 
        let pixelsInCircle radius position =  seq {
            for x in createSeq (position.X - radius) (position.X + radius) do
                for y in createSeq (position.Y - radius) (position.Y + radius) do
                    let newPos = {X=x;Y=y}
                    if x > 0<px> && y > 0<px> && position.FindDistance newPos <= radius then
                        yield newPos
        }    


        let newPosition m = 
            let ps = pixelsInCircle ((inch.ToPixels characterResolution maxMove / 1.<px> |> System.Math.Round |> int) * 1<px>) m.Position |> Seq.toArray
            let rec newPick ps = 
                let (p:Position<px>) = moveAsker ps
                if Seq.contains p ps then p
                else newPick ps
            newPick ps

        match tryFindPlayer gameState u with
        | Some p -> 
            { gameState with Board = 
                                { gameState.Board with Models = forAllModels (fun m -> { Model = m
                                                                                         Player = p.Player
                                                                                         Position = findModel m gameState |> newPosition }) u gameState} }
        | None -> failwith "Couldn't find player"
    
    let advancePhase gs = 
        let nextGt x = 
            match x with
            | Begin -> (GameTurn.One Phase.Begin)
            | One _ -> (GameTurn.Two Phase.Begin)
            | Two _ -> (GameTurn.Three Phase.Begin)
            | Three _ -> (GameTurn.Four Phase.Begin)
            | Four _ -> (GameTurn.Five Phase.Begin)
            | Five _ -> (GameTurn.Six Phase.Begin)
            | Six _ -> (GameTurn.Seven Phase.Begin)
            | Seven _ -> (GameTurn.End)
            | End -> (GameTurn.End)
        
        let enableDeactivatedRules gameState = 
            let rec enableRule = 
                function 
                | Nested(r, r2) -> Nested(enableRule r, enableRule r2)
                | DeactivatedUntilEndOfPhase r -> r
                | r -> r
            
            let createNewUnit (unit : Unit) = { unit with Rules = Map.map (fun k t -> enableRule t) unit.Rules }
            let newGameState = 
                gameState.Players 
                |> List.fold 
                       (fun acc p -> 
                       p.Units |> Map.fold (fun acc2 _ unit -> updatePlayerInGameState unit (createNewUnit unit) acc2) acc) 
                       gameState
            newGameState.Players
        
        let otherPt = 
            function 
            | Top(x) -> 
                Phase.Begin
                |> (splitGt x |> fst)
                |> Bottom
            | Bottom(x) -> Top(nextGt x)
        
        let changePhase turn = 
            let (PtMaker, gt) = splitPt turn
            let (GtMaker, phase) = splitGt gt
            match phase with
            | Some Phase.Begin -> PtMaker(GtMaker Phase.Movement)
            | Some Phase.Movement -> PtMaker(GtMaker Phase.Psychic)
            | Some Phase.Psychic -> PtMaker(GtMaker Phase.Shooting)
            | Some Phase.Shooting -> PtMaker(GtMaker Phase.Assault)
            | Some Phase.Assault -> PtMaker(GtMaker Phase.End)
            | Some Phase.End | None -> otherPt turn
        
        { gs with Game = { gs.Game with Turn = changePhase gs.Game.Turn }
                  Players = gs |> enableDeactivatedRules }
    let replaceRuleOnUnit gameState (unit : Unit) replace = 
        let newUnit = { unit with Rules = unit.Rules |> replace }
        updatePlayerInGameState unit newUnit gameState

    let rec eval fs (u:Guid option) (gameState:GameState) : Game = 
        match fs with
        | [] -> GameState gameState
        | h :: tail -> 
            (h, tryFindNewUnit u gameState)
            |> function 
                | Function(EndPhase), _ -> GameState (advancePhase gameState)
                | Function(Deploy), Some u ->  PositionAsker (deploy u gameState)
                | Function(Move maxMove), Some u -> MoveAsker (move u gameState maxMove)
                | Function(SetCharacteristicUnit(name, r)), Some u -> 
                    u.Rules 
                    |> Map.find name 
                    |> Map.replace (Rule.CreateNested r) <| name
                    |> replaceRuleOnUnit gameState u |> GameState
                | DeactivatedUntilEndOfPhaseOnFirstUse(r) as dr, Some u -> 
                    u.Rules 
                    |> Map.pickKeyOfItem dr 
                    |> Map.replace DeactivatedUntilEndOfPhase dr
                    |> replaceRuleOnUnit gameState u 
                    |> eval (collectRules r) (Some u.Id)
                | DeactivatedUntilEndOfGameOnFirstUse(r) as dr, Some u ->  
                    u.Rules 
                    |> Map.pickKeyOfItem dr 
                    |> Map.replace DeactivatedUntilEndOfGame dr
                    |> replaceRuleOnUnit gameState u 
                    |> eval (collectRules r) (Some u.Id)
                | _ -> GameState gameState
            |> function
                | GameState gs -> eval tail u gs
                | PositionAsker pa -> let chain = pa >> eval tail u
                                      let gss = (fun gs -> chain gs)
                | g -> Chain(fun gs -> g gs)
                | MoveAsker ma -> Chain(ma >> eval tail u)
                | DiceRollAsker dra -> Chain(dra >> eval tail u)
    
    
    let endPhase = Function(EndPhase)
    
    let availableRuleCapabilities player gs = 
        gs.Players
        |> List.filter (fun p -> p.Player = player)
        |> List.collect (fun p -> p.Units |> Map.toList |> List.unzip |> snd)
        |> List.collect (fun u -> 
               u.Rules
               |> Map.toList
               |> List.filter (fun (_,t) -> isRunnable t)
               |> List.map (fun (_,r) -> r, Some u))
    
    let makeNextMoveInfo f (player : Player) gameState (rule, unit) = 
        let capability() = f player unit rule gameState
        match unit with
        | Some u -> 
            UnitRule({ Unit = u
                       Rule = rule
                       Capability = capability })
        | None -> 
            EndRule { Rule = rule
                      Capability = capability }
    
    let moveResultFor player gs nextMoves = 
        match player with
        | Player1 -> Player1ToMove(gs, nextMoves)
        | Player2 -> Player2ToMove(gs, nextMoves)
    
    let makeMoveResultWithCapabilities f player newGameState rulesAndUnits = 
        match rulesAndUnits with
        | [] -> None
        | rulesAndUnits -> 
            (endPhase, None) :: rulesAndUnits
            |> List.map (makeNextMoveInfo f player newGameState)
            |> moveResultFor player newGameState
            |> Some
    
    let moveResult (gs:Game) playerMove currentPlayer = 
        let result = 
            gs
            |> availableRuleCapabilities currentPlayer
            |> makeMoveResultWithCapabilities playerMove currentPlayer gs
        match result with
        | Some ruleResult -> ruleResult
        | None -> playerMove currentPlayer None endPhase gs
    
    let rec playerMove player (unit:Unit option) thingToDo (gameState:Game) = 
        let newGameState = 
            let uId = unit |> Option.map (fun unit -> unit.Id)
            eval (collectRules thingToDo) uId gameState
        
        let newPlayer = 
            match (gameState, newGameState) with
                | (GameState gs, GameState newGs) ->
                    match gs.Game.Turn, newGs.Game.Turn with
                    | Top(_), Top(_) -> player
                    | Top(_), Bottom(_) -> other player
                    | Bottom(_), Top(_) -> other player
                    | Bottom(_), Bottom(_) -> player
                | _ -> player
        
        match newGameState with
         | EndGame gs -> 
            match gs with 
                | Leader player -> GameWon(gs, player)
                | Tied -> GameTied gs
         
         | _ -> moveResult newGameState playerMove newPlayer
    
    let newGame  () = 
        // create initial game state
        let gameState = Impl.ImplTest.initial |> GameState
        moveResult gameState playerMove Player1
    
    /// export the API to the application
    let api = { NewGame = newGame }
