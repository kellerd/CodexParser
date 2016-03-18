namespace GameImpl
module WarhammerImpl = 
    open Domain.WarhammerDomain
    
    let other player = player |> function
        | Player1 -> Player2
        | Player2 -> Player1

    let Mission = {
       MaxRounds = (fun gs -> Six)
       Rules = []
       EndCondition = (fun gs -> gs.Game.Round = Round.End && gs.Game.Phase = Phase.End)
    }

    let private gameWonBy gameState = 
        let maxPlayerInfo = gameState.Players |> List.sortBy (fun p -> p.Score)
        match maxPlayerInfo with 
            | p1::p2::_ when p1.Score > p2.Score -> Some p1.Player
            | _ -> None
    let private isEndCondition gameState = 
        (gameState.Game.Mission.EndCondition gameState)
    let pick nextMoveInfos = 
        nextMoveInfos |> List.tryHead

    let rec removeFirst pred lst =
        match lst with
        | h::t when pred h -> t
        | h::t -> h::removeFirst pred t
        | _ -> []
//
//    let getDisplayInfo (gameState:GameState) =
//        {DisplayInfo.Board = gameState.Board}
    let rec isRunnable = function
        | Single _ -> true
        | Nested (r,r2) -> 
            seq {yield r; yield r2} |> Seq.exists isRunnable
        | Overwritten (r,r2) -> isRunnable r
        | DeactivatedUntilEndOfPhase _ -> false
        | DeactivatedUntilEndOfGame _ -> false
        | Description _ -> false
    let rec collectRules = function
        | Single e -> [e]
        | Nested (r,r2) -> 
            [r;r2] |> List.map collectRules |> List.collect id
        | Overwritten (r,r2) -> collectRules r
        | DeactivatedUntilEndOfPhase _ -> []
        | DeactivatedUntilEndOfGame _ -> []
        | Description _ -> []
    let eval f gameState =
        gameState    
    let advanceRound gs = 
        match gs.Game.Round with 
            | Round.Begin -> Round.One
            | Round.One -> Round.Two
            | Round.Two -> Round.Three
            | Round.Three -> Round.Four
            | Round.Four -> Round.Five
            | Round.Five -> Round.Six
            | Round.Six -> Round.Seven
            | Round.Seven -> Round.End
            | Round.End -> Round.End
    let advancePhase gs = 
        match gs.Game.Phase with
            | Phase.Begin -> {gs with Game = {gs.Game with Phase = Phase.Movement} }
            | Phase.Movement -> {gs with Game = {gs.Game with Phase = Phase.Psychic}  }
            | Phase.Psychic -> {gs with Game = {gs.Game with Phase = Phase.Shooting}  }
            | Phase.Shooting -> {gs with Game = {gs.Game with Phase = Phase.Assault}  }
            | Phase.Assault -> {gs with Game = {gs.Game with Phase = Phase.End}       }
            | Phase.End -> {gs with Game = {gs.Game with Phase = Phase.Begin; Round = advanceRound gs}         }
    let updateGame f (unit:Unit) gameState  = 
        ///TODO
        let foundPlayer = gameState.Players |> List.tryPick (fun p -> p.Units 
                                                                    |> List.tryFind(fun u -> u = unit)
                                                                    |> Option.bind (fun _ -> Some p))
        let replace xs x y = 
            let pred z = x = z
            y :: (removeFirst pred xs)

        let cUnit f (unit:Unit)  = {unit with Rules = replace unit.Rules f (DeactivatedUntilEndOfPhase f)}
        let cPlayer p u nu = {p with Units = replace p.Units u nu }
        let cState s p np = {s with Players = replace s.Players p np }

        let newUnit = cUnit f unit
        let newPlayer = foundPlayer |> Option.map (fun p-> cPlayer p unit newUnit)
        match (eval (collectRules f) gameState), foundPlayer, newPlayer with
            | (gs, Some p, Some np) -> cState gs p np
            | (gs, _, _) -> gs
    let rec getCapabilities availableUnits gs player   = 
                match availableUnits gs player with
                    | [] -> match gs.Game.Phase with
                                | Phase.End ->  getCapabilities  availableUnits  <| advancePhase gs <| other player
                                | _ -> getCapabilities availableUnits  <| advancePhase gs  <| player
                    | xs -> NextMoveInfo xs, player, gs
    let availableUnits gs player : Unit list  = 
        gs.Players 
        |> List.filter (fun p -> p.Player = player) 
        |> List.collect (fun p -> p.Units)
        |> List.filter(fun u -> u.Rules |> Seq.ofList |> Seq.exists isRunnable)        
    let moveResultFor (nextMoves, player, gs) = 
        match player with
        | Player1 -> Player1ToMove (gs, nextMoves)
        | Player2 -> Player2ToMove (gs, nextMoves)

    let rec playerMove gameState player unit thingToDo = 
        let newGameState = gameState |> updateGame thingToDo unit
//        let displayInfo = getDisplayInfo newGameState 
        if isEndCondition gameState then 
            match gameWonBy gameState with
            | Some player -> GameWon (gameState, player) 
            | None -> GameTied gameState 
        else
            getCapabilities availableUnits newGameState player  |> moveResultFor

    let newGame() = 
        // create initial game state
        let gameState =  { 
                            Board = {
                                        Models=[]
                                        Dimensions = {Width=6<ft>;Height=4<ft>}
                                    }
                            Players = [
                                        {    
                                            Player= Player1
                                            Units= [Impl.ModelImplTest.TermUnit] 
                                            Score=Score 0
                                        };
                                        {    
                                            Player= Player2
                                            Units= [Impl.ModelImplTest.HormagauntUnit] 
                                            Score=Score 0
                                        }] 
                            Game={
                                    Phase = Phase.Begin
                                    Round = Round.Begin
                                    Mission= {
                                                MaxRounds=(fun _ ->Round.Six)
                                                Rules =[]
                                                EndCondition=(fun gs -> gs.Game.Round=Round.Seven )
                                              }
                                    }      
                        }
        let player1 = gameState.Players |> List.head  
        let moveResult = getCapabilities availableUnits gameState player1.Player  |> moveResultFor
        moveResult

    /// export the API to the application
    let api = {
        newGame = newGame 
        }

module TickTacToeImpl = 
    open Domain.TickTacToeDomain
    /// private implementation of game state
    type GameState = {
        cells : Cell list
        }

    /// the list of all horizontal positions
    let allHorizPositions = [Left; HCenter; Right]
    
    /// the list of all horizontal positions
    let allVertPositions = [Top; VCenter; Bottom]

    /// A type to store the list of cell positions in a line
    type Line = Line of CellPosition list

    /// a list of the eight lines to check for 3 in a row
    let linesToCheck = 
        let mkHLine v = Line [for h in allHorizPositions do yield (h,v)]
        let hLines= [for v in allVertPositions do yield mkHLine v] 

        let mkVLine h = Line [for v in allVertPositions do yield (h,v)]
        let vLines = [for h in allHorizPositions do yield mkVLine h] 

        let diagonalLine1 = Line [Left,Top; HCenter,VCenter; Right,Bottom]
        let diagonalLine2 = Line [Left,Bottom; HCenter,VCenter; Right,Top]

        // return all the lines to check
        [
        yield! hLines
        yield! vLines
        yield diagonalLine1 
        yield diagonalLine2 
        ]

    /// get the DisplayInfo from the gameState
    let getDisplayInfo gameState = 
        {DisplayInfo.cells = gameState.cells}

    /// get the cell corresponding to the cell position
    let getCell gameState posToFind = 
        gameState.cells 
        |> List.find (fun cell -> cell.pos = posToFind)

    /// update a particular cell in the GameState 
    /// and return a new GameState
    let private updateCell newCell gameState =

        // create a helper function
        let substituteNewCell oldCell =
            if oldCell.pos = newCell.pos then
                newCell
            else 
                oldCell                 

        // get a copy of the cells, with the new cell swapped in
        let newCells = gameState.cells |> List.map substituteNewCell 
        
        // return a new game state with the new cells
        {gameState with cells = newCells }

    /// Return true if the game was won by the specified player
    let private isGameWonBy player gameState = 
        
        // helper to check if a cell was played by a particular player
        let cellWasPlayedBy playerToCompare cell = 
            match cell.state with
            | Played player -> player = playerToCompare
            | Empty -> false

        // helper to see if every cell in the Line has been played by the same player
        let lineIsAllSamePlayer player (Line cellPosList) = 
            cellPosList 
            |> List.map (getCell gameState)
            |> List.forall (cellWasPlayedBy player)

        linesToCheck
        |> List.exists (lineIsAllSamePlayer player)


    /// Return true if all cells have been played
    let private isGameTied gameState = 
        // helper to check if a cell was played by any player
        let cellWasPlayed cell = 
            match cell.state with
            | Played _ -> true
            | Empty -> false

        gameState.cells
        |> List.forall cellWasPlayed 

    /// determine the remaining moves 
    let private remainingMoves gameState = 

        // helper to return Some if a cell is playable
        let playableCell cell = 
            match cell.state with
            | Played player -> None
            | Empty -> Some cell.pos

        gameState.cells
        |> List.choose playableCell

    // return the other player    
    let otherPlayer player = 
        match player with
        | PlayerX -> PlayerO
        | PlayerO -> PlayerX


    // return the move result case for a player 
    let moveResultFor player displayInfo nextMoves = 
        match player with
        | PlayerX -> PlayerXToMove (displayInfo, nextMoves)
        | PlayerO -> PlayerOToMove (displayInfo, nextMoves)

    // given a function, a player & a gameState & a position,
    // create a NextMoveInfo with the capability to call the function
    let makeNextMoveInfo f player gameState cellPos =
        // the capability has the player & cellPos & gameState baked in
        let capability() = f player cellPos gameState 
        {posToPlay=cellPos; capability=capability}

    // given a function, a player & a gameState & a list of positions,
    // create a list of NextMoveInfos wrapped in a MoveResult
    let makeMoveResultWithCapabilities f player gameState cellPosList =
        let displayInfo = getDisplayInfo gameState
        cellPosList
        |> List.map (makeNextMoveInfo f player gameState)
        |> moveResultFor player displayInfo 

    // player X or O makes a move
    let rec playerMove player cellPos gameState  = 
        let newCell = {pos = cellPos; state = Played player}
        let newGameState = gameState |> updateCell newCell 
        let displayInfo = getDisplayInfo newGameState 

        if newGameState |> isGameWonBy player then
            // return the move result
            GameWon (displayInfo, player) 
        elif newGameState |> isGameTied then
            // return the move result
            GameTied displayInfo 
        else
            let otherPlayer = otherPlayer player 
            let moveResult = 
                newGameState 
                |> remainingMoves
                |> makeMoveResultWithCapabilities playerMove otherPlayer newGameState
            moveResult 

    /// create the state of a new game
    let newGame() = 

        // allPositions is the cross-product of the positions
        let allPositions = [
            for h in allHorizPositions do 
            for v in allVertPositions do 
                yield (h,v)
            ]

        // all cells are empty initially
        let emptyCells = 
            allPositions 
            |> List.map (fun pos -> {pos = pos; state = Empty})
        
        // create initial game state
        let gameState = { cells=emptyCells }            

        // initial of valid moves for player X is all positions
        let moveResult = 
            allPositions 
            |> makeMoveResultWithCapabilities playerMove PlayerX gameState

        // return new game
        moveResult 


    /// export the API to the application
    let api = {
        newGame = newGame 
        }


