namespace ConsoleUi
module ConsoleWarhammer = 
    open Domain.WarhammerDomain
    open System
    /// Track the UI state
    type UserAction<'a> =
        | ContinuePlay of 'a
        | ExitGame
    
    let nextMovesToRulesList nextMoves = 
        nextMoves 
        |> List.map (function 
                            | UnitRule nextMove -> nextMove.Rule
                            | EndRule nextMove -> nextMove.Rule)
    /// Print the rules on the console.
    let displayNextMoves nextMoves = 
        nextMoves |> nextMovesToRulesList
        |> List.iteri (fun i (r,unitName) -> 
            printfn "%i) %s - %A" i unitName.UnitName r)

    let getCapability selectedIndex (nextMoves:NextMoveInfo list) = 
        if selectedIndex < List.length nextMoves then
            let (NextMoveInfo (_, moveCapability)) = List.item selectedIndex nextMoves 
            Some moveCapability
        else
            None

    let processMoveIndex inputStr availableMoves processInputAgain = 
        match Int32.TryParse inputStr with
        // TryParse will output a tuple (parsed?,int)
        | true,inputIndex ->
            // parsed ok, now try to find the corresponding move
            match getCapability inputIndex availableMoves with
            | Some capability -> 
                // corresponding move found, so make a move
                let moveResult = capability()  
                ContinuePlay moveResult // return it
            | None ->
                // no corresponding move found
                printfn "...No move found for inputIndex %i. Try again" inputIndex 
                // try again
                processInputAgain()
        | false, _ -> 
            // int was not parsed
            printfn "...Please enter an int corresponding to a displayed move."             
            // try again
            processInputAgain()

        /// Display the cells on the console in a grid
    let displayCells gameState = 
        let board = gameState.Board
        let characterWidth = 12<px>
        let characterHeight = 6<px>
        let ftToPx x = x * inch.perFoot |> inch.ToPixels resolution
        let maxHeight = ftToPx board.Dimensions.Height
        let maxWidth = ftToPx board.Dimensions.Height
        let models = gameState.Board.Models |>
                        
        let cellToStr cell = 
            match cell.state with
            | Empty -> "-"            
            | Played player ->
                match player with
                | PlayerO -> "O"
                | PlayerX -> "X"

        let printCells cells  = 
            cells
            |> List.map cellToStr
            |> List.reduce (fun s1 s2 -> s1 + "|" + s2) 
            |> printfn "|%s|"

        let topCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Top) 
        let centerCells = 
            cells |> List.filter (fun cell -> snd cell.pos = VCenter) 
        let bottomCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Bottom) 
        
        printCells topCells
        printCells centerCells 
        printCells bottomCells 
        printfn ""   // add some space

    /// Ask the user for input. Process the string entered as 
    /// a move index or a "quit" command
    let rec processInput availableCapabilities = 

        // helper that calls this function again with exactly
        // the same parameters
        let processInputAgain() = 
            processInput availableCapabilities 

        printfn "Enter an int corresponding to a displayed move or q to quit:" 
        let inputStr = Console.ReadLine()
        if inputStr = "q" then
            ExitGame
        else
            processMoveIndex inputStr availableCapabilities processInputAgain

module Console =
    open Domain.TickTacToeDomain
    open System
    /// Track the UI state
    type UserAction<'a> =
        | ContinuePlay of 'a
        | ExitGame

    /// Print each available move on the console
    let displayNextMoves nextMoves = 
        nextMoves 
        |> List.iteri (fun i moveInfo -> 
            printfn "%i) %A" i moveInfo.posToPlay)

    /// Get the move corresponding to the 
    /// index selected by the user
    let getCapability selectedIndex nextMoves = 
        if selectedIndex < List.length nextMoves then
            let move = List.item selectedIndex nextMoves 
            Some move.capability 
        else
            None

    /// Given that the user has not quit, attempt to parse
    /// the input text into a index and then find the move
    /// corresponding to that index
    let processMoveIndex inputStr availableMoves processInputAgain = 
        match Int32.TryParse inputStr with
        // TryParse will output a tuple (parsed?,int)
        | true,inputIndex ->
            // parsed ok, now try to find the corresponding move
            match getCapability inputIndex availableMoves with
            | Some capability -> 
                // corresponding move found, so make a move
                let moveResult = capability()  
                ContinuePlay moveResult // return it
            | None ->
                // no corresponding move found
                printfn "...No move found for inputIndex %i. Try again" inputIndex 
                // try again
                processInputAgain()
        | false, _ -> 
            // int was not parsed
            printfn "...Please enter an int corresponding to a displayed move."             
            // try again
            processInputAgain()

    /// Ask the user for input. Process the string entered as 
    /// a move index or a "quit" command
    let rec processInput availableCapabilities = 

        // helper that calls this function again with exactly
        // the same parameters
        let processInputAgain() = 
            processInput availableCapabilities 

        printfn "Enter an int corresponding to a displayed move or q to quit:" 
        let inputStr = Console.ReadLine()
        if inputStr = "q" then
            ExitGame
        else
            processMoveIndex inputStr availableCapabilities processInputAgain
    
    let rec askToPlayAgain api  = 
        printfn "Would you like to play again (y/n)?"             
        match Console.ReadLine() with
        | "y" -> 
            ContinuePlay (api.newGame())
        | "n" -> 
            ExitGame
        | _ -> askToPlayAgain api 
        
    /// Display the cells on the console in a grid
    let displayCells displayInfo = 
        let cells = displayInfo.cells
        let cellToStr cell = 
            match cell.state with
            | Empty -> "-"            
            | Played player ->
                match player with
                | PlayerO -> "O"
                | PlayerX -> "X"

        let printCells cells  = 
            cells
            |> List.map cellToStr
            |> List.reduce (fun s1 s2 -> s1 + "|" + s2) 
            |> printfn "|%s|"

        let topCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Top) 
        let centerCells = 
            cells |> List.filter (fun cell -> snd cell.pos = VCenter) 
        let bottomCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Bottom) 
        
        printCells topCells
        printCells centerCells 
        printCells bottomCells 
        printfn ""   // add some space
        
    /// After each game is finished,
    /// ask whether to play again.
    let rec askToPlayAgain api  = 
        printfn "Would you like to play again (y/n)?"             
        match Console.ReadLine() with
        | "y" -> 
            ContinuePlay (api.newGame())
        | "n" -> 
            ExitGame
        | _ -> askToPlayAgain api 

    /// The main game loop, repeated
    /// for each user input
    let rec gameLoop api userAction = 
        printfn "\n------------------------------\n"  // a separator between moves
        
        match userAction with
        | ExitGame -> 
            printfn "Exiting game."             
        
        | ContinuePlay moveResult -> 
            // handle each case of the result
            match moveResult with
            | GameTied displayInfo -> 
                displayInfo |> displayCells
                printfn "GAME OVER - Tie"             
                printfn ""             
                let nextUserAction = askToPlayAgain api 
                gameLoop api nextUserAction
            | GameWon (displayInfo,player) -> 
                displayInfo |> displayCells
                printfn "GAME WON by %A" player            
                printfn ""             
                let nextUserAction = askToPlayAgain api 
                gameLoop api nextUserAction
            | PlayerOToMove (displayInfo,nextMoves) -> 
                displayInfo |> displayCells
                printfn "Player O to move" 
                displayNextMoves nextMoves
                let newResult = processInput nextMoves
                gameLoop api newResult 
            | PlayerXToMove (displayInfo,nextMoves) -> 
                displayInfo |> displayCells
                printfn "Player X to move" 
                displayNextMoves nextMoves
                let newResult = processInput nextMoves
                gameLoop api newResult 

    /// start the game with the given API
    let startGame api =
        let userAction = ContinuePlay (api.newGame())
        gameLoop api userAction 

