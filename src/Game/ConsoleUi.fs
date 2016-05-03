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
                            | UnitRule nextMove -> sprintf "%s - %A" nextMove.Unit.UnitName nextMove.Rule
                            | EndRule nextMove ->  sprintf "%A" nextMove.Rule)
    /// Print the rules on the console.
    let displayNextMoves nextMoves = 
        nextMoves |> nextMovesToRulesList
        |> List.iteri (fun i r -> 
            printfn "%i) %s" i r)

    let getCapability selectedIndex (nextMoves:NextMoveInfo list) = 
        if selectedIndex < List.length nextMoves then
            match List.item selectedIndex nextMoves with
                | EndRule r -> Some r.Capability
                | UnitRule r -> Some r.Capability
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
    [<Measure>] 
    type HeightChars 
    [<Measure>] 
    type WidthChars
        /// Display the cells on the console in a grid

    let ftToPx x = x * inch.perFootI |> inch.ToPixelsI (int characterResolution * 1<dpi>)

    let displayBoard gameState = 
        
        let toCharacterWidth x =  x / 6<px/WidthChars>
        let toCharacterHeight x =  x / 12<px/HeightChars>
        
        let playerToStr  = function
            | Player1 -> "1"
            | Player2 -> "2"
        let boardToStr board = 
            let maxHeight = ftToPx board.Dimensions.Height |> toCharacterHeight
            let maxWidth = ftToPx board.Dimensions.Width |> toCharacterWidth
            let inline initCollection s =
                let coll = new ^t()
                Seq.iter (fun (k,v) -> (^t : (member Add : 'a * 'b -> unit) coll, k, v)) s
                coll

            let boardDisplay:System.Collections.Generic.Dictionary<int<WidthChars>*int<HeightChars>,string> = 
                initCollection (seq { for x in 0 .. (int maxWidth) do
                                        for y in 0 .. (int maxHeight) do
                                            yield ((x * 1<WidthChars>,y*1<HeightChars>),"█") } )
                
            
            board.Models 
                |> List.map (fun x -> (x.Position.X |> toCharacterWidth, 
                                        x.Position.Y |> toCharacterHeight), 
                                        x.Player)
                |> List.iter (fun ((x,y), player) -> boardDisplay.Item((x,y)) <- (playerToStr player))
            seq { for y in 0 .. (int maxHeight) do
                    yield seq { for x in 0 .. (int maxWidth) do
                                    yield  boardDisplay.Item((x * 1<WidthChars>,y*1<HeightChars>))}}
            |> (Seq.map (fun seq -> seq |> Seq.reduce (+)))
            
        
        boardToStr gameState.Board |> Seq.iter (printfn "%s")    // add some space

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
            ContinuePlay (api.NewGame())
        | "n" -> 
            ExitGame
        | _ -> askToPlayAgain api 

    let rec gameLoop api userAction = 
        printfn "\n------------------------------\n"  // a separator between moves
        
        match userAction with
        | ExitGame -> 
            printfn "Exiting game."             
        
        | ContinuePlay moveResult -> 
            // handle each case of the result
            match moveResult with
            | GameTied gameState -> 
                gameState |> displayBoard
                printfn "GAME OVER - Tie"       
                printfn "Turn: %A" gameState.Game.Turn      
                printfn ""             
                let nextUserAction = askToPlayAgain api 
                gameLoop api nextUserAction
            | GameWon (gameState,player) -> 
                gameState |> displayBoard
                printfn "GAME WON by %A" player    
                printfn "Turn: %A" gameState.Game.Turn          
                printfn ""             
                let nextUserAction = askToPlayAgain api 
                gameLoop api nextUserAction
            | Player1ToMove (gameState,nextMoves) -> 
                gameState |> displayBoard
                printfn "Player 1 to move" 
                printfn "Turn: %A" gameState.Game.Turn     
                displayNextMoves nextMoves
                let newResult = processInput nextMoves
                gameLoop api newResult 
            | Player2ToMove (gameState,nextMoves) -> 
                gameState |> displayBoard
                printfn "Player 2 to move" 
                printfn "Turn: %A" gameState.Game.Turn     
                displayNextMoves nextMoves
                let newResult = processInput nextMoves
                gameLoop api newResult 

    /// start the game with the given API
    let startGame api =
        let userAction = ContinuePlay (api.NewGame())
        gameLoop api userAction 

    let (|IntPx|_|) str =
       match System.Int32.TryParse(str) with
       | (true,int) -> Some(int * 1<px>)
       | _ -> None

    let rec positionAsker gameState =
        printfn "Give X coordinates"
        let x = Console.ReadLine()
        printfn "Give Y coordinates"
        let y = Console.ReadLine()
        match x, y, gameState.Board.Dimensions.Width, gameState.Board.Dimensions.Height with
            | IntPx xp, IntPx yp, maxX, maxY when xp >= 0<px> && yp >= 0<px> && xp <= ftToPx maxX && yp <= ftToPx maxY -> {X=xp; Y=yp}
            | _,_, maxX, maxY-> printfn "Please enter numbers within 0-%i wide and 0-%i tall" (ftToPx maxX) (ftToPx maxY)
                                positionAsker gameState

    let rec moveAsker positions =
        positions |> Array.iteri (fun i p -> printfn "%i) %i %i" i p.X p.Y)
        printfn "Enter an int corresponding to a displayed move or q to quit:" 
        match Console.ReadLine() |> Int32.TryParse with
        // TryParse will output a tuple (parsed?,int)
        | true, inputIndex ->
            match Array.tryItem inputIndex positions with
                | Some p -> p
                | None -> moveAsker positions
        | false, _ -> moveAsker positions
