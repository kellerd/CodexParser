// -----------------------------------------------------------
namespace Logging
// -----------------------------------------------------------

module Logger = 
    open TicTacToeDomain
     
    /// Transform a MoveCapability into a logged version
    let transformCapability transformMR player cellPos (cap:MoveCapability) :MoveCapability =
        
        // create a new capability that logs the player & cellPos when run
        let newCap() =
            printfn "LOGINFO: %A played %A" player cellPos
            let moveResult = cap() 
            transformMR moveResult 
        newCap

    /// Transform a NextMove into a logged version
    let transformNextMove transformMR player (move:NextMoveInfo) :NextMoveInfo = 
        let cellPos = move.posToPlay 
        let cap = move.capability
        {move with capability = transformCapability transformMR player cellPos cap} 

    /// Transform a MoveResult into a logged version
    let rec transformMoveResult (moveResult:MoveResult) :MoveResult =
        
        let tmr = transformMoveResult // abbreviate!

        match moveResult with
        | PlayerXToMove (display,nextMoves) ->
            let nextMoves' = nextMoves |> List.map (transformNextMove tmr PlayerX) 
            PlayerXToMove (display,nextMoves') 
        | PlayerOToMove (display,nextMoves) ->
            let nextMoves' = nextMoves |> List.map (transformNextMove tmr PlayerO)
            PlayerOToMove (display,nextMoves') 
        | GameWon (display,player) ->
            printfn "LOGINFO: Game won by %A" player 
            moveResult
        | GameTied display ->
            printfn "LOGINFO: Game tied" 
            moveResult

    /// inject logging into the API
    let injectLogging api =
       
        // create a new API with the functions 
        // replaced with logged versions
        { api with
            newGame = fun () -> api.newGame() |> transformMoveResult
            }

module ConsoleApplication = 

    let startGame() =
        let api = TicTacToeImplementation.api
        let loggedApi = Logger.injectLogging api
        ConsoleUi.startGame loggedApi 
