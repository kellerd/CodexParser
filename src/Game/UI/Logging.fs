namespace Logging
module Logger = 
    open Domain.Tabletop
    open Domain.Game
    open Domain
    /// Transform a MoveCapability into a logged version

    let transformCapability transformMR player cap  =
        // create a new capability that logs the player & cellPos when run
        let newCap() =
            printfn "LOGINFO: %A played %A" player cap 
            let moveResult = cap() 
            transformMR moveResult 
        newCap
    /// Transform a MoveResult into a logged version
    let rec transformMoveResult (moveResult:RuleResult) :RuleResult =
        let tmr = transformMoveResult
        match moveResult with
        | Player1ToMove (display,Next nextMove) ->
            let nextMove' =  transformCapability tmr Player1 nextMove
            Player1ToMove (display,Next nextMove') 
        | Player2ToMove (display,Next nextMove) ->
            let nextMove' =  transformCapability tmr Player2 nextMove
            Player2ToMove (display,Next nextMove') 
        | Player1ToMove (_,Ask _) ->
            printfn "LOGINFO: Player1 Asking question"
            moveResult
        | Player2ToMove (_,Ask _) ->
            printfn "LOGINFO: Player2 Asking question"
            moveResult
        | GameWon (_,player) ->
            printfn "LOGINFO: Game won by %A" player 
            moveResult
        | GameTied _ ->
            printfn "LOGINFO: Game tied" 
            moveResult


    /// inject logging into the API
    let injectLogging api =
       
        // create a new API with the functions 
        // replaced with logged versions
        { api with
            NewGame = fun () -> api.NewGame() |> transformMoveResult
            }
