namespace Logging
module Logger = 
    open Domain.Board
    open Domain.Game
    open Domain
    /// Transform a MoveCapability into a logged version

    let transformCapability transformMR player rule unit (cap:MoveCapability) :MoveCapability  =
        // create a new capability that logs the player & cellPos when run
        let newCap() =
            let withUnit = 
                match unit with 
                    | Some u -> sprintf " with %A" u
                    | None -> ""
            printfn "LOGINFO: %A played %A%s" player rule withUnit
            let moveResult = cap() 
            transformMR moveResult 
        newCap
    /// Transform a NextMove into a logged version
    let transformNextMove transformMR player move = 
        match move with 
            | UnitRuleInfo ur as uri,cap -> 
                let rule = ur.Rule  
                let unit = Some ur.UnitId
                uri,transformCapability transformMR player rule unit cap
            | GameStateRuleInfo ur as uri,cap ->
                let rule = ur
                uri, transformCapability transformMR player rule None cap
            | ModelRuleInfo ur as uri,cap ->
                let rule = ur.Rule  
                uri, transformCapability transformMR player rule None cap
    /// Transform a MoveResult into a logged version



    let rec transformMoveResult (moveResult:RuleResult) :RuleResult =
        let tmr = transformMoveResult
        match moveResult with
        | Player1ToMove (display,Next nextMoves) ->
            let nextMoves' =  transformNextMove tmr Player1 nextMove
            Player1ToMove (display,Next nextMoves') 
        | Player2ToMove (display,Next nextMove) ->
            let nextMoves' =  transformNextMove tmr Player2 nextMove
            Player2ToMove (display,Next nextMoves') 
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
