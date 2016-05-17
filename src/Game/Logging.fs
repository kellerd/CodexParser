namespace Logging
module Logger = 
    open Domain.WarhammerDomain
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
            | UnitRule ur -> 
                let rule = ur.Rule 
                let unit = Some ur.UnitId
                let cap = ur.Capability
                UnitRule({ur with Capability = transformCapability transformMR player rule unit cap})
            | EndRule er ->
                let rule = er.Rule 
                let cap = er.Capability
                EndRule({er with Capability = transformCapability transformMR player rule None cap})
    /// Transform a MoveResult into a logged version
    let rec transformMoveResult (moveResult:RuleResult) :RuleResult =
        let tmr = transformMoveResult
        match moveResult with
        | Player1ToMove (display,Next nextMoves) ->
            let nextMoves' = nextMoves |> List.map (transformNextMove tmr Player1) 
            Player1ToMove (display,Next nextMoves') 
        | Player2ToMove (display,Next nextMoves) ->
            let nextMoves' = nextMoves |> List.map (transformNextMove tmr Player2)
            Player2ToMove (display,Next nextMoves') 
        | Player1ToMove (display,Ask asker) ->
            printfn "LOGINFO: Player1 Asking question"
            moveResult
        | Player2ToMove (display,Ask asker) ->
            printfn "LOGINFO: Player2 Asking question"
            moveResult
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
            NewGame = fun () -> api.NewGame() |> transformMoveResult
            }
