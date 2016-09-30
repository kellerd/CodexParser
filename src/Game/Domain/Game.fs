namespace Domain
module Game =
    open Domain.Tabletop
    type EvalResult = 
        | GameStateResult of GameState
        | AskResult of Asker<EvalResult>
    and MoveCapability = 
        unit -> RuleResult
    and RuleInfo = 
        | UnitRuleInfo of UnitRuleInfo
        | ModelRuleInfo of ModelRuleInfo 
        | GameStateRuleInfo of Rule: Rule
    and NextResult= 
        | Next of MoveCapability
        | Ask of Asker<RuleResult>
    and RuleResult = 
        | Player1ToMove of GameState * NextResult
        | Player2ToMove of GameState * NextResult
        | GameWon of GameState * Player 
        | GameTied of GameState 

    // Only the newGame function is exported from the implementation
    // all other functions come from the results of the previous move
    type WarhammerAPI  = 
        {
        NewGame : MoveCapability
        }

    let woundTable str tough =
        match str - tough with 
        | 0 -> 4
        | 1 -> 3
        | -1 -> 5
        | -2 | -3 -> 6
        | x when x > 0 -> 2
        | _ -> 0
        |> DiceRoll
    let armourTable saves pen =
        let pen' = defaultArg pen 7
        match pen' - saves with
        | x when x > 0 -> saves
        | _ -> 2
        |> DiceRoll

    let hitAssaultTable ws wsOpponent =
         match ws,wsOpponent with
            | x,y when x > y -> 3
            | x,y when y > x * 2 -> 5
            | _ -> 4
        |> DiceRoll