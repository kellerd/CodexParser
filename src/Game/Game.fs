namespace Domain
module Game =
    open WarhammerDomain
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
        | Next of (RuleInfo * MoveCapability) list
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