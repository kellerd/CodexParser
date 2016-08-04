namespace Domain
module Board =
    open WarhammerDomain
            
    type [<ReferenceEquality>]  Model = {
      Name : string
      Id : ModelGuid
      Rules : Map<string,Rule>
      Base: Base
    } 
    and [<ReferenceEquality>] Unit = { 
      Id : UnitGuid
      UnitModels  : Map<UnitGuid,Model>
      UnitName    : string
      Rules : Map<string,Rule>
    } 

    type Player = Player1 | Player2

    let drawingResolution = 26.0<dpi>
    type Dimensions = {Width:int<ft>; Height:int<ft>}
    type Score = Score of int
    type GameState = {
        Board : BoardInfo
        Players : PlayerInfo list
        Rules : Map<string,Rule>
        Game:GameInfo
        }
    and PlayerInfo = {
        Player: Player
        Units: Map<UnitGuid,Unit>
        Score: Score
    }
    and BoardInfo = {
        Models : Map<ModelGuid,ModelInfo>
        Dimensions : Dimensions
    }
    and ModelInfo = {
        Model : Model
        Player : Player
        Position: Position<px>
    }
    and GameInfo = {
        Mission:Mission
    }
    and Mission = unit
    
    type UnitRuleInfo = { UnitId: UnitGuid; UnitName: string; Rule: Rule}
    type ModelRuleInfo = { ModelId: ModelGuid; Rule: Rule}

    type GenAsker<'a,'b> = Asker of ('a -> 'b)
        with static member Run (a:GenAsker<'a,'b>, input:'a) =  let (Asker asker') = a 
                                                                asker' input

    type Asker<'a> = 
        | PositionAsker of GenAsker<GameState -> Position<px>, 'a>
        | MoveAsker of GenAsker<Position<px>[] -> Position<px>, 'a>
        | DiceRollAsker of GenAsker<unit -> DiceRoll, 'a>
        with member this.Map(f) = 
                match this with
                    | PositionAsker(Asker(a)) -> a >> f |> Asker |> PositionAsker
                    | MoveAsker(Asker(a)) ->  a >> f |> Asker |> MoveAsker
                    | DiceRollAsker(Asker(a)) ->  a >> f |> Asker |> DiceRollAsker
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