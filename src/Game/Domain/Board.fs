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
      UnitModels  : Map<ModelGuid,Model>
      UnitName    : string
      Rules : Map<string,Rule>
    } 

    type Player = Player1 | Player2

    let drawingResolution = 26.0<dpi>
    type BoardDimensions = {Width:int<ft>; Height:int<ft>}
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
        Dimensions : BoardDimensions
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

    let pixelsOfBase b = 
        match b.Model.Base with 
        | BaseDiameter mms -> pixelsInCircle ((float mms) *  LanguagePrimitives.FloatWithMeasure<mm> 1. |> mmToInch) b.Position
        | ModelDimentions({Width=w;Length=h}) -> pixelsInRectangle (mmToInch w) (mmToInch h)  b.Position
        |> Set.ofSeq

    let overlapping base1 base2 = 
        Set.intersect (pixelsOfBase base1) (pixelsOfBase base2) 

    let touching base1 base2
        (pixelsOfBase base1) |> Set.