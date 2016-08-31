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

    let pixelsInBase b = 
        match b.Model.Base with 
        | BaseDiameter mms -> pixelsInCircle ((float mms) *  LanguagePrimitives.FloatWithMeasure<mm> 1. |> mmToInch) b.Position
        | ModelDimentions({Width=w;Length=h}) -> pixelsInRectangle (mmToInch w) (mmToInch h)  b.Position
        |> Set.ofSeq

    let pixelsOfBase b = 
        match b.Model.Base with 
        | BaseDiameter mms -> pixelsOfCircle ((float mms) *  LanguagePrimitives.FloatWithMeasure<mm> 1. |> mmToInch) b.Position
        | ModelDimentions({Width=w;Length=h}) -> pixelsOfRectangle (mmToInch w) (mmToInch h)  b.Position
        |> Set.ofSeq

    let overlapping base1 base2 = 
        Set.intersect (pixelsInBase base1) (pixelsInBase base2) 
    let shift xshift yshift pxs =
        Set.map (fun {X=x;Y=y} -> {X=x+xshift;Y=y+yshift}) pxs
    let shimmy pxs =
        seq {
            for x in -1 .. 1 do
            for y in -1 .. 1 do
                if not (x = 0 && y = 0) then
                    let x' = x * (LanguagePrimitives.Int32WithMeasure 1)
                    let y' = y * (LanguagePrimitives.Int32WithMeasure 1)
                    yield shift x' y' pxs
        }
    let touching base1 base2 =
        let firstBase = shimmy (pixelsOfBase base1) |> Set.unionMany 
        let secondBase =  (pixelsInBase base2)
        printfn "%A" firstBase
        printfn "%A" secondBase
        Set.intersect firstBase secondBase


    // open Domain
    // open WarhammerDomain
    // open Domain.WarhammerDomain
    // open Domain.Board
    // let m1 = { Model = { Name = "Termagant"
    //                      Id = (System.Guid.NewGuid())
    //                      Base = BaseDiameter 50<mm>
    //                      Rules = Map.empty}; 
    //                     Player=Player1;
    //         Position={X=0<px>;Y=0<px>}}
    // let m2 = { Model = { Name = "Termagant"
    //                      Id = (System.Guid.NewGuid())
    //                      Base = BaseDiameter 50<mm>
    //                      Rules = Map.empty}; 
    //                      Player=Player1;
    //         Position={X=1<px>;Y=0<px>}}
    // Board.touching m1 m2 |> Seq.isEmpty
    // let mms = 1<mm>
    // (float mms) *  LanguagePrimitives.FloatWithMeasure<mm> 1. |> mmToInch
    // pixelsInBase m2 |> Set.toList
    // roundToPixels (0.03937007874<inch> / 2.) |> max 1<px>

