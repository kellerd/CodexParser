namespace Domain
module Tabletop =
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


    let drawingResolution = 26.0<dpi>
    type Score = Score of int
    type GameState = {
        Players : PlayerInfo list
        Rules : Map<string,Rule>
        Game:GameInfo
    }
    and PlayerInfo = {
        Player: Player
        Units: Map<UnitGuid,Unit>
        Score: Score
    }
    and GameInfo = {
        Mission:Mission
    }
    and Mission = unit
    
    type UnitRuleInfo = { UnitId: UnitGuid; UnitName: string; Rule: Rule}
    type ModelRuleInfo = { ModelId: ModelGuid; Rule: Rule}

    let pixelsInBase b position = 
        match b.Base with 
        | BaseDiameter mms -> pixelsInCircle ((float mms) *  LanguagePrimitives.FloatWithMeasure<mm> 1. |> mmToInch) position
        | ModelDimentions({Width=w;Length=h}) -> pixelsInRectangle (mmToInch w) (mmToInch h)  position
        |> Set.ofSeq

    let pixelsOfBase b position = 
        match b.Base with 
        | BaseDiameter mms -> pixelsOfCircle ((float mms) *  LanguagePrimitives.FloatWithMeasure<mm> 1. |> mmToInch) position
        | ModelDimentions({Width=w;Length=h}) -> pixelsOfRectangle (mmToInch w) (mmToInch h)  position
        |> Set.ofSeq

    let overlapping (base1,pos1) (base2,pos2) = 
        Set.intersect (pixelsInBase base1 pos1) (pixelsInBase base2 pos2) 
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
    let touching (base1,pos1) (base2,pos2)  =
        let firstBase = shimmy (pixelsOfBase base1 pos1) |> Set.unionMany 
        let secondBase =  (pixelsInBase base2 pos2)
        Set.intersect firstBase secondBase


    // open Domain
    // open WarhammerDomain
    // open Domain.WarhammerDomain
    // open Domain.Tabletop
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

