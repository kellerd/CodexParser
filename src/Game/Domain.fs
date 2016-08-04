namespace Domain
module WarhammerDomain =
    open System
    open Microsoft.FSharp.Reflection

    let toString (x:'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None


    type RuleDescription = {Name: string; Description: string}
    let flip f x y = f y x
    let createSeq min max = Seq.initInfinite ((*) (LanguagePrimitives.Int32WithMeasure 1) >> (+) min) |> Seq.takeWhile (flip (<=) max)

    [<Measure>] type ft
    [<Measure>] type deg
    [<Measure>] type dpi
    [<Measure>]
    type inch =
      static member ToPixels (resolution : float<dpi>)  (inches : float<inch>) =
        LanguagePrimitives.FloatWithMeasure<px> (float inches * float resolution)
      static member ToPixelsI (resolution : int<dpi>)  (inches : int<inch>) =
        LanguagePrimitives.Int32WithMeasure<px> (int inches * int resolution)
      static member perFootI = 12<inch/ft>;
      static member perFoot = 12.<inch/ft>;
    and
      [<Measure>]
      px =
        static member ToInches  (resolution : float<dpi>) (pixels : float<px>) =
          LanguagePrimitives.FloatWithMeasure<inch> (float pixels / float resolution)
    
    let characterResolution = 6.0<dpi>
    let ftToPx x = x * inch.perFootI |> inch.ToPixelsI (int characterResolution * 1<dpi>)

    [<Measure>]
    type mm =
        static member perInch = 25.4<mm/inch>
    type Position< [<Measure>] 'u > = { X : int<'u>; Y : int<'u> } with
      member this.FindDistance other  : int<'u>=
        let deltaX = float (other.X - this.X)
        let deltaY = float (other.Y - this.Y)
        sqrt ((deltaX * deltaX) + (deltaY * deltaY)) |> Math.Round |> (fun x -> (x |> int) * (LanguagePrimitives.Int32WithMeasure 1))

    type MaxMovement = MaxMovement of int<inch>
    type Dimentions<[<Measure>]'u> =
      {Width:float<'u>;Length:float<'u>} 

    type ModelGuid = Guid
    type UnitGuid = Guid

    type LogicalOperator = And | Or