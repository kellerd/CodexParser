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

    let inchTomm x : float<mm>  = x / (1./mm.perInch)
    let mmToInch x : float<inch> = x / mm.perInch

    type MaxMovement = MaxMovement of int<inch>
    type Dimentions<[<Measure>]'u> =
      {Width:float<'u>;Length:float<'u>} 

    type ModelGuid = Guid
    type UnitGuid = Guid

    type LogicalOperator = And | Or

    let roundToPixels x = ((inch.ToPixels characterResolution x / 1.<px> |> System.Math.Round |> int) * 1<px>)
    let pixelsInCircle radius position =  
        let radius' = roundToPixels radius
        seq {
        for x in createSeq (position.X - radius') (position.X + radius') do
            for y in createSeq (position.Y - radius') (position.Y + radius') do
                let newPos = {X=x;Y=y}
                if x > 0<px> && y > 0<px> && position.FindDistance newPos <= radius' then
                    yield newPos
    }    
    let pixelsOfCircle radius position =
        let radius' = roundToPixels radius
        //(x - position.X)^2 + (y - position.Y) ^2 = radius ^2
        // (y - position.Y)(y - position.Y)
        //y^2 - 2y(position.Y) = radius ^2 - (x - position.X)^2  - position.Y ^ 2 
        let y1 h k r x = (2. * k + Math.Sqrt(8. * h * x - 4. * h * h + 4. * r * r - r * x * x |> float)|>float) / 2.
        
        let y1' h k r x =y1 (float h) (float k) (float r) (float x) 
        let y2' h k r x = y1' h (float k * -1.) r x |> (*) 1<px>
        seq {
           for x in createSeq (-1 * radius') radius' do
               yield y1' position.X position.Y radius' x  
               yield y2' position.X position.Y radius' x  
        }
        (y1,y2)
    let pixelsInRectangle widthmax heightmax start =
        let width' = roundToPixels widthmax
        let height' = roundToPixels heightmax
        seq {
            for x in createSeq start.X width' do
                yield {X=x;Y=start.Y}
                yield {X=x;Y=height'}
            for y in createSeq start.Y height' do
                yield {X=start.X;Y=y}
                yield {X=width'.X;Y=y}
        } |> Seq.distinct
