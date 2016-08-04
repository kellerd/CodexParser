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
    type CharacteristicValue = CharacteristicValue of int
    type MaxMovement = MaxMovement of int<inch>
    type Dimentions<[<Measure>]'u> =
      {Width:float<'u>;Length:float<'u>} 
    type Base =
     | BaseDiameter of int<mm>
     | ModelDimentions of Dimentions<mm>
    type Characteristic = 
        | WeaponSkill     of CharacteristicValue
        | BallisticSkill  of CharacteristicValue
        | Strength        of CharacteristicValue
        | Toughness       of CharacteristicValue
        | Wounds          of CharacteristicValue
        | Initiative      of CharacteristicValue
        | Attacks         of CharacteristicValue
        | Leadership      of CharacteristicValue
        | InvSaves        of CharacteristicValue
        | Saves           of CharacteristicValue with
        member this.ToString = toString this
        static member FromString s = fromString<Characteristic> s
    
    type DiceRoll = DiceRoll of int
    type Phase = Begin | Movement | Psychic | Shooting | Assault | End
    type ModelGuid = Guid
    type UnitGuid = Guid
    
    type Round = 
        | Begin
        | One    of Phase
        | Two    of Phase
        | Three  of Phase
        | Four   of Phase
        | Five   of Phase
        | Six    of Phase
        | Seven  of Phase
        | End
    type PlayerTurn = Top | Bottom 
    

    type DeploymentType = 
        | Destroyed
        | OngoingReserves
        | Reserves
        | Deployed
        | Start
    type LogicalOperator = And | Or


    type ModelRuleImpl = 
        | MCharacteristic of Characteristic
        override this.ToString() = toString this
        static member FromString s = fromString<ModelRuleImpl> s
    and UnitRuleImpl = 
        | Move of float<inch>
        | DeploymentState of DeploymentType
        | Deploy
        | UCharacteristic of Characteristic
        | SetCharacteristicUnit of string * Rule
        override  this.ToString() = toString this
        static member FromString s = fromString<UnitRuleImpl> s
    and GameRuleImpl = 
        | Noop
        | EndPhase
        | EndTurn
        | EndGame
        | PlayerTurn of PlayerTurn
        | GameRound of Round
        // | Deactivate of RuleApplication
        | DeactivateUntil of LogicalExpression * RuleApplication
        | Revert of RuleApplication
        override this.ToString() = toString this
        static member FromString s = fromString<GameRuleImpl> s
    and LogicalExpression =
        | Logical of LogicalExpression * LogicalOperator * LogicalExpression
        | Not of LogicalExpression
        | Rule of RuleApplication
    and RuleApplication = 
        | UnitRule of UnitRuleImpl * UnitGuid
        | ModelRule of ModelRuleImpl * ModelGuid
        | GameStateRule of GameRuleImpl
        | Sequence of RuleApplication list
    and Rule = 
        | Function of RuleApplication
        | UserActivated of Rule
        | ActiveWhen of LogicalExpression * Rule
        | Description of RuleDescription 
        | Overwritten of Rule * Rule 

    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    [<AutoOpen>]
    module Rule =
        let overwrite newR x = Overwritten(newR,x)
        let unoverwrite = function
                                | Overwritten(newR,r) -> r
                                | r -> r
        let userActivated rule =
            let rec userActivated' = function
                    | ActiveWhen(logic,r) -> 
                        ActiveWhen(logic,userActivated' r)
                    | UserActivated(_) as rule -> rule
                    | Description(_) as rule-> rule
                    | Function(Sequence(r1::tail)) -> Function(Sequence(r1::GameStateRule(Revert(r1))::tail))
                    | Function(Sequence([])) as rule -> rule
                    | Function(r) -> Function(Sequence([r])) |> userActivated'
                    | Overwritten(newRule,old) -> Overwritten(userActivated' newRule,old)
            userActivated' rule |> UserActivated
        let rec afterRunDeactivateUntil activatedWhen = function
                | ActiveWhen(logic,rule) -> 
                    ActiveWhen(logic,afterRunDeactivateUntil activatedWhen rule)
                | UserActivated(rule)   -> afterRunDeactivateUntil activatedWhen rule |> UserActivated
                | Description(_) as rule-> rule
                | Function(Sequence(r1::tail)) -> 
                    Function(Sequence(r1 :: (List.append tail [GameStateRule(DeactivateUntil(activatedWhen,r1))])))
                | Function(Sequence([])) as rule -> rule
                | Function(r) -> Function(Sequence([r])) |> afterRunDeactivateUntil activatedWhen
                | Overwritten(newRule,old) -> Overwritten(afterRunDeactivateUntil activatedWhen newRule,old)
        let onlyWhen l1 r1 = ActiveWhen(l1,r1)
        let (<&>) l1 l2 = Logical(l1,And,l2)
        let (<|>) l1 l2 = Logical(l1,And,l2)
        let (<!>) l1 = Not(l1)
        
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