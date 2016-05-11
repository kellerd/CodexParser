﻿
namespace Domain
module WarhammerDomain =
    open System
    open Microsoft.FSharp.Reflection

    let toString (x:'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString (s:string) =
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
        static member FromString s = fromString s
    
    type DiceRoll = DiceRoll of int
    type Phase = Begin | Movement | Psychic | Shooting | Assault | End
    type GameTurn = | Begin
                    | One    of Phase
                    | Two    of Phase
                    | Three  of Phase
                    | Four   of Phase
                    | Five   of Phase
                    | Six    of Phase
                    | Seven  of Phase
                    | End
    type PlayerTurn = Top of GameTurn | Bottom of GameTurn
    

    type Deployment = 
        | Deployed
        | Destroyed
        | OngoingReserves
        | Reserves
        | NotDeployed

    type RuleImpl = 
        | EndPhase
        | Move of float<inch>
        | Deploy
        | SetCharacteristicUnit of string * Rule
        member this.ToString = toString this
        static member FromString s = fromString<RuleImpl> s
    and Rule = 
        | Function of RuleImpl
        | Characteristic of Characteristic
        | Nested of Rule  * Rule
        | Overwritten of Rule  * Rule 
        | DeactivatedUntilEndOfPhaseOnFirstUse of Rule
        | DeactivatedUntilEndOfGameOnFirstUse of Rule
        | DeactivatedUntilEndOfPhase of Rule
        | DeactivatedUntilEndOfGame of Rule
        | Description of RuleDescription with
        static member CreateNested = (fun (newR:Rule) (x:Rule) -> Nested(newR,x)) 
    and Model = {
      Name : string;
      Id : Guid;
      Rules : Map<string,Rule>
      Base: Base
    } 
    
    and Unit = { 
      UnitModels  : Model list
      UnitName    : string
      Rules : Map<string,Rule>
      Deployment : Deployment
    } 

    type Player = Player1 | Player2

    let drawingResolution = 26.0<dpi>
    let characterResolution = 15.0<dpi>
    type Dimensions = {Width:int<ft>; Height:int<ft>}
    type Score = Score of int
    type GameState = {
        Board : BoardInfo
        Players : PlayerInfo list
        Game:GameInfo
        }
    and PlayerInfo = {
        Player: Player
        Units: Unit list
        Score: Score
    }
    and BoardInfo = {
        Models : ModelInfo list
        Dimensions : Dimensions
    }
    and ModelInfo = {
        Model : Model
        Player : Player
        Position: Position<px>
    }
    and GameInfo = {
        Turn : PlayerTurn
        Mission:Mission
    }
    and Mission = {
       MaxRounds:GameState->PlayerTurn
       Rules : Map<string,Rule>
       EndCondition:GameState->bool
    } 
    and Game = 
        | GameState of GameState
        | PositionAsker of ((GameState -> Position<px>) -> GameState)
        | MoveAsker of ((Position<px>[] -> Position<px>) -> GameState)
        | DiceRollAsker of ((unit -> DiceRoll) -> GameState)
    type UnitRule = {
        Unit: Unit 
        Rule: Rule 
        Capability: MoveCapability}
    and EndRule = {
        Rule: Rule 
        Capability: MoveCapability}
    and MoveCapability = 
        unit -> RuleResult
    and NextMoveInfo = 
        | UnitRule of UnitRule
        | EndRule of EndRule
    /// The result of a move. Do displayInfo later.
    and RuleResult = 
        | Player1ToMove of Game * NextMoveInfo list
        | Player2ToMove of Game * NextMoveInfo list
        | GameWon of GameState * Player 
        | GameTied of GameState 
    
    // Only the newGame function is exported from the implementation
    // all other functions come from the results of the previous move
    type WarhammerAPI  = 
        {
        NewGame : MoveCapability
        }