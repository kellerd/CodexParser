
namespace Domain
module WarhammerDomain =
    open System
    type RuleDescription = {Name: string; Description: string}
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
    type Value =
        | Bool of bool
        | Characteristic of CharacteristicValue
        | Double of double
        | String of string
        | Inch of int<inch>
        | Range of int<inch> * int<inch>
    and     Characteristic = 
        | WeaponSkill     of CharacteristicValue
        | BallisticSkill  of CharacteristicValue
        | Strength        of CharacteristicValue
        | Toughness       of CharacteristicValue
        | Wounds          of CharacteristicValue
        | Initiative      of CharacteristicValue
        | Attacks         of CharacteristicValue
        | Leadership      of CharacteristicValue
        | InvSaves        of CharacteristicValue
        | Saves           of CharacteristicValue
    

    type Phase = Begin | Movement | Psychic | Shooting | Assault | End
    type GameTurn = Begin | One | Two | Three | Four | Five | Six | Seven | End
    type PlayerTurn = Top of GameTurn | Bottom of GameTurn

    type RuleImpl = 
        | EndPhase
        | Move
    type Expr = 
        | Literal of Value
        | Function of RuleImpl
        | List of Value list
//    and invoke =
//        | Call of RuleImpl // * expr list 
//        | Method of string * string * expr list
//        | PropertyGet of string * string
    type Rule = 
        | Rule of Expr
        | Nested of Rule  * Rule 
        | Overwritten of Rule  * Rule 
        | DeactivatedUntilEndOfPhase of Rule
        | DeactivatedUntilEndOfGame of Rule
        | Description of RuleDescription

    

    type Deployment = 
        | Deployed of float<inch> * float<inch>
        | Destroyed
        | OngoingReserves
        | Reserves
        | NotDeployed

    
    type Player = Player1 | Player2

    
    type Dimentions<[<Measure>]'u> =
      {Width:float<'u>;Length:float<'u>} 
    type Base =
     | BaseDiameter of int<mm>
     | ModelDimentions of Dimentions<mm>
    type Model = {
      Name : string;
      Id : Guid;
      Characteristic : Map<string,Characteristic>;
      Rules : Rule list
      Base: Base
    } 
    

    type Unit = { 
      UnitModels  : Model list
      UnitName    : string
      Rules : Rule list
      Deployment : Deployment
    } 
    

    let drawingResolution = 26.0<dpi>
    let characterResolution = 9.0<dpi>
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
        Phase : Phase
        Turn : PlayerTurn
        Mission:Mission
    }
    and Mission = {
       MaxRounds:GameState->PlayerTurn
       Rules : Rule list
       EndCondition:GameState->bool
    }
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
        | Player1ToMove of GameState * NextMoveInfo list
        | Player2ToMove of GameState * NextMoveInfo list
        | GameWon of GameState * Player 
        | GameTied of GameState 
    
    // Only the newGame function is exported from the implementation
    // all other functions come from the results of the previous move
    type WarhammerAPI  = 
        {
        NewGame : MoveCapability
        }