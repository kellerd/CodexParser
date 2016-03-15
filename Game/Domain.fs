#if !INTERACTIVE
namespace Domain
module WarhammerDomain =
#endif 
    open System
    type RuleDescription = {Name: string; Description: string}
    [<Measure>] type ft
    [<Measure>] type deg
    [<Measure>] type dpi
    [<Measure>]
    type inch =
      static member ToPixels (inches : float<inch>) (resolution : float<dpi>) =
        LanguagePrimitives.FloatWithMeasure<px> (float inches * float resolution)
      static member perFoot = 12.0<inch/ft>;
    and
      [<Measure>]
      px =
        static member ToInches (pixels : float<px>) (resolution : float<dpi>) =
          LanguagePrimitives.FloatWithMeasure<inch> (float pixels / float resolution)
    [<Measure>]
    type mm =
        static member perInch = 25.4<mm/inch>
    type Position< [<Measure>] 'u > = { X : float<'u>; Y : float<'u> } with
      member this.FindDistance other =
        let deltaX = other.X - this.X
        let deltaY = other.Y - this.Y
        sqrt ((deltaX * deltaX) + (deltaY * deltaY))
    type CharacteristicValue = CharacteristicValue of int
    type MaxMovement = MaxMovement of int<inch>
    type value =
        | Bool of bool
        | Characteristic
        | Double of double
        | String of string
        | Inch of int<inch>
        | Range of int<inch> * int<inch>
        | D6 of (unit->int)
        
    type expr = 
        | Literal of value
        | Function of invoke
        | List of value list
    and invoke =
        | Call of string * expr list 
        | Method of string * string * expr list
        | PropertyGet of string * string
    [<StructuralEquality>][<NoComparison>]
    type Rule = 
        | Single of Rule
        | Nested of Rule  * Rule 
        | Overwritten of Rule  * Rule 
        | OncePerPhase of Rule
        | OncePerGame of Rule
        | Description of RuleDescription

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
        | Saves           of CharacteristicValue

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

    type Phase = Begin | Movement | Psychic | Shooting | Assault | End
    type Round = Begin | One | Two | Three | Four | Five | Six | Seven | End
    

    let resolution = 26.0<dpi>
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
        Models : (Player*Model*Position<px>) list
        Dimensions : Dimensions
    }
    and GameInfo = {
        Phase : Phase
        Round : Round
        Mission:Mission
    }
    and Mission = {
       MaxRounds:GameState->Round
       Rules : Rule list
       EndCondition:GameState->bool
    }

    type MoveCapability = 
        unit -> RuleResult

    and NextMoveInfo = 
        | NextMoveInfo of Unit list 
    /// The result of a move. Do displayInfo later.
    and RuleResult = 
        | Player1ToMove of GameState * NextMoveInfo
        | Player2ToMove of GameState * NextMoveInfo
        | GameWon of GameState * Player 
        | GameTied of GameState 
    
    // Only the newGame function is exported from the implementation
    // all other functions come from the results of the previous move
    type WarhammerAPI  = 
        {
        newGame : MoveCapability
        }


module TickTacToeDomain =
    type HorizPosition = Left | HCenter | Right
    type VertPosition = Top | VCenter | Bottom
    type CellPosition = HorizPosition * VertPosition 

    type Player = PlayerO | PlayerX

    type CellState = 
        | Played of Player 
        | Empty

    type Cell = {
        pos : CellPosition 
        state : CellState 
        }

    /// Everything the UI needs to know to display the board
    type DisplayInfo = {
        cells : Cell list
        }
    
    /// The capability to make a move at a particular location.
    /// The gamestate, player and position are already "baked" into the function.
    type MoveCapability = 
        unit -> MoveResult 

    /// A capability along with the position the capability is associated with.
    /// This allows the UI to show information so that the user
    /// can pick a particular capability to exercise.
    and NextMoveInfo = {
        // the pos is for UI information only
        // the actual pos is baked into the cap.
        posToPlay : CellPosition 
        capability : MoveCapability }

    /// The result of a move. It includes: 
    /// * The information on the current board state.
    /// * The capabilities for the next move, if any.
    and MoveResult = 
        | PlayerXToMove of DisplayInfo * NextMoveInfo list 
        | PlayerOToMove of DisplayInfo * NextMoveInfo list 
        | GameWon of DisplayInfo * Player 
        | GameTied of DisplayInfo 

    // Only the newGame function is exported from the implementation
    // all other functions come from the results of the previous move
    type TicTacToeAPI  = 
        {
        newGame : MoveCapability
        }

