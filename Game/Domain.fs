namespace Domain

module WarhammerDomain =
    
    type RuleDescription = {Name: string; Description: string}
    type Rule<'a> = 
        | StaticRule of 'a
        | NestedRule of Rule<'a>
        | RuleDescription of RuleDescription

    type Player = Player1 | Player2
    type Characteristic = 
        | WeaponSkill      of int
        | BallisticSkill    of int
        | Strength          of int
        | Toughness        of int
        | Saves             of int
        | Attacks          of int
        | InvSaves         of int
        | Wounds            of int
    type ArmorPen = 
        | None
        | ArmorPen of int
        | InvulPen of int
    type Range = 
        | Melee
    type WeaponType = Heavy | RapidFire | Assault 
    [<Measure>] type inch;
    type Weapon = {
      weaponName         : string;
      weaponType         : WeaponType;
      weaponAttacks      : Rule<int>;
      weaponStrength     : Rule<int>;
      weaponArmorPen     : ArmorPen;
      weaponRange        : int<inch>;
      weaponIsTwinLinked : bool
    } 

    type Model = {
      Name              : string;
      Characeristics : Characteristic list;
    } 

    type Unit = {
      unitModels  : Model list;
      unitName    : string
    }
    
    type Move = unit->int * Rule
    type Run = unit->decimal Distribution * Rule

    type BoardInfo = {
        Board : Model list
        Player : Player list 
        }

    type MoveCapability = 
        unit -> RuleResultR

    /// A capability along with the position the capability is associated with.
    /// This allows the UI to show information so that the user
    /// can pick a particular capability to exercise.
    and NextMoveInfo = {
        // the pos is for UI information only
        // the actual pos is baked into the cap.
        posToPlay : Rule 
        capability : MoveCapability }

    /// The result of a move. It includes: 
    /// * The information on the current board state.
    /// * The capabilities for the next move, if any.
    and RuleResultR = 
        | Player1ToMove of BoardInfo * NextMoveInfo list 
        | Player2ToMove of BoardInfo * NextMoveInfo list 
        | GameWon of BoardInfo * Player 
        | GameTied of BoardInfo 
    
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

