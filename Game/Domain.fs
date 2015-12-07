namespace Domain

module WarhammerDomain =
    open Distribution.Distribution
    open FSharpx.State

    type Rule = { Name : string; Desc : string list }
    type Tree<'a> = | EmptyTree | Branch of 'a * Tree<'a> list
    type Player = Player1 | Player2
    type Probability = decimal 
    type Characteristic = int
    
    type WeaponSkill = Characteristic
    type BallisticSkill = Characteristic
    type Strength = Characteristic
    type Toughness = Characteristic
    type Saves = Characteristic
    type ArmorPen = Characteristic
    type Attacks = Characteristic
    type InvSaves = Characteristic
    type Wounds = Characteristic

    type WeaponType = Heavy | RapidFire | Assault 
    type Weapon = {
      weaponName         : string;
      weaponType         : WeaponType;
      weaponAttacks      : int;
      weaponStrength     : int;
      weaponArmorPen     : ArmorPen;
      weaponRange        : int;
      weaponIsTwinLinked : bool
    } 

    type Model = {
      WeaponSkill       : WeaponSkill;
      BallisticSkill    : BallisticSkill;
      Strength          : Strength;
      Toughness         : Toughness;
      Saves             : Saves;
      Wounds            : Wounds;
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
        unit -> RuleResult 

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
    and RuleResult = 
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

    let D sides = toUniformDistribution [1..sides]
    let D6 = D 6
    let FourD6 =  [D6; D6; D6; D6;]
    let trans = traverseDistributionA (fun x -> x |> Seq.map(fun y -> {y with Value = 2 * y.Value})) FourD6 
    let seqss = sequenceDistributionA FourD6 

    printf "%A" FourD6 |> ignore
    printf "%A" trans |> ignore
    printf "%A" seqss |> ignore
    let HitDice = FourD6 |> List.map average |> List.sum


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

