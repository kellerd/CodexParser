namespace Domain
open WarhammerDomain
open Domain.Tabletop          
type GenAsker<'a,'b> = Asker of ('a -> 'b)
    with static member Run (a:GenAsker<'a,'b>, input:'a) =  let (Asker asker') = a 
                                                            asker' input
type Asker<'a> = 
    | PositionAsker of GenAsker<GameState -> Position<px>, 'a>
    | MoveAsker of GenAsker<Position<px>[] -> Position<px>, 'a>
    | DiceRollAsker of GenAsker<unit -> GameRuleImpl, 'a>
    | SortedWoundPoolAsker of GenAsker<(int * WeaponProfile) list -> int list, 'a>
    | PerformAsker of GenAsker<(Rule) list->int, 'a>
    with member this.Map(f) = 
            match this with
                | PositionAsker(Asker(a)) -> a >> f |> Asker |> PositionAsker
                | MoveAsker(Asker(a)) ->  a >> f |> Asker |> MoveAsker
                | DiceRollAsker(Asker(a)) ->  a >> f |> Asker |> DiceRollAsker
                | SortedWoundPoolAsker(Asker(a)) ->  a >> f |> Asker |> SortedWoundPoolAsker
                | PerformAsker(Asker(a)) -> a >> f |> Asker |> PerformAsker