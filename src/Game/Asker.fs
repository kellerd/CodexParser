namespace Domain
open WarhammerDomain
open Board          
type Asker<'a> = 
    | PositionAsker of GenAsker<GameState -> Position<px>, 'a>
    | MoveAsker of GenAsker<Position<px>[] -> Position<px>, 'a>
    | DiceRollAsker of GenAsker<unit -> DiceRoll, 'a>
    with member this.Map(f) = 
            match this with
                | PositionAsker(Asker(a)) -> a >> f |> Asker |> PositionAsker
                | MoveAsker(Asker(a)) ->  a >> f |> Asker |> MoveAsker
                | DiceRollAsker(Asker(a)) ->  a >> f |> Asker |> DiceRollAsker
