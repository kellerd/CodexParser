namespace Impl

module ImplTest = 
    open Domain.WarhammerDomain
    open System
    
    let Termagant id = 
        { Name = "Termagant"
          Id = id
          Base = BaseDiameter 25<mm>
          Rules = 
              [ Description { Name = "Lurker"
                              Description = "Termagant Lurks when outside synapse" } 
                Value(WeaponSkill(CharacteristicValue 3) |> box)  
                Value(BallisticSkill(CharacteristicValue 3) |> box) 
                Value(Strength(CharacteristicValue 3) |> box      ) 
                Value(Toughness(CharacteristicValue 3) |> box     ) 
                Value(Wounds(CharacteristicValue 3) |> box        ) 
                Value(Initiative(CharacteristicValue 3) |> box    ) 
                Value(Attacks(CharacteristicValue 3) |> box       ) 
                Value(Leadership(CharacteristicValue 3) |> box    ) 
                Value(InvSaves(CharacteristicValue 3) |> box      ) 
                Value(Saves(CharacteristicValue 3) |> box)
                ]};
    
    let TermUnit = 
        { UnitModels = [ Termagant(Guid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE") ]
          UnitName = "Termagaunts"
          Rules = 
              [ DeactivatedUntilEndOfPhaseOnFirstUse(Function(Move 6.<inch>));
                DeactivatedUntilEndOfPhaseOnFirstUse(Function(Deploy)) ]
          Deployment = NotDeployed }
    
    let Hormagaunt id = 
        { Name = "Hormagaunt"
          Id = id
          Base = BaseDiameter 25<mm>
          Rules = 
              [ Description { Name = "Lurker"
                              Description = "Termagant Lurks when outside synapse" }
                Value(WeaponSkill(CharacteristicValue 3) |> box)    
                Value(BallisticSkill(CharacteristicValue 3) |> box) 
                Value(Strength(CharacteristicValue 3) |> box      ) 
                Value(Toughness(CharacteristicValue 3) |> box     ) 
                Value(Wounds(CharacteristicValue 3) |> box        ) 
                Value(Initiative(CharacteristicValue 3) |> box    ) 
                Value(Attacks(CharacteristicValue 3) |> box       ) 
                Value(Leadership(CharacteristicValue 3) |> box    ) 
                Value(InvSaves(CharacteristicValue 3) |> box      ) 
                Value(Saves(CharacteristicValue 3) |> box) ]}
    
    let HormagauntUnit = 
        { UnitModels = [ Hormagaunt(Guid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE") ]
          UnitName = "Hormagaunts"
          Rules = 
              [ Description { Name = "Bounding Leap"
                              Description = "Run(CharacteristicValue 3) extra inches" };
                DeactivatedUntilEndOfPhaseOnFirstUse(Function(Move 6.<inch>));
                DeactivatedUntilEndOfPhaseOnFirstUse(Function(Deploy)) ]
          Deployment = NotDeployed }
    
    let initial = 
        { Board = 
              { Models = []
                Dimensions = 
                    { Width = 6<ft>
                      Height = 4<ft> } }
          Players = 
              [ { Player = Player1
                  Units = [ TermUnit ]
                  Score = Score 0 }
                { Player = Player2
                  Units = [ HormagauntUnit ]
                  Score = Score 0 } ]
          Game = 
              { Turn = Top GameTurn.Begin
                Mission = 
                    { MaxRounds = (fun gs -> Bottom(Six(Phase.End)))
                      Rules = []
                      EndCondition = (fun gs -> gs.Game.Turn = Bottom(GameTurn.End)) } } }
