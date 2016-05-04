namespace Impl

module ImplTest = 
    open Domain.WarhammerDomain
    open System
    
    let Termagant id = 
        { Name = "Termagant"
          Id = id
          Characteristic = 
              [ WeaponSkill(CharacteristicValue 3)
                BallisticSkill(CharacteristicValue 3)
                Strength(CharacteristicValue 3)
                Toughness(CharacteristicValue 3)
                Wounds(CharacteristicValue 3)
                Initiative(CharacteristicValue 3)
                Attacks(CharacteristicValue 3)
                Leadership(CharacteristicValue 3)
                InvSaves(CharacteristicValue 3)
                Saves(CharacteristicValue 3) ]
              |> List.map (fun x -> x.GetType().Name, x)
              |> Map.ofList
          Base = BaseDiameter 25<mm>
          Rules = 
              [ Description { Name = "Lurker"
                              Description = "Termagant Lurks when outside synapse" } ] }
    
    let TermUnit = 
        { UnitModels = [ Termagant(Guid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE") ]
          UnitName = "Termagaunts"
          Rules = 
              [ DeactivatedUntilEndOfPhaseOnFirstUse(Rule(Function(Move 6.<inch>)));
                DeactivatedUntilEndOfPhaseOnFirstUse(Rule(Function(Deploy))) ]
          Deployment = NotDeployed }
    
    let Hormagaunt id = 
        { Name = "Hormagaunt"
          Id = id
          Characteristic = 
              [ WeaponSkill(CharacteristicValue 3)
                BallisticSkill(CharacteristicValue 3)
                Strength(CharacteristicValue 3)
                Toughness(CharacteristicValue 3)
                Wounds(CharacteristicValue 3)
                Initiative(CharacteristicValue 3)
                Attacks(CharacteristicValue 3)
                Leadership(CharacteristicValue 3)
                InvSaves(CharacteristicValue 3)
                Saves(CharacteristicValue 3) ]
              |> List.map (fun x -> x.GetType().Name, x)
              |> Map.ofList
          Base = BaseDiameter 25<mm>
          Rules = 
              [ Description { Name = "Lurker"
                              Description = "Termagant Lurks when outside synapse" } ] }
    
    let HormagauntUnit = 
        { UnitModels = [ Hormagaunt(Guid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE") ]
          UnitName = "Hormagaunts"
          Rules = 
              [ Description { Name = "Bounding Leap"
                              Description = "Run 3 extra inches" };
                DeactivatedUntilEndOfPhaseOnFirstUse(Rule(Function(Move 6.<inch>)));
                DeactivatedUntilEndOfPhaseOnFirstUse(Rule(Function(Deploy))) ]
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
