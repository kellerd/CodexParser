namespace Impl

module ImplTest = 
    open Domain.WarhammerDomain
    open System
    
    let Termagant id = 
        { Name = "Termagant"
          Id = id
          Base = BaseDiameter 25<mm>
          Rules = 
              seq { 
                  yield "Lurker", 
                        Description { Name = "Lurker"
                                      Description = "Termagant Lurks when outside synapse" }
              }
              |> Map.ofSeq }
    
    let TermUnit = 
        { Id = Guid "155143C3-C1FD-46BE-B41C-02A992F16FB2" 
          UnitModels = [ Termagant(Guid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE") ] |> List.map(fun m -> m.Id, m) |> Map.ofList
          UnitName = "Termagaunts"
          Rules = 
              seq { 
                  yield WeaponSkill.ToString(), Characteristic(WeaponSkill(CharacteristicValue 3))
                  yield BallisticSkill.ToString(), Characteristic(BallisticSkill(CharacteristicValue 3))
                  yield Strength.ToString(), Characteristic(Strength(CharacteristicValue 3))
                  yield Toughness.ToString(), Characteristic(Toughness(CharacteristicValue 3))
                  yield Wounds.ToString(), Characteristic(Wounds(CharacteristicValue 3))
                  yield Initiative.ToString(), Characteristic(Initiative(CharacteristicValue 3))
                  yield Attacks.ToString(), Characteristic(Attacks(CharacteristicValue 3))
                  yield Leadership.ToString(), Characteristic(Leadership(CharacteristicValue 3))
                  yield InvSaves.ToString(), Characteristic(InvSaves(CharacteristicValue 3))
                  yield Saves.ToString(), Characteristic(Saves(CharacteristicValue 3))
                  yield "Move", DeactivatedUntilEndOfPhaseOnFirstUse(Function(Move 6.<inch>))
                  yield "Deploy", DeactivatedUntilEndOfPhaseOnFirstUse(Function(Deploy))
                  yield "WeakenResolve", Nested(Description { Name = "Lurker"; Description = "Termagant Lurks when outside synapse" },
                                                Function(SetCharacteristicUnit(Strength.ToString(),Characteristic(Strength(CharacteristicValue 1)))))
              }
              |> Map.ofSeq
          Deployment = NotDeployed }
    
    let Hormagaunt id = 
        { Name = "Hormagaunt"
          Id = id
          Base = BaseDiameter 25<mm>
          Rules = 
              seq { 
                  yield "Lurker", 
                        Description { Name = "Feeder"
                                      Description = "Hormagaunt gets rage when outside synapse" }
              }
              |> Map.ofSeq }
    
    let HormagauntUnit = 
        { Id = Guid "A4F2493F-08BA-44EA-B813-0F3E5E53110B"
          UnitModels = [ Hormagaunt(Guid "2D5045C8-0427-4C6D-B0A4-371F46DAF844") ] |> List.map(fun m -> m.Id, m) |> Map.ofList
          UnitName = "Hormagaunts"
          Rules = 
              seq { 
                  yield WeaponSkill.ToString(), Characteristic(WeaponSkill(CharacteristicValue 3))
                  yield BallisticSkill.ToString(), Characteristic(BallisticSkill(CharacteristicValue 3))
                  yield Strength.ToString(), Characteristic(Strength(CharacteristicValue 3))
                  yield Toughness.ToString(), Characteristic(Toughness(CharacteristicValue 3))
                  yield Wounds.ToString(), Characteristic(Wounds(CharacteristicValue 3))
                  yield Initiative.ToString(), Characteristic(Initiative(CharacteristicValue 3))
                  yield Attacks.ToString(), Characteristic(Attacks(CharacteristicValue 3))
                  yield Leadership.ToString(), Characteristic(Leadership(CharacteristicValue 3))
                  yield InvSaves.ToString(), Characteristic(InvSaves(CharacteristicValue 3))
                  yield Saves.ToString(), Characteristic(Saves(CharacteristicValue 3))
                  yield "Bounding Leap", 
                        Description { Name = "Bounding Leap"
                                      Description = "Run(CharacteristicValue 3) extra inches" }
                  yield "Move", DeactivatedUntilEndOfPhaseOnFirstUse(Function(Move 6.<inch>))
                  yield "Deploy", DeactivatedUntilEndOfPhaseOnFirstUse(Function(Deploy))
              }
              |> Map.ofSeq
          Deployment = NotDeployed }
    
    let initial = 
        { Board = 
              { Models = Map.empty<Guid,ModelInfo>
                Dimensions = 
                    { Width = 6<ft>
                      Height = 4<ft> } }
          Players = 
              [ { Player = Player1
                  Units = [ TermUnit ] |> List.map (fun u -> u.Id, u) |> Map.ofList
                  Score = Score 0 }
                { Player = Player2
                  Units = [ HormagauntUnit ] |> List.map (fun u -> u.Id, u) |> Map.ofList
                  Score = Score 0 } ]
          Game = 
              { Turn = Top GameTurn.Begin
                Mission = 
                    { MaxRounds = (fun gs -> Bottom(Six(Phase.End)))
                      Rules = Map.empty<string, Rule>
                      EndCondition = (fun gs -> gs.Game.Turn = Bottom(GameTurn.End)) } } }
