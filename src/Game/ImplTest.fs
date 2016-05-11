﻿namespace Impl

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
        { UnitModels = [ Termagant(Guid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE") ]
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
        { UnitModels = [ Hormagaunt(Guid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE") ]
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
                      Rules = Map.empty<string, Rule>
                      EndCondition = (fun gs -> gs.Game.Turn = Bottom(GameTurn.End)) } } }
