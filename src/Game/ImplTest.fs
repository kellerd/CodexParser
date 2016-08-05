namespace Impl

module ImplTest = 
    open Domain.WarhammerDomain
    open Domain
    open Domain.Board
    open System
    let hUnitId = UnitGuid "A4F2493F-08BA-44EA-B813-0F3E5E53110B"  
    let tUnitId = UnitGuid "155143C3-C1FD-46BE-B41C-02A992F16FB2"
          
    let makeRule r = 
        let rec makeApp = 
            function 
            | GameStateRule impl -> impl.ToString()
            | ModelRule(impl, _) -> impl.ToString()
            | Sequence(rs) -> makeApp (rs |> Seq.head)
            | UnitRule(impl, _) -> impl.ToString()
        
        let rec makeText = 
            function 
            | Function(impl) -> makeApp impl
            | UserActivated(r) -> makeText r
            | ActiveWhen(_, r) -> makeText r
            | Description(d) -> d.Name
            | Overwritten(_, r) -> makeText r
        
        makeText r, r

    let isSpecificPhase phase = 
        Rule(GameStateRule(GameRound(One(phase)))) <|>
            Rule(GameStateRule(GameRound(Two(phase)))) <|>
            Rule(GameStateRule(GameRound(Three(phase)))) <|>
            Rule(GameStateRule(GameRound(Four(phase)))) <|>
            Rule(GameStateRule(GameRound(Five(phase)))) <|>
            Rule(GameStateRule(GameRound(Six(phase)))) <|>
            Rule(GameStateRule(GameRound(Seven(phase))))   
    let Termagant id = 
        { Name = "Termagant"
          Id = id
          Base = BaseDiameter 25<mm>
          Rules = 
              seq { 
                  yield Description { Name = "Lurker"
                                      Description = "Termagant Lurks when outside synapse" }
                  yield Function(ModelRule(Melee(1,3,tUnitId),id))
                        |> Rule.onlyWhen (isSpecificPhase Assault)
                        |> Rule.userActivated
                        |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))
              }
              |> Seq.map makeRule
              |> Map.ofSeq }
    
    let TermUnit = 
        let mguid = ModelGuid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE"
        { Id = tUnitId
          UnitModels = 
              [ Termagant mguid ]
              |> List.map (fun m -> m.Id, m)
              |> Map.ofList
          UnitName = "Termagaunts"
          Rules = 
              seq { 
                  yield Function(UnitRule(UCharacteristic(WeaponSkill(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(UCharacteristic(BallisticSkill(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(UCharacteristic(Strength(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(UCharacteristic(Toughness(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(UCharacteristic(Wounds(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(UCharacteristic(Initiative(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(UCharacteristic(Attacks(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(UCharacteristic(Leadership(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(UCharacteristic(InvSaves(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(UCharacteristic(Saves(CharacteristicValue 3)), tUnitId))
                  yield Function(UnitRule(Move 6.<inch>, tUnitId))
                        |> Rule.onlyWhen (isSpecificPhase Movement)
                        |> Rule.userActivated
                        |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))
                  yield Function(UnitRule(DeploymentState(Start), tUnitId))
                  yield Function(UnitRule(Deploy, tUnitId))
                        |> Rule.onlyWhen (Rule(GameStateRule(GameRound(Begin))) <&> Rule(UnitRule(DeploymentState(Start), tUnitId))) 
                        |> Rule.userActivated
                        |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))
              }
              |> Seq.map makeRule
              |> Map.ofSeq }

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
        
        { Id = hUnitId
          UnitModels = 
              [ Hormagaunt(ModelGuid "2D5045C8-0427-4C6D-B0A4-371F46DAF844") ]
              |> List.map (fun m -> m.Id, m)
              |> Map.ofList
          UnitName = "Hormagaunts"
          Rules = 
              seq { 
                  yield Function(UnitRule(UCharacteristic(WeaponSkill(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(UCharacteristic(BallisticSkill(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(UCharacteristic(Strength(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(UCharacteristic(Toughness(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(UCharacteristic(Wounds(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(UCharacteristic(Initiative(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(UCharacteristic(Attacks(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(UCharacteristic(Leadership(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(UCharacteristic(InvSaves(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(UCharacteristic(Saves(CharacteristicValue 3)), hUnitId))
                  yield Function(UnitRule(Move 6.<inch>, hUnitId))
                        |> Rule.onlyWhen (Rule(GameStateRule(GameRound(One(Movement)))) <|>
                                            Rule(GameStateRule(GameRound(Two(Movement)))) <|>
                                            Rule(GameStateRule(GameRound(Three(Movement)))) <|>
                                            Rule(GameStateRule(GameRound(Four(Movement)))) <|>
                                            Rule(GameStateRule(GameRound(Five(Movement)))) <|>
                                            Rule(GameStateRule(GameRound(Six(Movement)))) <|>
                                            Rule(GameStateRule(GameRound(Seven(Movement)))))
                        |> Rule.userActivated
                        |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))
                  yield Function(UnitRule(DeploymentState(Start), hUnitId))
                  yield Function(UnitRule(Deploy, hUnitId))
                        |> Rule.onlyWhen (Rule(GameStateRule(GameRound(Begin))) <&> Rule(UnitRule(DeploymentState(Start), hUnitId))) 
                        |> Rule.userActivated
                        |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))
                  yield Description { Name = "Bounding Leap"
                                      Description = "Run(CharacteristicValue 3) extra inches" }
              }
              |> Seq.map makeRule
              |> Map.ofSeq }
    
    let initial = 
        { Board = 
              { Models = Map.empty<Guid, ModelInfo>
                Dimensions = 
                    { Width = 6<ft>
                      Height = 4<ft> } }
          Players = 
              [ { Player = Player1
                  Units = 
                      [ TermUnit ]
                      |> List.map (fun u -> u.Id, u)
                      |> Map.ofList
                  Score = Score 0 }
                { Player = Player2
                  Units = 
                      [ HormagauntUnit ]
                      |> List.map (fun u -> u.Id, u)
                      |> Map.ofList
                  Score = Score 0 } ]
          Rules = 
              seq { 
                  yield Function(GameStateRule(PlayerTurn(Top)))
                  yield Function(GameStateRule(GameRound(Begin)))
              }
              |> Seq.map makeRule
              |> Map.ofSeq
          Game = { Mission = () } }
