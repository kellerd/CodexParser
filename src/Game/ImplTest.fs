namespace Impl

module ImplTest = 
    open Domain.WarhammerDomain
    open System
    
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
        let uguid = UnitGuid "155143C3-C1FD-46BE-B41C-02A992F16FB2"
        let mguid = ModelGuid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE"
        { Id = uguid
          UnitModels = 
              [ Termagant mguid ]
              |> List.map (fun m -> m.Id, m)
              |> Map.ofList
          UnitName = "Termagaunts"
          Rules = 
              seq { 
                  yield Function(UnitRule(UCharacteristic(WeaponSkill(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(BallisticSkill(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Strength(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Toughness(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Wounds(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Initiative(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Attacks(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Leadership(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(InvSaves(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Saves(CharacteristicValue 3)), uguid))
                  yield UserActivated
                                 (ActiveWhen
                                    (Logical
                                           (Rule(GameStateRule(GameRound(One(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Two(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Three(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Four(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Five(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Six(Movement)))), Or, 
                                            Rule(GameStateRule(GameRound(Seven(Movement)))))))))), (Function(UnitRule(Move 6.<inch>, uguid)))))
                  yield Function(UnitRule(DeploymentState(Start), uguid))
                  yield 
                                 (ActiveWhen
                                      (Logical
                                           (Rule(GameStateRule(GameRound(Begin))), And, 
                                            Rule(UnitRule(DeploymentState(Start), uguid))), 
                                        Function(UnitRule(Deploy, uguid))
                                        )
                                    )
                                    |> Rule.userActivated |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))
              }
              //                  yield "WeakenResolve", Nested(Description { Name = "Lurker"; Description = "Termagant Lurks when outside synapse" },
              //                                                 OnceUntil(GameStateApplication(EndPhase), Function(UnitApplication(SetCharacteristicUnit(Strength.ToString(),Characteristic(Strength(CharacteristicValue 1))),uguid))))
              |> Seq.map makeRule |> Map.ofSeq }
    
    //                      yield "Move", OnceUntil(GameStateApplication(EndPhase), ActiveWhen(GameStateApplication(MovementPhase),(Function(Move 6.<inch>)))
    //                  yield "Deploy", OnceUntil(GameStateApplication(EndPhase), ActiveWhen(GameStateApplication(MovementPhase),(Function(Deploy)))
    //                  yield "WeakenResolve", Nested(Description { Name = "Lurker"; Description = "Termagant Lurks when outside synapse" },
    //                                                Function(UnitApplication(SetCharacteristicUnit(Strength.ToString(),Characteristic(Strength(CharacteristicValue 1))),))
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
        let uguid = UnitGuid "A4F2493F-08BA-44EA-B813-0F3E5E53110B"
        { Id = uguid
          UnitModels = 
              [ Hormagaunt(ModelGuid "2D5045C8-0427-4C6D-B0A4-371F46DAF844") ]
              |> List.map (fun m -> m.Id, m)
              |> Map.ofList
          UnitName = "Hormagaunts"
          Rules = 
              seq { 
                  yield Function(UnitRule(UCharacteristic(WeaponSkill(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(BallisticSkill(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Strength(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Toughness(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Wounds(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Initiative(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Attacks(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Leadership(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(InvSaves(CharacteristicValue 3)), uguid))
                  yield Function(UnitRule(UCharacteristic(Saves(CharacteristicValue 3)), uguid))
                  yield UserActivated
                                 (ActiveWhen
                                      (Logical
                                           (Rule(GameStateRule(GameRound(One(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Two(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Three(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Four(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Five(Movement)))), Or, 
                                            Logical
                                           (Rule(GameStateRule(GameRound(Six(Movement)))), Or, 
                                            Rule(GameStateRule(GameRound(Seven(Movement)))))))))), (Function(UnitRule(Move 6.<inch>, uguid)))))
                  yield Function(UnitRule(DeploymentState(Start), uguid))
                  yield UserActivated
                                 (ActiveWhen
                                      (Logical
                                           (Rule(GameStateRule(GameRound(Begin))), And, 
                                            Rule(UnitRule(DeploymentState(Start), uguid))), 
                                       (Function(UnitRule(Deploy, uguid)))))
                  yield Description { Name = "Bounding Leap"; Description = "Run(CharacteristicValue 3) extra inches" }
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
