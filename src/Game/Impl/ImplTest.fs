namespace Impl

module ImplTest = 
    open Domain.WarhammerDomain
    open Domain
    open Domain.Tabletop
    let hUnitId = UnitGuid "A4F2493F-08BA-44EA-B813-0F3E5E53110B"  
    let tUnitId = UnitGuid "155143C3-C1FD-46BE-B41C-02A992F16FB2"
          

    
    let private is turn = (Literal(Equation(Apply(NotApplied "TRound"),Eq,Evaluation(GameStateRule(GameRound(turn))))))
    let private endPhase = (Literal(Equation(Apply(NotApplied "TRound"),Eq,Evaluation(GameStateRule(EndPhase)))))
    let isSpecificPhase phase = 
        is (One(phase)) <|>
        is (Two(phase)) <|>
        is (Three(phase)) <|>
        is (Four(phase)) <|>
        is (Five(phase)) <|>
        is (Six(phase)) <|>
        is (Seven(phase))   
    let Termagant id = 
        { Name = "Termagant"
          Id = id
          Base = BaseDiameter 25<mm>
          Rules = 
              seq { 
                  yield Description { Name = "Lurker"
                                      Description = "Termagant Lurks when outside synapse" }
                  yield Function(ModelRule(Melee(1,Applied hUnitId),id))
                        |> Rule.onlyWhen (isSpecificPhase Assault)
                        |> Rule.userActivated Player1
                        |> Rule.afterRunDeactivateUntil endPhase (ModelList id) 
                  yield Function(ModelRule(WeaponSkill(CharacteristicValue 3), id))
                  yield Function(ModelRule(BallisticSkill(CharacteristicValue 3), id))
                  yield Function(ModelRule(Strength(CharacteristicValue 3), id))
                  yield Function(ModelRule(Toughness(CharacteristicValue 3), id))
                  yield Function(ModelRule(Wounds(CharacteristicValue 3), id))
                  yield Function(ModelRule(Initiative(CharacteristicValue 3), id))
                  yield Function(ModelRule(Attacks(CharacteristicValue 3), id))
                  yield Function(ModelRule(Leadership(CharacteristicValue 3), id))
                  yield Function(ModelRule(InvSaves(CharacteristicValue 3), id))
                  yield Function(ModelRule(Saves(CharacteristicValue 3), id))
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
                  
//                  yield Function(UnitRule(Move 6.<inch>, tUnitId))
//                        |> Rule.onlyWhen (isSpecificPhase Movement)
//                        |> Rule.userActivated
//                        |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))
                  yield Function(UnitRule(DeploymentState(Start), tUnitId))
                  yield Function(UnitRule(Deploy, tUnitId))
                         |> Rule.onlyWhen (is (Begin) <&>  (Literal(Equation(Apply(NotApplied ("TRound", UnitList tUnitId)),Eq,(Evaluation(UnitRule(DeploymentState(Start), tUnitId)))))))
                         |> Rule.userActivated Player1
                         |> Rule.afterRunDeactivateUntil endPhase (UnitList tUnitId) 
              }
              |> Seq.map makeRule
              |> Map.ofSeq }

    let Hormagaunt id = 
        { Name = "Hormagaunt"
          Id = id
          Base = BaseDiameter 25<mm>
          Rules = 
              seq { 
                  yield Description { Name = "Feeder"
                                      Description = "Hormagaunt gets rage when outside synapse" }
                  yield Function(ModelRule(WeaponSkill(CharacteristicValue 3), id))
                  yield Function(ModelRule(BallisticSkill(CharacteristicValue 3), id))
                  yield Function(ModelRule(Strength(CharacteristicValue 3), id))
                  yield Function(ModelRule(Toughness(CharacteristicValue 3), id))
                  yield Function(ModelRule(Wounds(CharacteristicValue 3), id))
                  yield Function(ModelRule(Initiative(CharacteristicValue 3), id))
                  yield Function(ModelRule(Attacks(CharacteristicValue 3), id))
                  yield Function(ModelRule(Leadership(CharacteristicValue 3), id))
                  yield Function(ModelRule(InvSaves(CharacteristicValue 3), id))
                  yield Function(ModelRule(Saves(CharacteristicValue 3), id))
              }
              |> Seq.map makeRule
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
                //   yield Function(UnitRule(Move 6.<inch>, hUnitId))
                //         |> Rule.onlyWhen (Rule(GameStateRule(GameRound(One(Movement)))) <|>
                //                             Rule(GameStateRule(GameRound(Two(Movement)))) <|>
                //                             Rule(GameStateRule(GameRound(Three(Movement)))) <|>
                //                             Rule(GameStateRule(GameRound(Four(Movement)))) <|>
                //                             Rule(GameStateRule(GameRound(Five(Movement)))) <|>
                //                             Rule(GameStateRule(GameRound(Six(Movement)))) <|>
                //                             Rule(GameStateRule(GameRound(Seven(Movement)))))
                //         |> Rule.userActivated
                //         |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))
                  yield Function(UnitRule(DeploymentState(Start), hUnitId))
                  yield Function(UnitRule(Deploy, hUnitId))
                         |> Rule.onlyWhen (is (Begin) <&>  (Literal(Equation(Apply(NotApplied ("TRound", UnitList tUnitId)),Eq,(Evaluation(UnitRule(DeploymentState(Start), tUnitId)))))))
                         |> Rule.userActivated Player2
                         |> Rule.afterRunDeactivateUntil endPhase (UnitList hUnitId)
                  yield Description { Name = "Bounding Leap"
                                      Description = "Run(CharacteristicValue 3) extra inches" }
              }
              |> Seq.map makeRule
              |> Map.ofSeq }
    
    let initial = 
        { 
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
                  yield Function(GameStateRule(Applications(Map.empty)))
                  yield Function(GameStateRule(EndPhase)) |> Rule.UserActivated
                  yield Function(GameStateRule(PlayerTurn(Top))) 
                  yield Function(GameStateRule(GameRound(Begin)))
                  yield Function(GameStateRule(CollectUserActivated)) 
                        |> Rule.onlyWhen(Not(Exists(GameStateRule(Activate(Function(GameStateRule(Noop))))) <|> 
                                                Exists(GameStateRule(DeactivateUntil (Exists(GameStateRule(Noop)),GameStateList,Function(GameStateRule(Noop)))))  <|>
                                                Exists(GameStateRule(Revert (GameStateList,Function(GameStateRule(Noop)))))  <|>
                                                Exists(GameStateRule(Remove (GameStateList,Function(GameStateRule(Noop)))))  <|>
                                                Exists(GameStateRule(AddOrReplace (GameStateList,Function(GameStateRule(Noop)))))  <|>
                                                Exists(GameStateRule(Overwrite (GameStateList,Function(GameStateRule(Noop)))))  <|>
                                                Exists(GameStateRule(Repeat (0,"",Function(GameStateRule(Noop))))) ))


                                                    
                  yield Function(GameStateRule(Board({ Width = 6<ft>; Height = 4<ft> })))
              }
              |> Seq.map makeRule
              |> Map.ofSeq
          Game = { Mission = () } }
