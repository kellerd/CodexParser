namespace Game.Tests

open Domain
open NUnit.Framework
open FsUnitTyped
open Domain.Tabletop
open Domain.Game
open Domain.WarhammerDomain
open GameImpl.RulesImpl

[<TestFixture>]
type ``Given a certain state eval should work``() = 
    let make = Seq.map makeRule >> Map.ofSeq
    let expected rules = 
        { Players = 
              [ { Player = Player1
                  Units = Map.empty<UnitGuid, Unit>
                  Score = Score 0 }
                { Player = Player2
                  Units = Map.empty<UnitGuid, Unit>
                  Score = Score 0 } ]
          Rules = make rules
          Game = { Mission = () } }
    let initial = expected []
    let rulesShouldEqual rules = function 
        | GameStateResult gameState -> gameState.Rules |> shouldEqual (make rules)
        | _ -> failwith "Not possible"
    let rulesShouldNotContain rules = function 
        | GameStateResult gameState -> gameState.Rules |> Map.filter (fun k r -> List.contains r rules) |> Map.isEmpty |> shouldEqual true
        | _ -> failwith "Not possible"
    [<Test>]
    member test.``Add rule should put it in gameState``() = 
        runRules [ Function(GameStateRule(AddOrReplace(GameStateList, Function(GameStateRule(Noop))))) ] initial
        |> rulesShouldEqual [ Function(GameStateRule(Noop)) ]
    
    [<Test>]
    member test.``Add/Replace with existing should replace rule in gameState``() = 
        { initial with Rules = make [Function(GameStateRule(GameRound(Round.Begin)))] }
        |> runRules [ Function(GameStateRule(AddOrReplace(GameStateList, Function(GameStateRule(GameRound(Round.End)))))) ] 
        |> rulesShouldEqual [ Function(GameStateRule(GameRound(Round.End))) ]
    
    [<Test>]
    member test.``Replace rule should not add it in gameState``() = 
        runRules [ Function(GameStateRule(Overwrite(GameStateList, Function(GameStateRule(Noop))))) ] initial
        |> rulesShouldNotContain [ Function(GameStateRule(Noop)) ]

    [<Test>]
    member test.``Replace rule should replace existing in gameState``() = 
        { initial with Rules = make [Function(GameStateRule(GameRound(Round.Begin)))] }
        |> runRules [ Function(GameStateRule(Overwrite(GameStateList, Function(GameStateRule(GameRound(Round.End)))))) ] 
        |> rulesShouldEqual [ Overwritten(Function(GameStateRule(GameRound(Round.End))),Function(GameStateRule(GameRound(Round.Begin)))) ]

    [<Test>]
    member test.``Remove rule should make it disapear in gameState``() = 
        { initial with Rules = make [Function(GameStateRule(GameRound(Round.Begin)))] }
        |> runRules [ Function(GameStateRule(Remove(GameStateList, Function(GameStateRule(GameRound(Round.End)))))) ] 
        |> rulesShouldEqual [ ]

    [<Test>]
    member test.``Remove rule shouldn't care about  make it disapear in gameState``() = 
        { initial with Rules = make [Function(GameStateRule(GameRound(Round.Begin)))] }
        |> runRules [ Function(GameStateRule(Remove(GameStateList, Function(GameStateRule(GameRound(Round.End)))))) ] 
        |> rulesShouldEqual [ ]

    [<Test>]
    member test.``Remove should be based on the rule name, not the content``() = 
        { initial with Rules = make [Function(GameStateRule(GameRound(Round.End)))] }
        |> runRules [ Function(GameStateRule(Remove(GameStateList, Function(GameStateRule(GameRound(Round.Begin)))))) ] 
        |> rulesShouldNotContain [ Function(GameStateRule(GameRound(Round.End))); Function(GameStateRule(GameRound(Round.Begin))) ]

    [<Test>]
    member test.``Activate should run the rule and return to normal`` () =
        let r = UserActivated(Function(GameStateRule(AddOrReplace(GameStateList, Function(GameStateRule(GameRound(Round.End))))))) 
        { initial with Rules = make [r] }
        |> runRules [Function(GameStateRule(Activate(r)))]
        |> rulesShouldEqual [r;Function(GameStateRule(GameRound(Round.End)))]
    
    [<Test>]
    member test.``Revert should make a revert to old rule`` () = 
        let r1 = Function(GameStateRule(GameRound(Round.End)))
        let r2 = Function(GameStateRule(GameRound(Round.Begin)))
        {initial with Rules = make [Overwritten(r2,r1)]}
        |> runRules [Function(GameStateRule(Revert(GameStateList, r2)))]
        |> rulesShouldEqual[r1]

    [<Test>]
    member test.``Revert should make a create a new rule if not there``() = 
        let r1 = Function(GameStateRule(GameRound(Round.End)))
        let r2 = Function(GameStateRule(GameRound(Round.Begin)))
        {initial with Rules = make []}
        |> runRules [Function(GameStateRule(Revert(GameStateList, r2)))]
        |> rulesShouldEqual [ ]