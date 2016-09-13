namespace Game.Tests

open NUnit.Framework
open FsUnit
open Domain.Board
open Domain
open GameImpl.RulesImpl
open GameImpl.GameState
open Domain.WarhammerDomain

[<TestFixture>] 
type ``Given a Example state with Single Rules`` () =
   let gameState = Impl.ImplTest.initial
   let ruleToAdd = Activate (GameStateRule(PlayerTurn(Top))) |> GameStateRule |> Function |> Rule.afterRunRemove  
   let ruleToAddUnit =     
      let uId = UnitGuid "A4F2493F-08BA-44EA-B813-0F3E5E53110B"              
      Function(UnitRule(Move 6.<inch>, uId))
                         |> Rule.onlyWhen (Rule(GameStateRule(GameRound(One(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Two(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Three(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Four(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Five(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Six(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Seven(Movement)))))
                         |> Rule.userActivated
                         |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))

   let ruleToModify = Function(GameStateRule(GameRound(Round.Begin)))  
   let containsKey rule gs = gs |> (fun m -> m.Rules) |> Map.tryFind (makeRule rule |> fst)  |> Option.isSome  
   let doesntContainKeyIsFalse ruleToModify = (snd >> containsKey ruleToModify >> not) 
   let containsValue rule gs = gs |> (fun m -> m.Rules) |> Map.tryFindKey (fun k r -> k = (makeRule rule |> fst) && r = rule)  |> Option.isSome       
   let falseActTrue setup before act after =
        setup |> before  |> should be False 
        setup |> act  |> after |> should be True
   let addOrUpdateGameState (rule,gameState) = tryReplaceRuleOnGameState def rule gameState
   let removeFromGameState (rule,gameState) = tryReplaceRuleOnGameState defnot rule gameState
   [<Test>] member test.
    ``Player should be structurally equal`` ()=
           gameState.Players 
            |> List.filter (fun p -> p.Player = gameState.Players.Head.Player) |> should not' (be Empty)

    
   [<Test>] member test.
    ``Player should have units and rules to test`` ()=
           gameState.Players 
            |> List.filter (fun p -> p.Player = gameState.Players.Head.Player)
            |> List.collect (fun p -> p.Units |> Map.toList |> List.map (snd))
            |> List.collect (fun u -> u.Rules |> Map.toList |> List.map (snd)) |> should not' (be Empty)

//   [<Test>] member test.
//    ``Expect isRunnable will be true`` ()=
//           gameState.Players 
//            |> List.filter (fun p -> p.Player = gameState.Players.Head.Player) 
//            |> List.collect (fun p -> p.Units |> Map.toList |> List.map (snd))
//            |> List.collect (fun u -> u.Rules |> Map.toList |> List.map (snd) |> List.collect (collectRules gameState)) |> should not' (be Empty)

   [<Test>] member test.
    ``Capabilities should not be empty`` ()=
           availableRules (activeRules gameState) Player1 gameState |> should not' (be Empty)
   
   [<Test>] member test.
    ``Adding a new rule, adds a new rule`` () =
        falseActTrue (ruleToAdd,gameState) (snd >> containsKey ruleToAdd) addOrUpdateGameState (containsKey ruleToAdd)
   [<Test>] member test.
    ``Adding a new rule, is instance of new rule`` () =
        falseActTrue (ruleToAdd,gameState) (snd >> containsKey ruleToAdd) addOrUpdateGameState (containsValue ruleToAdd)
   [<Test>] member test.
    ``Modifying a rule, changes it`` () =
        falseActTrue (ruleToModify,gameState) (doesntContainKeyIsFalse ruleToModify) addOrUpdateGameState (containsKey ruleToModify)
   [<Test>] member test.
    ``Modifying a rule, is instance of new rule`` () =
        falseActTrue (ruleToModify,gameState) (doesntContainKeyIsFalse ruleToModify) addOrUpdateGameState (containsValue ruleToModify)
   [<Test>] member test.
    ``Removing a rule, removes the rule`` () =
        falseActTrue (ruleToModify,gameState) (doesntContainKeyIsFalse ruleToModify) removeFromGameState (containsKey ruleToModify >> not)

   
   [<Test>] member test.
    ``Adding a new rule, adds a new rule`` () =
        falseActTrue (ruleToAddUnit,gameState) (snd >> containsKey ruleToAddUnit) addOrUpdateUnit (containsKey ruleToAddUnit)
   [<Test>] member test.
    ``Adding a new rule, is instance of new rule`` () =
        falseActTrue (ruleToAddUnit,gameState) (snd >> containsKey ruleToAddUnit) addOrUpdateUnit (containsValue ruleToAddUnit)
   [<Test>] member test.
    ``Modifying a rule, changes it`` () =
        falseActTrue (ruleToModifyUnit,gameState) (doesntContainKeyIsFalse ruleToModifyUnit) addOrUpdateUnit (containsKey ruleToModifyUnit)
   [<Test>] member test.
    ``Modifying a rule, is instance of new rule`` () =
        falseActTrue (ruleToModifyUnit,gameState) (doesntContainKeyIsFalse ruleToModifyUnit) addOrUpdateUnit (containsValue ruleToModifyUnit)
   [<Test>] member test.
    ``Removing a rule, removes the rule`` () =
        falseActTrue (ruleToModifyUnit,gameState) (doesntContainKeyIsFalse ruleToModifyUnit) removeFromUnit (containsKey ruleToModifyUnit >> not)

   [<Test>] member test.
    ``Adding a new rule, adds a new rule`` () =
        falseActTrue (ruleToAddModel,gameState) (snd >> containsKey ruleToAddModel) addOrUpdateModel (containsKey ruleToAddModel)
   [<Test>] member test.
    ``Adding a new rule, is instance of new rule`` () =
        falseActTrue (ruleToAddModel,gameState) (snd >> containsKey ruleToAddModel) addOrUpdateModel (containsValue ruleToAddModel)
   [<Test>] member test.
    ``Modifying a rule, changes it`` () =
        falseActTrue (ruleToModifyModel,gameState) (doesntContainKeyIsFalse ruleToModifyModel) addOrUpdateModel (containsKey ruleToModifyModel)
   [<Test>] member test.
    ``Modifying a rule, is instance of new rule`` () =
        falseActTrue (ruleToModifyModel,gameState) (doesntContainKeyIsFalse ruleToModifyModel) addOrUpdateModel (containsValue ruleToModifyModel)
   [<Test>] member test.
    ``Removing a rule, removes the rule`` () =
        falseActTrue (ruleToModifyModel,gameState) (doesntContainKeyIsFalse ruleToModifyModel) removeFromModel (containsKey ruleToModifyModel >> not)
//type ``Given a mission in top, at the end of phase`` () =
//    let gameState = { Impl.ImplTest.initial with Game = {Impl.ImplTest.initial.Game with Turn = Top(One(Phase.End))}}
//    let positionAsker gs = 
//        let r = new System.Random()
//        {X=r.Next(ConsoleUi.ConsoleWarhammer.ftToPx gs.Board.Dimensions.Width-1<px> |> int)*1<px>;Y=r.Next(ConsoleUi.ConsoleWarhammer.ftToPx gs.Board.Dimensions.Height-1<px> |> int)*1<px>}
//    let moveAsker positions = 
//        let r = new System.Random()
//        let n = r.Next(1,(Array.length positions)-1)
//        Array.item n positions
//
//    let bottomOf = 
//        match gameState.Game.Turn with 
//            | Top(Begin) -> Bottom(Begin)
//            | Top(One(Phase.End))   -> Bottom(One(Phase.Begin))
//            | Top(Two(Phase.End))   -> Bottom(Two(Phase.Begin))
//            | Top(Three(Phase.End)) -> Bottom(Three(Phase.Begin))
//            | Top(Four(Phase.End))  -> Bottom(Four(Phase.Begin))
//            | Top(Five(Phase.End))  -> Bottom(Five(Phase.Begin))
//            | Top(Six(Phase.End))   -> Bottom(Six(Phase.Begin))
//            | Top(Seven(Phase.End)) -> Bottom(Seven(Phase.Begin))
//            | Top(End) -> Bottom(End)
//            | _ -> failwith "Wrong turn starting state, should be Top of"
//    [<Test>] member test.
//        ``Turn should change from top to bottom``() = 
//            (advancePhase gameState).Game.Turn |> should equal bottomOf
//    [<Test>] member test.
//        ``Turn should go to other player if ending last phase``() =
//            match (playerMove Player1 None [endPhase] gameState) with
//                | Player1ToMove _ -> failwith "Player should swap"
//                | Player2ToMove _ -> true |> should be True
//                | GameWon _ -> failwith "Tied not enough capabilities" 
//                | GameTied _ -> failwith "Tied not enough capabilities" 