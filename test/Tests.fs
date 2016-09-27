namespace Game.Tests

open NUnit.Framework
open FsUnit
open Domain.Board
open Domain
open GameImpl.RulesImpl
open GameImpl.GameState
open GameImpl.GameStateComputation
open Domain.WarhammerDomain

[<TestFixture>] 
type ``Given a Example state with Single Rules`` () =
   let addOrUpdateGameState (rule,gameState) = runT (tryReplaceRuleOnGameState def rule) gameState  |> snd
   let removeFromGameState (rule,gameState) = runT (tryReplaceRuleOnGameState defnot rule ) gameState |> snd

   let ruleToAdd = Activate (GameStateRule(PlayerTurn(Top))) |> GameStateRule |> Function |> Rule.afterRunRemove  
   let ruleToModify = Function(GameStateRule(GameRound(Round.Begin))) 
   let containsKey rule gs = gs |> (fun m -> m.Rules) |> Map.tryFind (makeRule rule |> fst)  |> Option.isSome  
   let doesntContainKeyIsFalse ruleToModify = (snd >> containsKey ruleToModify >> not) 
   let containsValue rule gs = gs |> (fun m -> m.Rules) |> Map.tryFindKey (fun k r -> k = (makeRule rule |> fst) && r = rule)  |> Option.isSome       
   
   let uId = UnitGuid "A4F2493F-08BA-44EA-B813-0F3E5E53110B"    
   let ruleToAddUnit =     
      Function(UnitRule(Move 3.<inch>, uId))
                         |> Rule.onlyWhen (Rule(GameStateRule(GameRound(One(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Two(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Three(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Four(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Five(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Six(Movement)))) <|>
                                             Rule(GameStateRule(GameRound(Seven(Movement)))))
                         |> Rule.userActivated
                         |> Rule.afterRunDeactivateUntil (Rule(GameStateRule(EndPhase)))

   
   let ruleToModifyUnit = Function(UnitRule(DeploymentState(OngoingReserves),uId))  
   let containsKeyUnit rule gs = runT (tryFindUnit uId) gs |> fst |> Option.bind (fun m -> m.Rules |> Map.tryFind (makeRule rule |> fst))  |> Option.isSome  
   let doesntContainKeyIsFalseUnit ruleToModify = (snd >> containsKeyUnit ruleToModify >> not) 
   let containsValueUnit rule gs =  runT (tryFindUnit uId) gs |> fst |> Option.bind (fun m -> m.Rules |> Map.tryFindKey (fun k r -> k = (makeRule rule |> fst) && r = rule))  |> Option.isSome       
   let addOrUpdateUnit (rule,gameState) = runT (tryReplaceRuleOnUnit def rule uId) gameState |> snd
   let removeFromUnit (rule,gameState) = runT (tryReplaceRuleOnUnit defnot rule uId) gameState |> snd

   let mId = ModelGuid "2D5045C8-0427-4C6D-B0A4-371F46DAF844" 
   
   let (Some foundUnit) = runT (tryFindUnit  uId) Impl.ImplTest.initial   |> fst      
   let (Some p) = runT (tryFindPlayer foundUnit) Impl.ImplTest.initial      |> fst
   let gameState = Impl.ImplTest.initial 
   let f m = { Model = m; Player = p.Player; Position = {X = 1<px>;Y = 1<px>}} 
   let gameState = runT (forAllModels (map f) foundUnit) gameState |> snd

   let ruleToAddModel = Function(ModelRule(CoverSaves(CharacteristicValue 3), mId))
   let ruleToModifyModel = Function(ModelRule(Toughness(CharacteristicValue 6), mId))
   let containsKeyModel rule gs = runT (tryFindModel mId) gs |> fst |> Option.bind (fun m -> m.Model.Rules |> Map.tryFind (makeRule rule |> fst))  |> Option.isSome  
   let doesntContainKeyIsFalseModel ruleToModify = (snd >> containsKeyModel ruleToModify >> not) 
   let containsValueModel rule gs = runT (tryFindModel mId) gs |> fst |> Option.bind (fun m -> m.Model.Rules |> Map.tryFindKey (fun k r -> k = (makeRule rule |> fst) && r = rule))  |> Option.isSome       
   let addOrUpdateModel (rule,gameState) = runT (tryReplaceRuleOnModel def rule mId) gameState |> snd
   let removeFromModel (rule,gameState) = runT (tryReplaceRuleOnModel defnot rule mId) gameState |> snd
   
   
   
   let falseActTrue setup before act after =
        setup |> printfn "Setup: %A"
        setup |> before |> should be False
        let result = setup |> act 
        result |> printfn "Result: %A"
        result |> after |> should be True
 
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
    ``Adding a new rule, adds a new rule Unit`` () =
        falseActTrue (ruleToAddUnit,gameState) (snd >> containsKeyUnit ruleToAddUnit) addOrUpdateUnit (containsKeyUnit ruleToAddUnit)
   [<Test>] member test.
    ``Adding a new rule, is instance of new rule Unit`` () =
        falseActTrue (ruleToAddUnit,gameState) (snd >> containsKeyUnit ruleToAddUnit) addOrUpdateUnit (containsValueUnit ruleToAddUnit)
   [<Test>] member test.
    ``Modifying a rule, changes it Unit`` () =
        falseActTrue (ruleToModifyUnit,gameState) (doesntContainKeyIsFalseUnit ruleToModifyUnit) addOrUpdateUnit (containsKeyUnit ruleToModifyUnit)
   [<Test>] member test.
    ``Modifying a rule, is instance of new rule Unit`` () =
        falseActTrue (ruleToModifyUnit,gameState) (doesntContainKeyIsFalseUnit ruleToModifyUnit) addOrUpdateUnit (containsValueUnit ruleToModifyUnit)
   [<Test>] member test.
    ``Removing a rule, removes the rule Unit`` () =
        falseActTrue (ruleToModifyUnit,gameState) (doesntContainKeyIsFalseUnit ruleToModifyUnit) removeFromUnit (containsKeyUnit ruleToModifyUnit >> not)
   [<Test>] member test.
    ``Adding a new rule, adds a new rule Model`` () =
        falseActTrue (ruleToAddModel,gameState) (snd >> containsKeyModel ruleToAddModel) addOrUpdateModel (containsKeyModel ruleToAddModel)
   [<Test>] member test.
    ``Adding a new rule, is instance of new rule Model`` () =
        falseActTrue (ruleToAddModel,gameState) (snd >> containsKeyModel ruleToAddModel) addOrUpdateModel (containsValueModel ruleToAddModel)
   [<Test>] member test.
    ``Modifying a rule, changes it Model`` () =
        falseActTrue (ruleToModifyModel,gameState) (doesntContainKeyIsFalseModel ruleToModifyModel) addOrUpdateModel (containsKeyModel ruleToModifyModel)
   [<Test>] member test.
    ``Modifying a rule, is instance of new rule Model`` () =
        falseActTrue (ruleToModifyModel,gameState) (doesntContainKeyIsFalseModel ruleToModifyModel) addOrUpdateModel (containsValueModel ruleToModifyModel)
   [<Test>] member test.
    ``Removing a rule, removes the rule Model`` () =
        falseActTrue (ruleToModifyModel,gameState) (doesntContainKeyIsFalseModel ruleToModifyModel) removeFromModel (containsKeyModel ruleToModifyModel >> not)
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