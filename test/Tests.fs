namespace Game.Tests

open NUnit.Framework
open FsUnit
open Domain.Board
open Domain
open GameImpl.RulesImpl

[<TestFixture>] 
type ``Given a Example state with Single Rules`` () =
   let gameState = Impl.ImplTest.initial
  
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
           availableRules (activeRules gameState) activeRulesMap Player1 gameState |> should not' (be Empty)
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