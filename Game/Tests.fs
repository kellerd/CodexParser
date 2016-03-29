namespace Game.Tests

open NUnit.Framework
open FsUnit
open Domain.WarhammerDomain
open GameImpl.WarhammerImpl

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
            |> List.collect (fun p -> p.Units)
            |> List.collect (fun u -> u.Rules) |> should not' (be Empty)

   [<Test>] member test.
    ``Expect isRunnable will be true`` ()=
           gameState.Players 
            |> List.filter (fun p -> p.Player = gameState.Players.Head.Player) 
            |> List.collect (fun p -> p.Units)
            |> List.collect (fun u -> u.Rules |> List.filter isRunnable) |> should not' (be Empty)

   [<Test>] member test.
    ``Capabilities should not be empty`` ()=
           availableRuleCapabilities gameState.Players.Head.Player gameState |> should not' (be Empty)

type ``Given a mission in top, at the end of phase`` () =
    let gameState = { Impl.ImplTest.initial with Game = {Impl.ImplTest.initial.Game with Turn = Top(One(Phase.End))}}
    let gameTurn = 
        match gameState.Game.Turn with 
            | Top gt -> gt
            | _ -> failwith "Wrong turn starting state, should be Top of"
    [<Test>] member test.
        ``Turn should change from top to bottom``() = 
            (advancePhase gameState).Game.Turn |> should equal (Bottom(gameTurn))
    [<Test>] member test.
        ``Turn should go to other player if ending last phase``() =
            match (playerMove Player1 None endPhase gameState) with
                | Player1ToMove _ -> failwith "Player should swap"
                | Player2ToMove _ -> true |> should be True
                | GameWon _ -> failwith "Tied not enough capabilities" 
                | GameTied _ -> failwith "Tied not enough capabilities" 