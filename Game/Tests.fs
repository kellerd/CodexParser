namespace Game.Tests

open NUnit.Framework
open FsUnit
open Domain.WarhammerDomain
open GameImpl.WarhammerImpl

[<TestFixture>] 
type ``Given a Example state with Single Rules`` ()=
   let gameState = initial


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

