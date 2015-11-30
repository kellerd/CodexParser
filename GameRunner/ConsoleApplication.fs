
namespace ConsoleApplication
// -----------------------------------------------------------
// ConsoleApplication 
// -----------------------------------------------------------

module ConsoleApplication = 
    open System
    open Implementation
    open Logging
    open ConsoleUi
    let startGame() =
        let api = TicTacToeImplementation.api
        let loggedApi = Logger.injectLogging api
        ConsoleUi.startGame loggedApi 

(*
To play in a IDE:
1) first highlight all code in the file and "Execute in Interactive" or equivalent
2) Uncomment the ConsoleApplication.startGame() line below and execute it
To play in command line:
1) Uncomment the ConsoleApplication.startGame() line below and execute the entire file using FSI
*)


