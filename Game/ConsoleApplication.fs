
namespace ConsoleApplication
// -----------------------------------------------------------
// ConsoleApplication 
// -----------------------------------------------------------


module ConsoleApplication = 
    open GameImpl
    open Logging
    let startGame() =
        let api = TickTacToeImpl.api
        let loggedApi = Logger.injectLogging api
        ConsoleUi.Console.startGame loggedApi 


(*
To play in a IDE:
1) first highlight all code in the file and "Execute in Interactive" or equivalent
2) Uncomment the ConsoleApplication.startGame() line below and execute it
To play in command line:
1) Uncomment the ConsoleApplication.startGame() line below and execute the entire file using FSI
*)


