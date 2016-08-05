#if !INTERACTIVE 
namespace ConsoleApplication
module ConsoleApplication = 
#else
    #load "Equals.fs"
    #load "Map.fs"
    #load "Base.fs"
    #load "Rule.fs"
    #load "Board.fs"
    #load "Asker.fs"
    #load "Game.fs"
    #load "GameState.fs"
    #load "ImplTest.fs"
    #load "ConsoleUi.fs"
    #load "RulesImpl.fs"
    #load "GameLoop.fs"
    #load "Logging.fs"
#endif
    open GameImpl
    open Logging
    let startGame() =
        
        let api = GameLoop.api 
        let loggedApi = Logger.injectLogging api
        ConsoleUi.ConsoleWarhammer.startGame loggedApi 
#if INTERACTIVE 
    startGame()
#endif