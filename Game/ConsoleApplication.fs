#if !INTERACTIVE 
namespace ConsoleApplication
module ConsoleApplication = 
#else
    #load "Domain.fs"
    #load "ModelImplTest.fs"
    #load "GameImpl.fs"
    #load "ConsoleUi.fs"
    #load "Logging.fs"
#endif
    open GameImpl
    open Logging
    let startGame() =
        let api = WarhammerImpl.api
        let loggedApi = Logger.injectLogging api
        ConsoleUi.ConsoleWarhammer.startGame loggedApi 

#if INTERACTIVE 
    startGame()
#endif
