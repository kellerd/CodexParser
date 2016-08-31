#if !INTERACTIVE 
namespace ConsoleApplication
module ConsoleApplication = 
#else
#r """..\..\..\packages\FSharpx.Collections\lib\net40\FSharpx.Collections.dll"""
#load "..\Base\Equals.fs"
        "..\Base\Map.fs"
        "..\Base\Seq.fs"
        "..\Base\Base.fs"
        "..\Domain\Rule.fs"
        "..\Domain\Board.fs"
        "..\Domain\Asker.fs"
        "..\Domain\Game.fs"
        "..\Impl\GameState.fs"
        "..\Impl\ImplTest.fs"
        "..\Impl\RulesImpl.fs"
        "..\Impl\GameLoop.fs"
        "..\UI\ConsoleUi.fs"
        "..\UI\Logging.fs"
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
;;