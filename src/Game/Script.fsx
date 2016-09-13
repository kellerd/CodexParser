#r """..\..\packages\FSharpx.Collections\lib\net40\FSharpx.Collections.dll"""
#load "Base\Equals.fs"
#load  "Base\Map.fs"
#load  "Base\Seq.fs"
#load  "Base\Base.fs"
#load  "Domain\Rule.fs"
#load  "Domain\Board.fs"
#load  "Domain\Asker.fs"
#load  "Domain\Game.fs"
#load  "Impl\GameState.fs"
#load  "Impl\ImplTest.fs"
#load  "Impl\RulesImpl.fs"
#load  "Impl\GameLoop.fs"
#load  "UI\ConsoleUi.fs"
#load  "UI\Logging.fs"
    
let startGame() =
    let api = GameImpl.GameLoop.api 
    let loggedApi = Logging.Logger.injectLogging api
    ConsoleUi.ConsoleWarhammer.startGame loggedApi 
startGame()