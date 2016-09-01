[<EntryPoint>]
let main argv = 
    let startGame() =
        let api = GameImpl.GameLoop.api 
        let loggedApi = Logging.Logger.injectLogging api
        ConsoleUi.ConsoleWarhammer.startGame loggedApi 
    startGame()
    0 // return an integer exit code
