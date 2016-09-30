namespace UI 

module Display =
    open Domain.Tabletop 
    open Domain.WarhammerDomain
    open Domain
    type ModelInfo = {
        Model : Model
        Player : Player
    }
    type Display = {
        Models : (ModelInfo * Position<px>) list
        Rules : Rule list
        Dimensions : BoardDimensions
    }