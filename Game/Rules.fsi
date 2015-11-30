namespace Domain

module WarhammerRules =
    open Domain.WarhammerDomain

    type hits = Weapon -> BallisticSkill -> Probability
    type wounds = Toughness -> Strength -> Probability -> Probability
    type saved = ArmorPen -> Saves -> InvSaves -> Probability -> Probability
    type battleModels' = Model -> Model -> Probability
    type battleModels = Model -> Model -> Weapon -> Probability
    type toWound = Toughness -> Strength -> int

