namespace Domain
    open WarhammerDomain
    
    type Base =
     | BaseDiameter of int<mm>
     | ModelDimentions of Dimentions<mm>
    type Phase = Begin | Movement | Psychic | Shooting | Assault | End
    type Round = 
        | Begin
        | One    of Phase
        | Two    of Phase
        | Three  of Phase
        | Four   of Phase
        | Five   of Phase
        | Six    of Phase
        | Seven  of Phase
        | End    
    type CharacteristicValue = CharacteristicValue of int
    type Characteristic = 
        | WeaponSkill     of CharacteristicValue
        | BallisticSkill  of CharacteristicValue
        | Strength        of CharacteristicValue
        | Toughness       of CharacteristicValue
        | Wounds          of CharacteristicValue
        | Initiative      of CharacteristicValue
        | Attacks         of CharacteristicValue
        | Leadership      of CharacteristicValue
        | InvSaves        of CharacteristicValue
        | Saves           of CharacteristicValue with
        member this.ToString = toString this
        static member FromString s = fromString<Characteristic> s

    type DiceRoll = DiceRoll of int

    type PlayerTurn = Top | Bottom 

    type DeploymentType = 
        | Destroyed
        | OngoingReserves
        | Reserves
        | Deployed
        | Start

    type ModelRuleImpl = 
        | MCharacteristic of Characteristic
        | Melee of int * int * UnitGuid
        | MeleeHits of int * UnitGuid
        override this.ToString() = toString this
        static member FromString s = fromString<ModelRuleImpl> s
    and UnitRuleImpl = 
        | Move of float<inch>
        | DeploymentState of DeploymentType
        | Deploy
        | UCharacteristic of Characteristic
        | SetCharacteristicUnit of string * Rule
        override  this.ToString() = toString this
        static member FromString s = fromString<UnitRuleImpl> s
    and GameRuleImpl = 
        | Noop
        | EndPhase
        | EndTurn
        | EndGame
        | PlayerTurn of PlayerTurn
        | GameRound of Round
        // | Deactivate of RuleApplication
        | DeactivateUntil of LogicalExpression * RuleApplication
        | Revert of RuleApplication
        | Remove of RuleApplication
        override this.ToString() = toString this
        static member FromString s = fromString<GameRuleImpl> s
    and LogicalExpression =
        | Logical of LogicalExpression * LogicalOperator * LogicalExpression
        | Not of LogicalExpression
        | Rule of RuleApplication
    and RuleApplication = 
        | UnitRule of UnitRuleImpl * UnitGuid
        | ModelRule of ModelRuleImpl * ModelGuid
        | GameStateRule of GameRuleImpl
        | Sequence of RuleApplication list
    and Rule = 
        | Function of RuleApplication
        | UserActivated of Rule
        | ActiveWhen of LogicalExpression * Rule
        | Description of RuleDescription 
        | Overwritten of Rule * Rule 

    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    [<AutoOpen>]
    module Rule =
        let overwrite newR x = Overwritten(newR,x)
        let unoverwrite = function
                                | Overwritten(_,r) -> r
                                | r -> r
        let userActivated rule =
            let rec userActivated' = function
                    | ActiveWhen(logic,r) -> 
                        ActiveWhen(logic,userActivated' r)
                    | UserActivated(_) as rule -> rule
                    | Description(_) as rule-> rule
                    | Function(Sequence(r1::tail)) -> Function(Sequence(r1::GameStateRule(Revert(r1))::tail))
                    | Function(Sequence([])) as rule -> rule
                    | Function(r) -> Function(Sequence([r])) |> userActivated'
                    | Overwritten(newRule,old) -> Overwritten(userActivated' newRule,old)
            userActivated' rule |> UserActivated
        let rec private after perform = function
            | ActiveWhen(logic,rule) -> 
                    ActiveWhen(logic,after perform rule)
                | UserActivated(rule)   -> after perform  rule |> UserActivated
                | Description(_) as rule-> rule
                | Function(Sequence(r1::tail)) -> 
                    Function(Sequence(r1 :: (List.append tail [r1 |> perform |> GameStateRule])))//          GameStateRule(DeactivateUntil(activatedWhen,r1))])))
                | Function(Sequence([])) as rule -> rule
                | Function(r) -> Function(Sequence([r])) |> after perform 
                | Overwritten(newRule,old) -> Overwritten(after perform newRule,old)
        let afterRunDeactivateUntil activatedWhen = 
            after (fun r -> DeactivateUntil(activatedWhen,r))
        let afterRunRemove =
            after Remove
        let onlyWhen l1 r1 = ActiveWhen(l1,r1)
        let (<&>) l1 l2 = Logical(l1,And,l2)
        let (<|>) l1 l2 = Logical(l1,Or,l2)
        let (<!>) l1 = Not(l1)