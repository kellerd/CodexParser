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
    type CharacteristicValue = 
        | CharacteristicValue of int

    type DiceRoll = DiceRoll of int

    type PlayerTurn = Top | Bottom 

    type DeploymentType = 
        | Destroyed
        | OngoingReserves
        | Reserves
        | Deployed
        | Start
    type ArmourPen = int option
    type ModelRuleImpl = 
        | WeaponSkill     of CharacteristicValue
        | BallisticSkill  of CharacteristicValue
        | Strength        of CharacteristicValue
        | Toughness       of CharacteristicValue
        | Wounds          of CharacteristicValue
        | Initiative      of CharacteristicValue
        | Attacks         of CharacteristicValue
        | Leadership      of CharacteristicValue
        | InvSaves        of CharacteristicValue
        | CoverSaves      of CharacteristicValue
        | Saves           of CharacteristicValue
        | SetCharacteristic of string * Rule
        | ArmourPenetration of ArmourPen
        | Melee of int * DiceRoll * UnitGuid
        | MeleeHit of int * UnitGuid
        override this.ToString() = toString this
        static member FromString s = fromString<ModelRuleImpl> s
    and UnitRuleImpl = 
        | Move of float<inch>
        | DeploymentState of DeploymentType
        | Deploy
        | WoundPool of  list<int * WeaponProfile> * ModelGuid
        | SortedWoundPool of  list<int * WeaponProfile> * ModelGuid
        | Save of WeaponProfile * ModelGuid
        | Unsaved of WeaponProfile*ModelGuid
        override  this.ToString() = toString this
        static member FromString s = fromString<UnitRuleImpl> s
    and GameRuleImpl = 
        | Noop
        | EndPhase
        | EndTurn
        | EndGame
        | RollDice
        | SupplySortedWeaponProfiles of list<int * WeaponProfile>
        | SortedWeaponProfiles of int list
        | DiceRolled of DiceRoll
        | PlayerTurn of PlayerTurn
        | GameRound of Round
        | DeactivateUntil of LogicalExpression * RuleApplication
        | Revert of RuleApplication
        | Remove of RuleApplication
        | Repeat of int * string * Rule
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
        | Nested of Rule * Rule list
    and WeaponProfile = RuleApplication list

   
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    [<AutoOpen>]
    module Rule =
        let (<&>) l1 l2 = Logical(l1,And,l2)
        let (<|>) l1 l2 = Logical(l1,Or,l2)
        let (<!>) l1 = Not(l1)

        let overwrite newR x = Overwritten(newR,x)
        let unoverwrite = function
                                | Overwritten(_,r) -> r
                                | r -> r
        let userActivated rule =
            let rec userActivated' = function
                    | ActiveWhen(logic,r)               -> ActiveWhen(logic,userActivated' r)
                    | UserActivated(_) as rule          -> rule
                    | Description(_) as rule            -> rule
                    | Function(Sequence(r1::tail))      -> Function(Sequence(r1::GameStateRule(Revert(r1))::tail))
                    | Function(Sequence([])) as rule    -> rule
                    | Function(r)                       -> Function(Sequence([r])) |> userActivated'
                    | Overwritten(newRule,old)          -> Overwritten(userActivated' newRule,old)
                    | Nested(_) as rule                 -> rule
            userActivated' rule |> UserActivated
        let rec private after perform = function
            | ActiveWhen(logic,rule)            -> ActiveWhen(logic,after perform rule)
            | UserActivated(rule)               -> after perform  rule |> UserActivated
            | Description(_) as rule            -> rule
            | Function(Sequence(r1::tail))      -> Function(Sequence(r1 :: (List.append tail [r1 |> perform |> GameStateRule])))//          GameStateRule(DeactivateUntil(activatedWhen,r1))])))
            | Function(Sequence([])) as rule    -> rule
            | Function(r)                       -> Function(Sequence([r])) |> after perform 
            | Overwritten(newRule,old)          -> Overwritten(after perform newRule,old)
            | Nested(r,rs)                        -> (after perform r,(rs |> List.map (after perform))) |> Nested
        let afterRunDeactivateUntil activatedWhen = 
            after (fun r -> DeactivateUntil(activatedWhen,r))
        let afterRunRemove =
            after Remove
        let afterRunRepeat times name r = 
            after (fun _  -> Repeat(times,name,r)) r

        let rec append r1 r2 = 
            let unwrap = function
                | UnitRule(_) as rule       -> [rule]              
                | ModelRule(_) as rule      -> [rule] 
                | GameStateRule(_) as rule  -> [rule] 
                | Sequence(ras)             -> ras
            let r2' = unwrap r2
            match r1 with
            | UnitRule(_) as rule -> Sequence(rule :: r2')                 
            | ModelRule(_) as rule -> Sequence(rule :: r2')   
            | GameStateRule(_) as rule -> Sequence(rule :: r2')   
            | Sequence(ras) -> Sequence(ras @  r2')

        let onlyWhen l1 r1 = ActiveWhen(l1,r1)

        let otherwise r1 r2 = 
            match r1 with 
            | ActiveWhen(l1,_) -> Nested(r1,[onlyWhen (Not(l1)) r2])
            | r -> Overwritten(r2,r)
        let afterIfRemove logical ra =
            Function(ra)            
            |> afterRunRemove
            |> onlyWhen logical
            |> otherwise (Function(GameStateRule(Remove(ra))))
            
        let (++) = append

        let rec textFromRuleApplication = 
                function 
                | GameStateRule impl -> impl.ToString()
                | ModelRule(impl, _) -> impl.ToString()
                | Sequence(rs) -> textFromRuleApplication (rs |> Seq.head)
                | UnitRule(impl, _) -> impl.ToString()

        let makeRule r = 
            let rec makeText = 
                function 
                | Function(impl) -> textFromRuleApplication impl
                | UserActivated(r) -> makeText r
                | ActiveWhen(_, r) -> makeText r
                | Description(d) -> d.Name
                | Overwritten(_, r) -> makeText r
                | Nested(h,_) -> makeText h
        
            makeText r, r

        let D6 = 6
        
        let badRolls sides (DiceRoll lessThan) =
            Seq.initInfinite ((+) 1) 
            |> Seq.takeWhile (fun i -> i < lessThan && i <= sides) 
            |> Seq.map (DiceRoll >> DiceRolled >> GameStateRule >> Rule)
            |> Seq.reduce (<|>)
        let goodRolls sides (DiceRoll equalOrGreaterThan) =
            Seq.initInfinite ((+) equalOrGreaterThan) 
            |> Seq.takeWhile (fun i -> i <= sides) 
            |> Seq.map (DiceRoll >> DiceRolled >> GameStateRule >> Rule)
            |> Seq.reduce (<|>)
        