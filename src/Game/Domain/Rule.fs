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
    type Player = Player1 | Player2
    type PlayerTurn = Top | Bottom 
    type RuleListId = GameStateList | ModelList of ModelGuid | UnitList of UnitGuid
    type BoardDimensions = {Width:int<ft>; Height:int<ft>}
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
        | ModelPosition        of Position<px>
        | Initiative      of CharacteristicValue
        | Attacks         of CharacteristicValue
        | Leadership      of CharacteristicValue
        | InvSaves        of CharacteristicValue
        | CoverSaves      of CharacteristicValue
        | Saves           of CharacteristicValue
        | SetCharacteristic of Rule
        | Unsaved of WeaponProfile
        | RemoveOnZeroCharacteristic 
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
        override  this.ToString() = toString this
        static member FromString s = fromString<UnitRuleImpl> s
    and GameRuleImpl = 
        | Noop
        | EndPhase
        | EndTurn
        | EndGame
        | RollDice
        | Board of BoardDimensions
        | SupplySortedWeaponProfiles of list<int * WeaponProfile>
        | SortedWeaponProfiles of int list
        | DiceRolled of DiceRoll
        | PlayerTurn of PlayerTurn
        | GameRound of Round
        | DeactivateUntil of LogicalExpression * RuleListId * Rule
        | Revert of RuleListId * Rule
        | Remove of RuleListId * Rule
        | Activate of Rule
        | AddOrReplace of RuleListId * Rule
        | Overwrite of RuleListId * Rule
        | Repeat of int * string * Rule
        | CollectUserActivated //of Value<Player>
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

    type TRule = 
        | TCharacteristicValue of CharacteristicValue
        | TPosition      of Position<px>
        | TRule of Rule
        | TWeaponProfile of WeaponProfile
        | TUnit 
        | TArmourPen of ArmourPen
        | TMelee of  int * DiceRoll * UnitGuid
        | TMeleeHit of int * UnitGuid
        | TMeasurement of float<inch>
        | TDeploymentState of DeploymentType
        | TWoundPool of  list<int * WeaponProfile> * ModelGuid
        | TSave of WeaponProfile * ModelGuid
        | TBoardDimensions of BoardDimensions
        | TSupplySortedWeaponProfiles of list<int * WeaponProfile>
        | TSortedWeaponProfiles of int list
        | TDiceRoll of DiceRoll
        | TPlayerTurn of PlayerTurn
        | TRound of Round
        | TList of TRule list
   
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    [<AutoOpen>]
    module Rule =
        open Microsoft.FSharp.Collections
        let (<&>) l1 l2 = Logical(l1,And,l2)
        let (<|>) l1 l2 = Logical(l1,Or,l2)
        let (<!>) l1 = Not(l1)

        let overwrite newR x = (newR,x) |> Overwritten |> Some
        let overriteOrNew r = Option.either (overwrite (r)) (Some(r))
        let overriteOrNone r = Option.either (overwrite (r)) (None)
        let unoverwrite _ = function
                                | Overwritten(_,r) -> r |> Some
                                | r -> r |> Some
        
        let activeWhen activateWhen rule =
            ActiveWhen(activateWhen,rule)

        let unoverwriteOrNew  r = Option.either (unoverwrite(r)) (Some(r))  
        let unoverwriteOrNone  r = Option.either (unoverwrite(r)) (None)                    

        let rec private after perform = function
            | ActiveWhen(logic,rule)            -> ActiveWhen(logic,after perform rule)
            | UserActivated(rule)               -> after perform  rule |> UserActivated
            | Description(_) as rule            -> rule
            | Function(Sequence(r1::tail))      -> Function(Sequence(r1 :: (List.append tail [r1 |> perform |> GameStateRule])))//          GameStateRule(DeactivateUntil(activatedWhen,r1))])))
            | Function(Sequence([])) as rule    -> rule
            | Function(r)                       -> Function(Sequence([r])) |> after perform 
            | Overwritten(newRule,old)          -> Overwritten(after perform newRule,old)
            | Nested(r,rs)                        -> (after perform r,(rs |> List.map (after perform))) |> Nested
        let afterRunDeactivateUntil list activatedWhen r = 
            after (fun _ -> DeactivateUntil(list,activatedWhen,r)) r
        let afterRunRemove list r =
            after (fun _ -> Remove(list,r)) r
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

        let rec findRuleListId rule = 
            match rule with 
            | ActiveWhen(logic,r)               -> findRuleListId r
            | UserActivated(r) as rule          -> findRuleListId r
            | Description(_) as rule            -> None 
            | Function(ra)     -> findRuleApplication ra
            | Overwritten(newRule,old)        -> findRuleListId newRule
            | Nested(rule,rules)              -> findRuleListId rule |> Option.either Some (List.choose findRuleListId rules |> List.tryHead) 


        and findRuleApplication = function 
            | GameStateRule(_)     -> GameStateList |> Some
            | ModelRule(_,mId)      -> ModelList mId |> Some
            | UnitRule(_,uId)      -> UnitList uId |> Some
            | Sequence(rs) -> List.choose findRuleApplication rs |> List.tryHead

        let afterIfRemove logical (r:Rule) =

            let ruleList = findRuleListId r |> defaultArg <| GameStateList
            r            
            |> afterRunRemove (ruleList)
            |> onlyWhen logical
            |> otherwise (Function(GameStateRule(Remove(ruleList, r))))
            
        let (++) = append

        let rec textFromRuleApplication = 
                function 
                | GameStateRule impl -> impl.ToString()
                | ModelRule(impl, _) -> impl.ToString()
                | Sequence(rs) -> textFromRuleApplication (rs |> Seq.head)
                | UnitRule(impl, _) -> impl.ToString()

        let rec textFromRule = 
            function 
            | Function(impl) -> textFromRuleApplication impl
            | UserActivated(r) -> textFromRule r
            | ActiveWhen(_, r) -> textFromRule r
            | Description(d) -> d.Name
            | Overwritten(_, r) -> textFromRule r
            | Nested(h,_) -> textFromRule h
        let makeRule r = 
            textFromRule r, r

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
        