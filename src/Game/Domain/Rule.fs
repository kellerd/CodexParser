namespace Domain
    open WarhammerDomain
    
    type Apply<'a> =
        | NotApplied  
        | Applied of 'a
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
        | Melee of int * UnitGuid Apply
        | MeleeHit of int * UnitGuid
        override this.ToString() = toString this
        static member FromString s = fromString<ModelRuleImpl> s
    and UnitRuleImpl = 
        | Move of float<inch>
        | DeploymentState of DeploymentType
        | Deploy
        | WoundPool of  list<int * WeaponProfile> *  ModelGuid
        | SortedWoundPool of  list<int * WeaponProfile> * ModelGuid
        | Save of WeaponProfile * ModelGuid
        override  this.ToString() = toString this
        static member FromString s = fromString<UnitRuleImpl> s
    and GameRuleImpl = 
        | Noop
        | EndPhase
        | EndTurn
        | EndGame
        | Board of BoardDimensions
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
        | CollectUserActivated 
        | Applications of Map<string,Rule>
        | Supply of string * TRule
        override this.ToString() = toString this
        static member FromString s = fromString<GameRuleImpl> s
    and LogicalExpression =
        | Logical of LogicalExpression * LogicalOperator * LogicalExpression
        | Not of LogicalExpression
        | Matches of RuleApplication
        | Exists of RuleApplication
        | Literal of PrimaryEqualityExpression
    and PrimaryEqualityExpression = TRule Apply * PrimaryOperator * TRule Apply
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

    and TRule = 
        | TCharacteristicValue of CharacteristicValue
        | TPosition      of Position<px>
        | TWeaponProfile of WeaponProfile
        | TCount of int
        | TUnit 
        | TRule of Rule
        | TArmourPen of ArmourPen
        | TTarget of UnitGuid
        | TSpecialTarget of ModelGuid
        | TMeasurement of float<inch>
        | TDeploymentState of DeploymentType
        | TBoardDimensions of BoardDimensions
        | TDiceRoll of DiceRoll
        | TPlayerTurn of PlayerTurn
        | TRound of Round
        | TList of TRule list
        | TApplicationMap of  Map<string,Rule>
        static member Compare = function 
        | TCharacteristicValue          x, Eq,   TCharacteristicValue         y -> x = y
        | TPosition                     x, Eq,   TPosition                    y -> x = y
        | TWeaponProfile                x, Eq,   TWeaponProfile               y -> x = y
        | TUnit                          , Eq,   TUnit                          -> true
        | TArmourPen                    x, Eq,   TArmourPen                   y -> x = y
        | TMeasurement                  x, Eq,   TMeasurement                 y -> x = y
        | TDeploymentState              x, Eq,   TDeploymentState             y -> x = y
        | TBoardDimensions              x, Eq,   TBoardDimensions             y -> x = y
        | TDiceRoll                     x, Eq,   TDiceRoll                    y -> x = y
        | TPlayerTurn                   x, Eq,   TPlayerTurn                  y -> x = y
        | TRound                        x, Eq,   TRound                       y -> x = y
        | TList                         x, Eq,   TList                        y -> x = y
        | TRule                         x, Eq,   TRule                        y -> x = y
        | TApplicationMap               x, Eq,   TApplicationMap              y -> x = y
        | TCharacteristicValue          x, Ne,   TCharacteristicValue         y -> x <> y
        | TPosition                     x, Ne,   TPosition                    y -> x <> y
        | TWeaponProfile                x, Ne,   TWeaponProfile               y -> x <> y
        | TUnit                          , Ne,   TUnit                          -> false
        | TArmourPen                    x, Ne,   TArmourPen                   y -> x <> y
        | TMeasurement                  x, Ne,   TMeasurement                 y -> x <> y
        | TDeploymentState              x, Ne,   TDeploymentState             y -> x <> y
        | TBoardDimensions              x, Ne,   TBoardDimensions             y -> x <> y
        | TDiceRoll                     x, Ne,   TDiceRoll                    y -> x <> y
        | TPlayerTurn                   x, Ne,   TPlayerTurn                  y -> x <> y
        | TRound                        x, Ne,   TRound                       y -> x <> y
        | TList                         x, Ne,   TList                        y -> x <> y
        | TRule                         x, Ne,   TRule                        y -> x <> y
        | TApplicationMap               x, Ne,   TApplicationMap              y -> x <> y
        | TCharacteristicValue          x, Gt,   TCharacteristicValue         y -> x > y
        | TPosition                     x, Gt,   TPosition                    y -> x > y
        | TWeaponProfile                x, Gt,   TWeaponProfile               y -> x > y
        | TUnit                          , Gt,   TUnit                          -> false
        | TArmourPen                    x, Gt,   TArmourPen                   y -> x > y
        | TMeasurement                  x, Gt,   TMeasurement                 y -> x > y
        | TDeploymentState              x, Gt,   TDeploymentState             y -> x > y
        | TBoardDimensions              x, Gt,   TBoardDimensions             y -> x > y
        | TDiceRoll                     x, Gt,   TDiceRoll                    y -> x > y
        | TPlayerTurn                   x, Gt,   TPlayerTurn                  y -> x > y
        | TRound                        x, Gt,   TRound                       y -> x > y
        | TList                         x, Gt,   TList                        y -> x > y
        | TRule                         x, Gt,   TRule                        y -> x > y
        | TApplicationMap               x, Gt,   TApplicationMap              y -> x > y
        | TCharacteristicValue          x, Lt,   TCharacteristicValue         y -> x < y
        | TPosition                     x, Lt,   TPosition                    y -> x < y
        | TWeaponProfile                x, Lt,   TWeaponProfile               y -> x < y
        | TUnit                          , Lt,   TUnit                          -> false
        | TArmourPen                    x, Lt,   TArmourPen                   y -> x < y
        | TMeasurement                  x, Lt,   TMeasurement                 y -> x < y
        | TDeploymentState              x, Lt,   TDeploymentState             y -> x < y
        | TBoardDimensions              x, Lt,   TBoardDimensions             y -> x < y
        | TDiceRoll                     x, Lt,   TDiceRoll                    y -> x < y
        | TPlayerTurn                   x, Lt,   TPlayerTurn                  y -> x < y
        | TRound                        x, Lt,   TRound                       y -> x < y
        | TList                         x, Lt,   TList                        y -> x < y
        | TRule                         x, Lt,   TRule                        y -> x < y
        | TApplicationMap               x, Lt,   TApplicationMap              y -> x < y
        | _, _, _ -> failwith "Unexpected compare"
    
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
        let userActivated player r1  = 
            let turn =
                match player with
                | Player1 -> PlayerTurn(Top)
                | Player2 -> PlayerTurn(Bottom)
                |> GameStateRule
                |> LogicalExpression.Matches
            onlyWhen turn r1 |> UserActivated

        let otherwise r1 r2 = 
            match r1 with 
            | ActiveWhen(l1,_) -> Nested(r1,[onlyWhen (Not(l1)) r2])
            | r -> Overwritten(r2,r)
        
        let either l r1 r2 = ActiveWhen(l,r1) |> otherwise r2

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
            either logical (afterRunRemove ruleList r) (Function(GameStateRule(Remove(ruleList, r))))
            
        let afterIfElseRemove logical (r:Rule) r2 =
            let ruleList = findRuleListId r |> defaultArg <| GameStateList
            either logical (afterRunRemove ruleList r) (Nested(r2,[Function(GameStateRule(Remove(ruleList, r)))]))
            

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
            |> Seq.map (DiceRoll >> DiceRolled >> GameStateRule >> Matches)
            |> Seq.reduce (<|>)
        let goodRolls sides (DiceRoll equalOrGreaterThan) =
            Seq.initInfinite ((+) equalOrGreaterThan) 
            |> Seq.takeWhile (fun i -> i <= sides) 
            |> Seq.map (DiceRoll >> DiceRolled >> GameStateRule >> Matches)
            |> Seq.reduce (<|>)
