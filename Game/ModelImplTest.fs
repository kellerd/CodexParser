#if !INTERACTIVE 
namespace Impl
module ModelImplTest = 
    open Domain.WarhammerDomain
#endif
    open System
    let Termagant id  = 
        {   Name="Termagant"; 
            Id=id;
            Characteristic = 
                [WeaponSkill    (CharacteristicValue 3);
                BallisticSkill  (CharacteristicValue 3);
                Strength        (CharacteristicValue 3);
                Toughness       (CharacteristicValue 3);
                Wounds          (CharacteristicValue 3);
                Initiative      (CharacteristicValue 3);
                Attacks         (CharacteristicValue 3);
                Leadership      (CharacteristicValue 3);
                InvSaves        (CharacteristicValue 3);
                Saves           (CharacteristicValue 3)] 
                |> List.map(fun x -> x.GetType().Name, x) 
                |> Map.ofList;
            Base = BaseDiameter 25<mm>;
            Rules = [Description {Name = "Lurker"; Description = "Termagant Lurks when outside synapse"}];
        }

    let TermUnit = {
     unitModels = [Termagant (Guid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE")]
     unitName = "Termagaunts"
     Rules = []
     Deployment=NotDeployed
    }

    let Hormagaunt id  = 
        {   Name="Hormagaunt"; 
            Id=id;
            Characteristic = 
                [WeaponSkill    (CharacteristicValue 3);
                BallisticSkill  (CharacteristicValue 3);
                Strength        (CharacteristicValue 3);
                Toughness       (CharacteristicValue 3);
                Wounds          (CharacteristicValue 3);
                Initiative      (CharacteristicValue 3);
                Attacks         (CharacteristicValue 3);
                Leadership      (CharacteristicValue 3);
                InvSaves        (CharacteristicValue 3);
                Saves           (CharacteristicValue 3)] 
                |> List.map(fun x -> x.GetType().Name, x) 
                |> Map.ofList;
            Base = BaseDiameter 25<mm>;
            Rules = [Description {Name = "Lurker"; Description = "Termagant Lurks when outside synapse"}];
        }

    let HormagauntUnit = {
     unitModels = [Hormagaunt (Guid "666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE")]
     unitName = "Hormagaunts"
     Rules = [Description {Name = "Bounding Leap"; Description = "Run 3 extra inches"}]
     Deployment=NotDeployed
    }