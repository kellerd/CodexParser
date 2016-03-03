#if !INTERACTIVE 
namespace Impl
module ModelImplTest = 
    open Domain.WarhammerDomain
#endif
    open System
    let Termagant = 
        {   Name="Termagant"; 
            Id=Guid("666D7AF7-D74B-49B8-B7B9-EFFF44D77ACE");
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
            Rules = [Description {Name = "Lurker"; Description = "Termagant Lurks when outside synapse"}];
        }