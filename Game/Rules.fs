namespace Domain

module WarhammerRules =
    open Domain.WarhammerDomain
    open Distribution.Distribution
    open Distribution.Probability

    type 'T Rule = {Calculation: ('T Distribution list -> 'T Distribution); 
                    Check:('T->bool)} with
                    member this.Average f = 
                        this.Calculation >> average f
                    member this.Probability = this.Calculation >> filter this.Check >> probability



    let SumOfValues = sequenceDistributionA >> map Seq.sum
    
    //LD >= Result
    let MortalCheck ld = 
        ld |> (>=)     
    
    let RunePriest = 
        let ld = 10
        let Moral = {Calculation=SumOfValues; Check=MortalCheck ld}
        [Moral]
    
    printfn "%A" ([D6; D6] |> RunePriest.Head.Probability) |> ignore
    printfn "%A" ([D6; D6] |> RunePriest.Head.Average (float)) |> ignore

        
//
//type Rank = int
//type Suit = | Spades | Hearts | Diamonds | Clubs
//type Card = Rank * Suit
//
//let value = fst
//let suit = snd
//
//let A,K,Q,J,T = 14,13,12,11,10
//let allRanksInSuit suit = [2..A] |> List.map (fun rank -> rank,suit)
//let completeDeck = 
//  [Spades; Hearts ; Diamonds; Clubs] 
//    |> List.map allRanksInSuit 
//    |> List.concat
//let isPair c1 c2 = value c1 = value c2
//let isSuited c1 c2 = suit c1 = suit c2
//let isConnected c1 c2 = 
//    let v1,v2 = value c1,value c2
//    (v1 - v2 |> abs |> (=) 1) ||
//    (v1 = A && v2 = 2) ||
//    (v1 = 2 && v2 = A)

//// draw Ace of Clubs and Ace of Spaces in order
//completeDeck 
//  |> select 2
//  |> filter ((=) [A,Clubs; A,Spades])
//  |> probability 
//  |> printfn "%A"  // prints "1/2652"
//
//// draw Ace of Clubs and Ace of Spaces in any order
//completeDeck 
//  |> select 2
//  |> filterInAnyOrder [A,Clubs; A,Spades]    
//  |> probability
//  |> printfn "A"  //  prints "1/1326"

