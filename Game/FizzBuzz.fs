namespace FizzBuzz
module FizzBuzz =
    type Drink<'c,'u> = Carbonated of 'c | Uncarbonated of 'u
    let bind f x  = match x with | Carbonated y -> Carbonated y | Uncarbonated y -> f y
    let (>=>) s1 s2 = s1 >> bind s2
    let either carbFunc uncarb = function | Carbonated c -> carbFunc c | Uncarbonated i -> uncarb i
    let Rules = [15, "FizzBuzz";5,"Fizz";3, "Buzz"]
    let Test (num,replace) = function | x when x % num = 0 -> Carbonated replace | x -> Uncarbonated x
    let Fizzer = Rules |> List.map Test |> List.reduce (>=>) >> either (sprintf "%s") (sprintf "%i")
    [1..100] |> List.map Fizzer |> List.iter (printfn "%A")
