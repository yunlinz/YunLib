namespace BasicLib

module EulerProject21_30 =
    open BasicLib.IntegerMath
    open BasicLib.FloatMath

    let Euler021SumAmicablePairs n = // a number has a amicable pair iff d(d(n)) = n, unless d(n) = n
        [1..n] |> List.map SumFactors |> List.mapi (fun i x -> if x=(i+1) then 0 else x) |> List.map SumFactors |> List.mapi (fun i x -> if x=(i+1) then x else 0) |> List.sum

    let Euler022SumNameScores (S: string list) = // stores the rank values to a hashmap for quicklookup
        let stringVal (s:string) = s.ToCharArray() |> Seq.toList |> List.sumBy (fun c -> int(c) - 64)
        let alphaOrder = S |> List.sort |> List.mapi (fun i s -> (s, i+1)) |> Map.ofList
        let TotalVal s = alphaOrder.[s] * stringVal s
        S |> List.sumBy TotalVal

    let Euler023NonAbundantSum upperLimit =
        let IsAbundant x = 
            let sumFactors = SumFactors x
            if sumFactors > x then true
            else false
        let abundantNumbers = [1 .. upperLimit] |> List.filter IsAbundant
        let abundantNumberSum = 
            abundantNumbers
            |> Seq.collect (fun n -> abundantNumbers |> List.map (fun m -> m + n))
            |> Seq.filter( fun n -> n <= upperLimit)
            |> Seq.distinct
            |> Seq.toList |> Seq.sum
        (upperLimit * (upperLimit + 1)) / 2 - abundantNumberSum

    let Euler024LexigraphicalOrder n = // finds the nth lexigraphically ordered permutation of 0123456789
        let ToString n = n.ToString(); 
        let factorials = [ 1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880] // memoized the first 9 factorials
        let rec Factorial n = 
            if n < 0 then 1
            else List.nth factorials n
        let rec PickNext (C: string list) (S:string list) n = // to find the next lexigraphical digit, we have to keep in mind that we need to exhaust all possible combinations before it, while is (n-1)!
            let numCombo = Factorial((S |> List.length) - 1)
            let posPicked = n / (numCombo + 1)
            let digitPicked = List.nth S posPicked
            if n <= 1 then C@S
            else 
                PickNext (C@[digitPicked]) (S |> List.filter (fun s -> s <> digitPicked)) (n - posPicked * numCombo)
        let numbers = [0..9] |> List.map ToString
        PickNext [] numbers n

    let Euler025FirstFibWithNDigits n = // using the constant time calculation of nth fibonacci number using Binet's formula, as n gets very large, psi^n->0 => F(n)->phi^n/sqrt5, so we take log10 of that and finds the first term that is greater than 1000
        int(Ceiling((float(n - 1) + 0.5 * Log10(5.)) / Log10((1. + sqrt 5.)/2.)))