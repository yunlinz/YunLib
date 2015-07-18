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

    let Euler026LongestRepeatingReciprocal n = // finds the number below n such that 1/n has the longest repeating numbers...do long division until we find the same remainder
        let rec LongDivide numer n rem =
            if (List.exists (fun x -> x = numer) rem) || numer = 0 then 
                List.length rem
            else 
                let rec IncreaseBase m d =
                    if m >= d then m
                    else IncreaseBase (10*m) d
                LongDivide ((IncreaseBase numer n) % n) n (rem@[numer])
        ([1..n] |> List.map (fun x -> LongDivide 1 x []) 
               |> List.mapi (fun i x -> i,x) 
               |> List.maxBy snd
               |> fst) + 1
    
    let Euler027PrimeQuadratics amax bmax = // finds product ab where the number of primes is maximized given maximum value for a and for b
        // notice that sequence for n has to start at 0 therefore b has to be prime
        let rec PrimeSequence nlist i = // assumes i-th element of list is prime, use sieve of erosthenes to get other primes
            if i >= List.length nlist then nlist
            else
                let nthItem = List.nth nlist i 
                PrimeSequence (nlist |> List.filter (fun x -> (x = i || x % nthItem <> 0))) (i + 1)
        let bvals = PrimeSequence [2..bmax] 0
        //now can we eliminate some a's? if n=1, then 1+a+b is prime, other than the case of b=2, b is odd, and a has to be odd
        //now let's eliminate b = 2 since for the n = 1 case, a has to be even and n=2 case, (6 + 2a) only -2 can make this prime, and the chain is very short lived
        //rearranging some terms, n^2 + a*n + b = prime => a has to be odd
        let ClosestOdd x = 
            if (x % 2 = 0) then (x - 1)
            else x
        let aList = [-(ClosestOdd amax)..(ClosestOdd amax)] 
        let rec GetPrimeQuadratics a b n =
            if (not(IsPrime(int64( n * n + a * n + b)))) then (n - 1)
            else GetPrimeQuadratics a b (n + 1)
        let third (_,_,c) = c
        let TupleProduct (a, b, c) = a*b
        bvals |> List.map (fun b -> (aList |> List.map (fun a -> (a, b, GetPrimeQuadratics a b 1)) |> List.maxBy third)) |> List.maxBy third  |> TupleProduct