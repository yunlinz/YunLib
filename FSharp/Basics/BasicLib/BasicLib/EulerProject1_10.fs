namespace BasicLib

module EulerProject1_10 =
    open BasicLib.IntegerMath

    let Euler001SumMultiples a b N = // Euler #1: finds all common multiples for a and b under N
        [1..N] |> List.map (fun i -> if (IsMultiple i a || IsMultiple i b) then i else 0) |> List.sum
        
    let Euler002SumEvenFibonacci N = // Euler #2: sums all even Fibonacci numbers up to X
        let rec FibsLessThanN (X:List<int>) (N:int) =
            let nextFib = (List.nth X ((List.length X) - 1)) + (List.nth X ((List.length X) - 2))
            if nextFib > N then
                X
            else 
                FibsLessThanN (X@[nextFib]) N
        FibsLessThanN [ 1; 2 ] N |> SumAllEvens

    let Euler003LargestPrimeFactor (x:int64) =
        let rec FactorReduce (q:int64*int64) = // returns t/y if y is a factor of t other wise, go on to the next number
            let (t,y) = q
            if t%y = 0L then
                t/y, y
            else
                t, if y = 2L then 3L else y + 2L
        let rec PrimeFactor (q: int64*int64) = // divide initial number by 2 and subsequently each odd number to reduce it down using prime factorization rule, odd nonprime are already covered by their factors
            let (t,y) = q
            if y * y > t then
                t, y
            else
                PrimeFactor (FactorReduce (t, y))
        let (t, y) = PrimeFactor (x , 2L) // the last 2 prime factors, we pick the larger one
        if t > y then
            t
        else
            y
        // [2L..x] |> Seq.filter (fun i -> x%i = 0L) |> Seq.filter (fun i -> IsPrime i) |> Seq.max  <-- too slow


    let Euler004LargestPalindromeProduct (d:int) = // finds the largest palindromic number that is the product of 2 three digit numbers...this solution is O(d^3) where d is the number of input digits, O(exp(d)) memory usage...
        List.map2 (fun i j -> i*j) [for i in int64(10.**float(d - 1)) .. int64(10.**float(d)-1.) do for j in int64(10.**float(d - 1)) .. int64(10.**float(d)-1.) do yield j]
            [for i in int64(10.**float(d - 1)) .. int64(10.**float(d)-1.) do for j in int64(10.**float(d - 1)) .. int64(10.**float(d)-1.) do yield j]
                |> List.filter (fun i -> IsPalindrome i) |> List.max

    let Euler005SmallestCommonMultiple (n:int64) = // finds of the smallest common multiple of  1...n
        [1L..n] |> List.reduce LCM

    let Euler006SumSquareMinusSquareSum n = // finds the difference between the square of the sum and sum of the squares of 1..n
        let sumOfSquares = [1..n] |> List.sumBy Square
        let squareOfSums = [1..n] |> List.sum |> Square
        squareOfSums - sumOfSquares

    let Euler007NthPrime n = // finds the N-th prime number, assuming it's smaller than a certain number
        let max = 1000000
        let rec HeadPrime c S = // c is a counter where if it is 0 then return the head of S otherwise, use the head of S as sieve to find next prime
            if c = 1 then
                List.head S
            else
                let p = List.head S
                HeadPrime (c - 1) (S |> List.filter (fun i -> i%p <> 0))
        HeadPrime n [2..max]

    let Euler008MaxConsecutiveProduct (s:string) n = // finds the maximum product of n consecutive digits in s, which reprsents a number with more than n digits
        let SlidingWindowHelper i = // calculates the product of the sliding windows starting from the ith element
            let substring = s.Substring(i, n)
            substring.ToCharArray() |> Array.toList |> List.map (fun c -> int(c.ToString())) |> List.reduce (*)
        [ for i in 0..(s.Length - n) -> SlidingWindowHelper i] |> List.max

    let Euler009PythagoreanTriple n = //finds product of a pythagorean tripe if their sum is n
        let allTriples = // get all set of possible triples as list of list of ints
            [ for a in n/3 .. n do
                for b in 2 .. a do
                    for c in 1 .. b do
                        if a + b + c = 1000 then
                            yield [a;b;c]
            ]
        allTriples |> List.filter IsPythagoreanTriple |> List.head |> List.reduce (*) // taking head since there is only 1 
    
    let Euler010SumPrimesUnderN n =
        let rec AddAndSieve (P:int list) (c:int64) = // adds the head of the list and sifts through the rest
            match P with
            | [] -> c
            | a::b -> AddAndSieve (P |> List.filter (fun i -> i%a <> 0)) (c + int64(a))
        AddAndSieve [2..n] 0L