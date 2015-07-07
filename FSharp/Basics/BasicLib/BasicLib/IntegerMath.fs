namespace BasicLib

module IntegerMath =
    
    let IsMultiple a b = // determines if a is a multiple of b
        match a%b with
        | 0 -> true
        | _ -> false
        
                  
    let SumAllEvens X = X |> List.filter(fun i -> i%2 = 0) |> List.sum

    let SumAllOdd X = X |> List.filter(fun i -> i%2 > 0) |> List.sum

    let IsPrime (x:int64) = ([for i in 2L..int64(sqrt (float x)) -> i] |> List.filter (fun i -> x%i = 0L) |> List.length) = 0

    let IsPalindrome (x:int64) = // determines is a number is a palindrome ie 9009
        let Reverse (x:int64) = // reverses the digits of x -> x'
            let rec AddDigit (x':int64) (y:int64) =
                if y = 0L then
                    x'
                else 
                    AddDigit (x' * 10L + y%10L) (y/10L)
            AddDigit 0L x
        let x' = Reverse x
        if x = x' then
            true
        else
            false
    let rec GCD (x:int64) (y:int64) = //greatest common divisor
        if y=0L then
            x
        else GCD y (x%y)

    let LCM (x:int64) (y:int64) = // least common multiple
        x * y / GCD x y

    let Square x = 
        x * x

    let IsPythagoreanTriple (S: int list) = //checks whether a list of 3 numbers is a pythagorean triple
        match S |> List.sort with
        | [ a; b; c ] -> Square a + Square b = Square c
        | _ -> false

    let PrimeFactorization x = // finds the prime factorization of x storing all occurrences of each factor
        let rec PrimeReduce x y = // is a large number, y current number in sequence of potential factors to try
            if x < y*y then
                [x]
            else if x % y = 0 then
                y :: PrimeReduce (x/y) y
            else 
                PrimeReduce x (y + 1)
        PrimeReduce x 2

    let NumberOfFactors n =
        let GetTupleSecondElement (z: int * int) =
            let (_, b) = z
            b
        n |> PrimeFactorization |> Seq.countBy id |> Seq.toList |> List.map GetTupleSecondElement |> List.map (fun x -> x + 1) |> List.reduce (*)

    let NumberOfDigits (n: int64) = 
        int(System.Math.Log10(double n)) + 1

    let CollatzChainSize x = // length of Collatz Chain starting at x
        let rec ChainLink x n =
            if x = 1 then
                n
            else if x%2 = 0 then
                ChainLink (x / 2) (n + 1)
            else 
                ChainLink (3*x + 1) (n + 1)
        ChainLink x 1

    let ReverseDigits n =
        let rec ReverseDigitHelper n y =
            if n = 0 then y
            else ReverseDigitHelper (n / 10) (y*10 + n % 10)
        ReverseDigitHelper n 0

    let IsEven n = 
        n%2 = 0

    let bigint (x:int) = bigint(x);

    let rec Factorial (n:int) : bigint =
        match n with 
            | 1 -> bigint(1)
            | _ -> bigint(n) * Factorial (n-1)

    let SumFactors n =
        [for i in 1 .. int(sqrt (float n)) do 
            if n % i = 0 then 
                yield i
                if ((n/i) <> i) && i<>1 then
                    yield n/i
            else yield 0] |> List.sum