
namespace BasicLib

module EulerProject11_20 = 
    open BasicLib.IntegerMath
    
    let Euler011MaxLineProd (X: List<List<int>>) m = // finds the maximum product of m numbers in all possible directions from each element
        let rec StencilLine i j p n (D:string) = // from the i,j element, calculates the product of it and n-1 elements to the right, p keeps track of the product and D is the direction
            try 
                let elem = X.[i].[j]
                if n = 1 then
                    elem * p
                else 
                    if D="right" then
                        StencilLine i (j + 1) (elem * p) (n - 1) D
                    else if D="down" then
                        StencilLine (i + 1) j (elem * p) (n - 1) D
                    else if D="rightdown" then
                        StencilLine (i + 1) (j + 1) (elem * p) (n - 1) D
                    else if D="leftdown" then
                        StencilLine (i + 1) (j - 1) (elem * p) (n - 1) D
                    else 
                        0
            with
                | :? System.IndexOutOfRangeException as e -> 0 
        let MaxAtElement i j =
            [StencilLine i j 1 m "right";
             StencilLine i j 1 m "down";
             StencilLine i j 1 m "rightdown";
             StencilLine i j 1 m "leftdown" ] |> List.max
        
        let y = [ for i in 0..(X.Length - 1) do
                    for j in 0..(X.[1].Length - 1) do
                        yield MaxAtElement i j ]
        y |> List.max

    let Euler012TriangleNumberWithNFactors n =
        let IthTriangle i = // triangle numbers are just partial sum of the arithmetic sequence i
            i * (i + 1) / 2
        let rec TestNumber m = // tests whether the mth triangle number has n or more factors
            let x = IthTriangle m
            if (x |> NumberOfFactors) >= n then
                x
            else 
                TestNumber (m + 1)
        TestNumber 1

    let Euler013FirstNDigitsOfSum (X:int64 list) n = // finds the first n digits of a list of integers. 
        // basic concept is to first determine whether we'll end up with n digits
        // then reduce each integer by a power such that the numbers in the decimal will not affect the results so that we don't run into overflow
        // this is a divisor that is calculated by number of digits of x - n + length of X
        let rec ReduceDigits (x:int64) =
            if NumberOfDigits x > n then
                ReduceDigits (x / 10L)
            else 
                x
        if (int((X |> List.max |> fun x -> NumberOfDigits x))) * int(X |> List.length) < n then
            -1L
        else
            let power = int((X |> List.max |> fun x -> NumberOfDigits x)) - n + int(X|>List.length)
            let divisor = int64(10.**float power)
            X |> List.map (fun x -> x/divisor) |> List.sum |> ReduceDigits
        
    let Euler014LongestCollatzChainUnderN n = // finds the number under n such that it has the longest collatz chain
        let chainSizes = [1..n] |> List.map CollatzChainSize
        (chainSizes |> List.findIndex (fun x -> x = (chainSizes |> List.max))) + 1 

    let Euler015PossiblePathsInASquare n = // finds the number of possible paths from upper left corner to lower right corner going only right and down, dynamic programming approach...
        let grid = Array2D.create (n+1) (n+1) 1L
        for i in 1..n do
            for j in 1..n do
                grid.[i,j] <- grid.[i-1,j] + grid.[i,j-1]
        grid.[n,n]

    let Euler016SumDigitsTwoToTheN n = 
        (2I**n).ToString() |> Seq.map (fun c -> int(c.ToString())) |> Seq.sum

    let Euler017NumbersToCharacters n = // counts the number of characters in all numbers expressed as words
        let onesDigit a = a % 10
        let tensDigit a = (a / 10) % 10
        let hundredsDigit a = a / 100
        let hundredChars a =
            let hundredsWord =
                match hundredsDigit a with
                    | 1 -> "onehundred"
                    | 2 -> "twohundred"
                    | 3 -> "threehundred"
                    | 4 -> "fourhundred"
                    | 5 -> "fivehundred"
                    | 6 -> "sixhundred"
                    | 7 -> "sevenhundred"
                    | 8 -> "eighthundred"
                    | 9 -> "ninehundred"
                    | _ -> ""
            hundredsWord.Length
        let restChars a =
            if tensDigit a = 1 then
                let lastTwoDigits = (tensDigit a) * 10 + onesDigit a
                let lastTwoWords = 
                    match lastTwoDigits with
                        | 10 -> "ten"
                        | 11 -> "eleven"
                        | 12 -> "twelve"
                        | 13 -> "thirteen"
                        | 14 -> "fourteen"
                        | 15 -> "fifteen"
                        | 16 -> "sixteen"
                        | 17 -> "seventeen"
                        | 18 -> "eighteen"
                        | 19 -> "nineteen"
                        | _ -> ""
                lastTwoWords.Length
            else
                let tensWord =
                    match tensDigit a with
                        | 0 -> ""
                        | 2 -> "twenty"
                        | 3 -> "thirty"
                        | 4 -> "forty"
                        | 5 -> "fifty"
                        | 6 -> "sixty"
                        | 7 -> "seventy"
                        | 8 -> "eighty"
                        | 9 -> "ninety"
                        | _ -> ""
                let onesWord =
                    match onesDigit a with
                        | 0 -> ""
                        | 1 -> "one"
                        | 2 -> "two"
                        | 3 -> "three"
                        | 4 -> "four"
                        | 5 -> "five"
                        | 6 -> "six"
                        | 7 -> "seven"
                        | 8 -> "eight"
                        | 9 -> "nine"
                        | _ -> ""
                tensWord.Length + onesWord.Length
        let andChars a =
            if ((onesDigit a <> 0) || (tensDigit a <> 0)) && (hundredsDigit a <> 0) then
                3
            else
                0
        let totalChars a = 
            if a = 1000 then
                ("onethousand").Length
            else
                (hundredChars a) + (restChars a) + (andChars a)
        if n > 1000 then
            -1
        else 
            [1..n] |> List.map totalChars |> List.sum

    let Euler018MaxPyramidPath (X: int list list) = // given a list of list of tiers of the pyramid, calculate the paths that gives the greatest sum
        let emptyPyramid = [for i in 0..X.Length do yield 0 ] // creates an empty pyramid of length r + 1, where r is the height of X to use as base for our dynamic algorithm
        let rec AddMaxNeighborNextRow r (filledPyramid: int list) = //starts at the bottom of the pyramid and work your way up
            let takeMax i = 
                let baseVal = X.[r].[i]
                [baseVal + filledPyramid.[i]; baseVal + filledPyramid.[i+1]] |> List.max
            if r = -1 then
                filledPyramid.[0]
            else
                AddMaxNeighborNextRow (r-1) [for i in 0..r do yield takeMax i ]
        AddMaxNeighborNextRow (X.Length - 1) emptyPyramid

    
    let Euler019FirstOfMonthOnSundays s e = // calculates the number of sundays that are on the first of a month from 1/1/s to year 12/31/e, s,e>1900
        let leapYear y =
            if y%100 = 0 then
                if y%400 = 0 then
                    true
                else
                    false
            else 
                if y%4 = 0 then
                    true
                else
                    false
        let numDaysInMonth m =
            match m with 
                | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
                | 4 | 6 | 9 | 11 -> 30
                | 2 -> 28
                | _ -> 0
        let HowManyDaysSinceOne y m d = //calculates the number of days since 1900/1/1
            let temp =
                if m > 2 then y 
                else y - 1
            (y - 1900) * 365 + ([for i in 1..(m-1) -> i] |> List.map numDaysInMonth |> List.sum) + d + ([1900..temp] |> List.filter leapYear |> List.length)
        let IsSunday d = //finds whether d days after 1900/1/1 is Sunday
            d % 7
        let WhatDay y m d =
            let days = HowManyDaysSinceOne y m d
            days % 7
        let sundaysTilY y = [ for i in 1900..y do
                                for j in 1..12 do yield (WhatDay i j 1)] |> List.filter (fun x -> x=0) |> List.length
        (sundaysTilY e) - (sundaysTilY (s-1))

    let Euler020SumDigitsFactorial n =
        (Factorial(n)).ToString().ToCharArray() |> Seq.sumBy (fun c -> int(c.ToString()))
