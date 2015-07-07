namespace BasicLib

module EulerProject111_120 = 
    open BasicLib.IntegerMath
    let Euler120DivideNthPowerBySquare l r = // finds the sum of r_max for range from l to r
    // recognizing that anything in the numerator above n^2, inclusive, will cancel and not contribute to remaining
    // we only need to do binomial expansion and keep the last two terms, then % it with a^2
    // even though there is a combinatorial to calculate, notice that the 0th term is alway 1 and the 1th term is always n in the combinatorial
    // further more, if n is odd, then the 0th term cancels -- in which case it is always 2*a*n, and if n is even then the 1st term cancels -- in which case it is always 2
    // so to maximize 2*a*n, n has to be the largest integer smaller than a/2
    // so the largest remainder will be a(a-1) for odd a and a(a-2) for even a
        let largestRemainder a =
            match IsEven a with
                | true -> a*(a-2)
                | false -> a*(a-1)
        [l..r] |> List.map largestRemainder |> List.sum