namespace BasicLib

module FloatMath =

    let BinomialCumulative x n p =
        let f = float

        let essentiallyZero = 1.E-12
        
        //find n*p round to nearest whole number
        let m = (f(n) * p) |> truncate |> int 

        let CalcCurrent value k =
            if k > m then
                value * f(n - k + 1) * p / (f(k) * (1. - p)) // for unscaled probabilities k > m
            else
                value * f(k + 1) * (1. - p) / (f(n - k) * p)

        let CalcUnscaled x k acc incre = 
            if k <= x then acc + incre
            else acc

        let Done current = current <= essentiallyZero

        let NextK k = if k > m then k + 1 else k - 1

        let rec Calculate k totalUnscaledProbability previous unscaled = 
            let current = CalcCurrent previous k
            let totalUnscaledProbability' = totalUnscaledProbability + current
            let unscaled' = CalcUnscaled x k unscaled current
            if Done current then
                unscaled', totalUnscaledProbability'
            else 
                Calculate (NextK k) totalUnscaledProbability' current unscaled'

        let InitialUnscaled = if (m <= x) then 1. else 0.

        let UnscaledResultAboveM, TotalUnscaledProbabilityAboveM =
            Calculate (m + 1) 1. 1. InitialUnscaled

        let UnscaledResult, TotalUnscaledProbability = 
            Calculate (m - 1) TotalUnscaledProbabilityAboveM 1. UnscaledResultAboveM

        UnscaledResult / TotalUnscaledProbability

    let BinomialDensity x n p =
        match x with
        | 0 -> 0.
        | _ -> BinomialCumulative x n p - BinomialCumulative (x - 1) n p

    let GoldenRatio = (1. + sqrt(5.))/2.

    let Fib n = ((GoldenRatio)**float(n) - (-1./GoldenRatio)**float(n)) / (sqrt 5.)

