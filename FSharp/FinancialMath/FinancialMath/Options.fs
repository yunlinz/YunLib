namespace FinancialMath
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open System.Windows.Forms.DataVisualization
open System.Windows.Forms
open System.Drawing
open System
open FSharp.Charting

module PathGenerator =

    let get_dW rnd dt N = //samples dW from a normal distribution
        let dW = Normal.WithMeanVariance(0. , dt)
        dW.RandomSource <- rnd
        (fun () -> Array.init N (fun _ -> dW.Sample()))

    let generate_GBM_paths_by_log rnd S0 r sigma T N M = //simple brownian motion
        let dt = T/(float N)
        let drift = (r - 0.5 * (sigma ** 2.0)) * dt
        let generator = get_dW rnd dt N

        Array.init M (fun _ -> generator()
                                |> Array.map (fun dWt -> drift + sigma * dWt)
                                |> Array.scan (+) 0.0
                                |> Array.map (fun x -> S0 * exp(x)) )

    let generate_GBM_paths_by_log_AV rnd S0 r sigma T N M = // antithetic variates method for geomtric brownian motion
        let dt = T/(float N)
        let drift = (r - 0.5 * (sigma ** 2.0)) * dt
        let dW = get_dW rnd dt N
        let dWs = Array.init M (fun _ -> dW())
        let negated_dWs = 
            dWs |> Array.map (fun x ->
              x |> Array.map (fun y -> - y))
        let generate_path dWs =
            dWs 
            |> Array.map (fun dWt -> drift + sigma * dWt)
            |> Array.scan (+) 0.0
            |> Array.map (fun x -> S0 * exp (x))

        let path_tuple = Array.map2 (fun x y -> (generate_path x, generate_path y)) dWs negated_dWs
        [| for i in 0..(path_tuple.Length-1) do 
            yield fst path_tuple.[i]
            yield snd path_tuple.[i] |]


module OptionPricing = // K is strike, H is barrier, where applicable
    open PathGenerator

    let S_T (path:float array) = path.[path.Length - 1] 
    
    let european_call K (path: float array) = max ((S_T path) - K) 0.0

    let up_and_out_call K H (path:float array) = 
        if Array.max path.[1..] >= H then 0.0
        else european_call K path

    let up_and_in_call K H (path: float array) =
        if Array.max path.[1..] <= H then 0.0
        else european_call K path

    let asian_call K (path:float array) =
        let S_avg = path.[1..] |> Array.average
        max (S_avg - K) 0.0

    let simulate_payoffs rnd S0 r sigma T N M generator payoff = 
        [| for path in generator rnd S0 r sigma T N M ->
                let currentPayoff = payoff path
                (exp(-r*T)) * currentPayoff  |]

    let price_option rnd S0 r sigma T N M generator payoff = //calculates just the payoff from taking the average of all simulated paths
        simulate_payoffs rnd S0 r sigma T N M generator payoff |> Array.average

    let price_option_2 rnd S0 r sigma T N M generator payoff = //calculates the payoff, variance and std error of the paths
        let Ys = simulate_payoffs rnd S0 r sigma T N M generator payoff
        let C_estimate = Ys |> Array.average
        let Y_var = Ys.Variance();
        let std_error = sqrt( Y_var / (float M))
        (C_estimate, Y_var, std_error)



module Visualization =
    open PathGenerator

    let plot_path (T:float) (N:int) (path:float array) color =
        let dt = T / (float N)
        path |> Array.mapi (fun n p -> ((float n) * dt, p))
             |> Chart.Line
             |> Chart.WithStyling (Color = color, BorderWidth = 2)

    let draw_chart (rnd:System.Random) S0 r sigma T N M generator = // plots the path generated
        let paths = generator rnd S0 r sigma T N M
        let mx, mn = paths |> Array.fold (fun (mx, mn) p -> 
                                                    (max mx (Array.max p), min mn (Array.min p)))
                                                    (Double.MinValue, Double.MaxValue)
        let colors = [| for i in 0..(paths.Length - 1) -> Color.FromArgb(rnd.Next(256), rnd.Next(256), rnd.Next(256)) |]
        let path_charts = Array.map2 (plot_path T N) paths colors
        let title = sprintf "%d simulated paths S0=%.2f  r=%.2f  sigma=%.2f  T=%.2f  N=%d" M S0 r sigma T N
        let chart = 
            Chart.Combine path_charts 
            |> Chart.WithStyling(Margin =( 2.0, 12.0, 2.0, 2.0))
            |> Chart.WithTitle (Text = title, FontName = "Arial", FontSize = 14.0, FontStyle = FontStyle.Bold, InsideArea = false)
            |> Chart.WithXAxis(Title = "time (yr)", Max = T, Min = 0.0, TitleAlignment = StringAlignment.Center, TitleFontName = "Arial", TitleFontStyle = FontStyle.Bold)
            |> Chart.WithYAxis(Title = "price $", Max = (Math.Round(mx) + 1.0), Min = (Math.Round(mn) - 1.0), TitleAlignment = StringAlignment.Center, TitleFontName = "Arial", TitleFontStyle = FontStyle.Bold)

        chart.ShowChart()