namespace FinancialMath

module PathGenerator =
    open MathNet.Numerics.Distributions
    open System.Windows.Forms.DataVisualization
    open System.Drawing
    open FSharp.Charting

    let get_dW rnd dt N =
        let dW = Normal.WithMeanVariance(0. , dt)
        dW.RandomSource <- rnd
        (fun () -> Array.init N (fun _ -> dW.Sample()))

    let generate_GBM_paths_by_log rnd S0 r sigma T N M =
        let dt = T/(float N)
        let drift = (r - 0.5 * (sigma ** 2.0)) * dt
        let generator = get_dW rnd dt N

        Array.init M (fun _ -> generator()
                                |> Array.map (fun dWt -> drift + sigma * dWt)
                                |> Array.scan (+) 0.0
                                |> Array.map (fun x -> S0 * exp(x)) )

    let plot_path (T:float) (N:int) (path:float array) color =
        let dt = T / (float N)
        path |> Array.mapi (fun n p -> ((float n) * dt, p))
             |> Chart.Line
             |> Chart.WithStyling (Color = color, BorderWidth = 2)

    let S_T (path:float array) = path.[path.Length - 1]

    let european_call K (path: float array) = max ((S_T path) - K) 0.0

    let up_and_out_call K H (path:float array) = 
        if Array.max path.[1..] >= H then 0.0
        else european_call K path

    let simulate_payoffs rnd S0 r sigma T N M payoff = 
        [| for path in generate_GBM_paths_by_log rnd S0 r sigma T N M ->
                let currentPayoff = payoff path
                (exp(-r*T)) * currentPayoff  |]

    let price_option rnd S0 r sigma T N M payoff = 
        simulate_payoffs rnd S0 r sigma T N M payoff |> Array.average