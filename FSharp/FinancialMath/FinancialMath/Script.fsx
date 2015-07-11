// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r @"..\packages\MathNet.Numerics.3.7.0\lib\net40\MathNet.Numerics.dll"
#r "System.Windows.Forms.DataVisualization.dll"
#r @"..\packages\FSharp.Charting.0.90.12\lib\net40\FSharp.Charting.dll"
#r "System.Drawing.dll"

#load "Library1.fs"
open FinancialMath.PathGenerator
open FinancialMath.OptionPricing
open FSharp.Charting
open System.Drawing
open System

let K = 100.
let T = 0.25
let M = 100000
let N = 100
let payoff = asian_call K
let S0 = 100.0
let sigma = 0.4
let r = 0.02
let rnd = (new System.Random())

let C = price_option rnd S0 r sigma T N M payoff

//let paths = FinancialMath.PathGenerator.generate_GBM_paths_by_log rnd S0 r sigma T N M
//
//let mx, mn = paths |> Array.fold (fun (mx, mn) p -> 
//                                                    (max mx (Array.max p), min mn (Array.min p)))
//                                                    (Double.MinValue, Double.MaxValue)
//
//let colors = [| for i in 1..M -> Color.FromArgb(rnd.Next(256), rnd.Next(256), rnd.Next(256)) |]
//
//let path_charts = Array.map2 (plot_path T N) paths colors
//
//let title = sprintf 
//                "3 simulated GBM paths"
//
//let chart = Chart.Combine path_charts
//            |> Chart.WithStyling(Margin =( 2.0, 12.0, 2.0, 2.0))
//            |> Chart.WithTitle (Text = title, FontName = "Arial", FontSize = 14.0, FontStyle = FontStyle.Bold, InsideArea = false)
//            |> Chart.WithXAxis(Title = "time (yr)", Max = T, Min = 0.0, TitleAlignment = StringAlignment.Center, TitleFontName = "Arial", TitleFontStyle = FontStyle.Bold)
//            |> Chart.WithYAxis(Title = "price $", Max = (Math.Round(mx) + 1.0), Min = (Math.Round(mn) - 1.0), TitleAlignment = StringAlignment.Center, TitleFontName = "Arial", TitleFontStyle = FontStyle.Bold)
//
//chart.ShowChart()

// Define your library scripting code here

