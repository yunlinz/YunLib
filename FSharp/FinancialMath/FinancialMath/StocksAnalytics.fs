namespace FinancialMath

module StocksAnalytics = 
    open System
    open System.Net
    open System.Windows.Forms
    open System.Windows.Forms.DataVisualization
    open System.Windows.Forms.DataVisualization.Charting
    open Microsoft.FSharp.Control.WebExtensions


    let PlotStock ticker = 
        let chart = new Chart(Dock = DockStyle.Fill)
        let area = new ChartArea("Main")
        chart.ChartAreas.Add(area)

        let mainForm = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
        do mainForm.Text <- "Yahoo Finance Data"
        mainForm.Controls.Add(chart)

        let stockPrice = new Series("StockPrice")
        do stockPrice.ChartType <- SeriesChartType.Line
        do stockPrice.BorderWidth <- 2
        do stockPrice.Color <- Drawing.Color.Red
        chart.Series.Add(stockPrice)

        let movingAvg = new Series("Moving Average")
        do movingAvg.ChartType <- SeriesChartType.Line
        do movingAvg.BorderWidth <- 2
        do movingAvg.Color <- Drawing.Color.Blue
        chart.Series.Add(movingAvg)

        let fetchOne() =
            let uri = new System.Uri("http://real-chart.finance.yahoo.com/table.csv?s="+ticker+"&d=6&e=18&f=2015&g=d&a=11&b=12&c=1980&ignore=.csv")
            let client = new WebClient()
            let html = client.DownloadString(uri)
            html

        let getPrices() = 
            let data = fetchOne()
            data.Split('\n')
            |> Seq.skip 1
            |> Seq.map (fun s -> s.Split(','))
            |> Seq.map (fun s -> float s.[4])
            |> Seq.truncate 2500

        let movingAverage n (prices:seq<float>) =
            prices |> Seq.windowed n |> Seq.map Array.sum |> Seq.map (fun a -> a / float n)

        let sp = getPrices()
        do sp |> Seq.iter(stockPrice.Points.Add >> ignore)

        let ma = movingAverage 100 sp
        do ma |> Seq.iter(movingAvg.Points.Add >> ignore)



