module App

open Fable.React
open Fable.Core

open GP
open Browser.Dom
open System

open Feliz
open Feliz.Plotly
open Fable.React
open Fable.React.Props
open Browser.Types


let computeGP (f: float->float) (kernel: float->float->float) (train: seq<float>) (test: seq<float>) =
    // Train

    let X = train |> Seq.toArray
    let Y = X |> Array.map f


    // All
    let X2 = test |> Seq.toArray
    let Y2 = X2 |> Array.map f

    let actual = Seq.zip X2 Y2

    let g = gp kernel X Y


    let XX =
        [|-90..300|] |> Seq.map (fun x -> (float x)*0.1 )

    let infered = g XX |> Seq.toList

    (X, Y, actual, infered)

let quantileOf (p:float) (xs: seq<float*float*float>) =
    (xs |> Seq.map (fun (x, mu, sigma) -> (x, quantile p mu sigma)))

let polyLine (name: string) (xs: seq<float*float>) =
    let xs = xs|> Seq.cache
    traces.scatter [
        scatter.x (xs|> Seq.map fst)
        scatter.y (xs|> Seq.map snd)
        scatter.mode [
            scatter.mode.lines
        ]
        scatter.name name
        scatter.line [
            Plotly.line.shape.linear
        ]
    ]

let boundary (name:string) (xs: seq<float*float>) =
    let xs = xs|> Seq.cache
    traces.scatter [
        scatter.x (xs|> Seq.map fst)
        scatter.y (xs|> Seq.map snd)
        scatter.fill.tonexty
        scatter.name name
    ]



let markers (name: string) (xs: seq<float*float>) =
    let xs = xs|> Seq.cache
    traces.scatter [
        scatter.x (xs|> Seq.map fst)
        scatter.y (xs|> Seq.map snd)
        scatter.mode [
            scatter.mode.markers
        ]
        scatter.name name
    ]

  
[<Emit("new Function('x', 'return ' + $0)")>]
let compileFormula (_:string):(float->float) = jsNative


let chart () = FunctionComponent.Of(fun _ ->
    let formulaRef = React.useRef None
    let trainRef = React.useRef None
    let selectRef = React.useRef None

    let lRef = React.useRef None
    let pRef = React.useRef None

    let trainData, setTrainData = React.useState [|0.0;1.0;2.0;3.0;4.0;5.0;6.0|]
    let formula, setFormula = React.useState "x*Math.sin(x)-x"


    let (kId, l, p), setKernel = React.useState ( (0, 1.0, 6.28) )
    let kernel = 
        match kId with
        | 0 -> gauss l
        | 1 -> periodic l p
        | 2 -> periodicGauss l p
        | _ -> gauss l
    
    
    let formulaInput = input [
      DefaultValue "x*Math.sin(x)-x"; 
      RefValue formulaRef
    ]

    let trainInput = input [
      Style [MinWidth "300px"]
      DefaultValue "-1 0 1 2 3 7 8 9 9.5 9.7 10 15 20"; 
      RefValue trainRef
    ]

    let lInput = input [
      DefaultValue "1.0"; 
      RefValue lRef
    ]

    let pInput = input [
      DefaultValue "6.28"; 
      RefValue pRef
    ]
 
    let kernelSelect = 
        select [
          DefaultValue 0; 
          RefValue selectRef
        ] [
            option [Value 0] [str "gauss: exp(-(x-y)^2/l)"]
            option [Value 1] [str "periodic: exp(-sin(|x-y|/p)^2/l)"]
            option [Value 2] [str "gauss * periodic"]
        ]
    

    let inline updateFormula (_) =
        match formulaRef.current with
        | Some e -> setFormula ((e :?> HTMLInputElement).value)
        | None -> ()

        match trainRef.current with
        | Some e -> 
            let text = (e :?> HTMLInputElement).value
            let points = 
                (text.Split([|' '; ','; '\n'|], StringSplitOptions.RemoveEmptyEntries))
                |> Array.map float
            setTrainData(points)
        | None -> ()

        match selectRef.current, lRef.current, pRef.current with
        | Some s, Some l, Some p  -> 
            let selection = (s :?> HTMLSelectElement).value
            let l = (l :?> HTMLInputElement).value |> float
            let p = (p :?> HTMLInputElement).value |> float
            match selection with
            | "0" -> setKernel (0, l, p)
            | "1" -> setKernel (1, l, p)
            | "2" -> setKernel (2, l, p)
            | _ -> ()
        | _ -> ()


    let f = compileFormula formula
    System.Console.WriteLine formula


    let tMin = trainData|>Seq.reduce((fun a b -> if a>b then b else a))
    let tMax = trainData|>Seq.reduce((fun a b -> if a<b then b else a))

    let plotData = 
        Seq.replicate 200 0
        |> Seq.mapi (fun i _ -> (float i)/200.0 * (tMax - tMin) * 1.5 - 5.0)

    kernel 1.0 1.0 |> System.Console.WriteLine

    let (x, y, actual, infered) = computeGP f kernel trainData plotData
    

    div [] [
        h1 [] [str ("Learning " + formula)]
        div [] [
            span [] [str "formula"]
            formulaInput
            br []
            span [] [str "list of training points"]
            trainInput
            br []
            kernelSelect
            span[] [str "l:"; lInput]
            span[] [str "p"; pInput]
            br []
            button [OnClick updateFormula] [str "recompute"]
        ]
        Plotly.plot [
            plot.traces [
                traces.scatter [
                    scatter.x (infered |> List.map (fun (x, _,_) -> x))
                    scatter.y (infered |> List.map (fun (_, x,_) -> x))
                    scatter.mode [
                        scatter.mode.lines
                    ]
                    scatter.name "predict"
                    scatter.line [
                        Plotly.line.shape.spline
                    ]
                ]

                infered |> quantileOf 0.9 |> boundary "95%"
                infered |> quantileOf 0.1 |> boundary "5%"

                actual |> polyLine "actual" 
                (Seq.zip x y) |> markers "train"

            ]
            plot.layout [
                layout.legend [
                    Plotly.legend.y 0.5
                    //Plotly.legend.traceorder.reversed
                    Plotly.legend.font [
                        font.size 16
                    ]
                ]
            ]
        ]
    ]
)

let app = chart () []



ReactDom.render (app, document.querySelector("#root"))