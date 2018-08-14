module NewLineUp

open Aardvark.UI
open Aardvark.UI.Primitives

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.UI
open Aardvark.UI.Operators

open NewLineUpModel
open Aardvark.Base
open Aardvark.Base.MultimethodTest
open System.Linq.Expressions

type Message =
    | Nop
    | AddAttributeAt of (string * int) // ASK HARRI
    | AddAttribute of string
    | RemoveAttribute of string
    | CalculateScore
    | Sort of string

let update (model : Table) (msg : Message) =
    //model
    match msg with
        | AddAttribute key -> 
            let newVisibleOrder =
                match model.visibleOrder |> List.contains key with
                    | false -> model.visibleOrder |> List.append [key]
                    | _ -> model.visibleOrder
            { model with visibleOrder = newVisibleOrder }
        | AddAttributeAt (key, index) -> failwith ""
        | RemoveAttribute key -> 
            let newVisibleOrder =
                match model.visibleOrder |> List.contains key with
                    | true -> model.visibleOrder |> List.filter (fun x -> key <> x) 
                    | _ -> model.visibleOrder
            { model with visibleOrder = newVisibleOrder }
        | CalculateScore -> 
            let weights = model.weights
            let rows = model.rows   
            let indices = Seq.zip rows [0..rows.Length-1]
            let newScores = 
                indices |> Seq.map (fun (row, index) -> 
                    let attributeList = row.values |> Map.toList
                    attributeList |> List.map (fun (key, value) -> 
                        match weights |> Map.tryFind key with
                            | Some (Some w) ->
                                let atrWeight = w
                                let atrValue = Value.toFloat value           
                                atrWeight * atrValue
                            | _ -> 0.0
                    ) |> List.toArray 
                      |> Array.sum
                )  |> Seq.toArray
            
            { model with scores = newScores}
        | Sort key ->
            let rows = model.rows
            let sortf (r : Row) : Value = 
                r.values |> Map.find key
            { model with rows = Array.sortBy sortf rows }
          
        | _ -> model

        
let dependencies = 
    [
        { name = "lineUp.css"; url = "lineUp.css"; kind = Stylesheet }

    ] @ Html.semui


let view (model : MTable) =
    printfn "View..."
    body [] [
        require dependencies (
            Incremental.table (AttributeMap.ofList ["border" => "0"; "cellspacing" => "0"; style "border-collapse:collapse;"]) <|
                alist {
                    let! headers = model.header
                    let! visibleOrder = model.visibleOrder
                    let! rows = model.rows
                    let! scores = model.scores
                    
                    yield thead [] [                      
                        tr[][
                            yield th[onClick (fun _ -> Sort "Score")][text "Score"]
                            for visibleName in visibleOrder do
                                let attribute = headers |> Map.find visibleName
                                yield th [onClick (fun _ -> Sort visibleName)] [text attribute.name]
                            ]
                        ]
                    yield tbody [] [
                        
                        for (row, index) in Seq.zip rows [0..rows.Length-1] do
                            let score = scores.[index]
                            let firstColu = td [] [text (sprintf "%.2f" score)]  
                            let columns = visibleOrder |> List.map (fun x -> 
                                let value = row.values |> Map.find x
                                let attribute = headers |> Map.find x 
                                match attribute.kind with
                                    | Kind.Plain -> match value with
                                                    | String s -> td [] [text s]
                                                    | Float f -> td [] [text (sprintf "%.2f" f)]
                                                    | Int i -> td []  [text (sprintf "%d" i)]
                                                    | Missing -> td [] [text ""]
                                    | Kind.Bar target -> 
                                        let stats = attribute.stats
                                        let v = value |>  Value.toFloat 


                                        let percentage = 
                                            match target with
                                                | Max -> ((v - stats.min) * 100.0 / (stats.max - stats.min))
                                                | Min -> 100.0 - ((v - stats.min) * 100.0 / (stats.max - stats.min))
                                        
                                        let stringWidth = sprintf "width: %.2f%%" percentage

                                        td [] [
                                            div [clazz "outer"] [                                                        
                                                div [clazz "inner"; clazz "maxRes"; style stringWidth] []
                                                p [] [
                                                    span [clazz "alignLeft"][text (sprintf "%.2f%%" percentage)]
                                                    span [clazz "alignRight"][text (sprintf "%.2f" v)]
                                                ]
                                            ]
                                        ]                                
                                    )
                            yield tr [] (firstColu :: columns)
                        ]
                }
        )
        br []
        Incremental.div (AttributeMap.ofList[]) <| (
            alist {
                let! visibleAttributes = model.visibleOrder
                for name in visibleAttributes do
                    yield button [clazz "ui red mini button"; onClick (fun _ -> RemoveAttribute name)][text name]
            }
        )
        br []
        Incremental.div (AttributeMap.ofList[]) <| (
            alist {
                let! header = model.header
                let! visible = model.visibleOrder
                let list = header |> Map.toList |> List.map (fun (k,v) -> v.name)
                for a in list do
                    let isVisible = visible |> List.contains a
                    match isVisible with
                        | false -> yield button [clazz "ui green mini button"; onClick (fun _ -> AddAttribute a)][text a]
                        | true -> ()
                }
        )
        br []

        button [onClick (fun _ -> CalculateScore)][text "score calc"]
    ]

let threads (model : Table) = 
    ThreadPool.empty

let app () =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
    {
        unpersist = Unpersist.instance     
        threads = threads 
        initial = 
            Log.startTimed "loading model"
            let data = NewLineUp.defaultModel()
            Log.stop()
            data
        update = update 
        view = view
    }