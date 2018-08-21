﻿module NewLineUp

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
    | SetTargetMode of (string * Kind)
    | SetWeight of (string * float)
    | AddAttributeAt of (string * int) // ASK HARRI
    | AddAttribute of string
    | RemoveAttribute of string
    | CalculateScore
    | NormalizeWeight
    | ToggleOptions
    | Sort of string

let CalculateScore model =

    let updatedRows = 
        model.rows |> Array.map (fun row -> 
            let score = 
                row.values
                    |> Map.toList
                    |> List.map (fun (k, v) ->
                        match model.visibleOrder |> List.contains k with
                            | true -> 
                                match model.weights |> Map.tryFind k with
                                    | Some (Some w) -> // w * (Value.toFloat v)
                                        let attr = model.header |> Map.find k //todo tryFind
                                        let stats = attr.stats
                                        let v = v |>  Value.toFloat
                                        match attr.kind with
                                            | Bar Max -> w * ((v - stats.min) * 100.0 / (stats.max - stats.min))
                                            | Bar Min -> w * (100.0 - ((v - stats.min) * 100.0 / (stats.max - stats.min)))
                                            | _ -> 0.0
                                    | _ -> 0.0
                            | false -> 0.0
                    ) |> List.sum
            let updatedAttrList = row.values |> Map.map ( fun k v ->
                match k with
                    | "Score" -> Value.Float score
                    | _ -> v)
            { row with values = updatedAttrList})
    { model with rows = updatedRows}

let colorToHex (color : C3b) = 
        let bytes = [| color.R; color.G; color.B |]
        let colorString = 
            bytes 
                |> Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x))
                |> String.concat System.String.Empty
        "#"+colorString

let myOnChange (cb : bool -> 'msg) = 
        onEvent "onchange" ["event.target.checked"] (List.head >> Pickler.json.UnPickleOfString >> cb)

let update (model : Table) (msg : Message) =
    match msg with
        | SetTargetMode (name, targetMode) ->
            //printf ("%s" targetMode.ToString)
            let attr = 
                match model.header |> Map.tryFind name with
                        | Some a ->
                            Some {
                                a with
                                    kind = match a.kind with   
                                            | Bar _ -> targetMode
                                            | _ -> a.kind
                                }
                        | None -> None

            match attr with 
                | Some updateAttribe -> CalculateScore { model with header = model.header |> Map.add name updateAttribe } 
                | None -> model

        | SetWeight (name, value) ->

            let newWeights = model.weights |> Map.add name (Some value) 
            CalculateScore {model with weights = newWeights}
                 
        | AddAttribute key -> 
            let newVisibleOrder =
                match model.visibleOrder |> List.contains key with
                    | false -> List.append model.visibleOrder [key]
                    | _ -> model.visibleOrder
            CalculateScore { model with visibleOrder = newVisibleOrder }

        | AddAttributeAt (key, index) -> failwith ""

        | RemoveAttribute key -> 
            let newVisibleOrder =
                match model.visibleOrder |> List.contains key with
                    | true -> model.visibleOrder |> List.filter (fun x -> key <> x) 
                    | _ -> model.visibleOrder
            CalculateScore { model with visibleOrder = newVisibleOrder }

        | CalculateScore -> CalculateScore model

        | Sort key ->
            let header = model.header
            match header |> Map.tryFind key with
                | None -> model
                | Some currentAttr ->
                    let newState =
                        match currentAttr.sortState with
                            | Inactive -> Asc
                            | Asc -> Desc
                            | Desc -> Asc

                    let sortf (r:Row)  = 
                        match r.values |> Map.tryFind key with
                            | Some v -> v
                            | None -> Missing
                    
                    let sortedRows =
                        match newState with
                            | Asc -> Array.sortBy sortf model.rows
                            | _ -> Array.sortByDescending sortf model.rows       

                    let newHeader = header |> Map.map (fun k a -> 
                        match k = key with
                            | true -> {currentAttr with sortState = newState}
                            | false -> {a with sortState = Inactive})
                    { model with rows = sortedRows; header = newHeader }

        | NormalizeWeight -> 
             let weights = 
                    model.header |> Map.map (fun key _ -> 
                            match key with
                            | "Score" -> None
                            | _ -> Some (1.0 / float (model.visibleOrder.Length-1)) ) //-1 weil Score in visibleOrder immer includiert
             CalculateScore { model with weights = weights}
            
        | ToggleOptions ->
            { model with showOptions = 
                            match model.showOptions with
                             | true -> false
                             | false -> true
            }
          
        | _ -> model

        
let dependencies = 
    [
        { name = "lineUp.css"; url = "lineUp.css"; kind = Stylesheet }
        { name = "lineUp.js"; url = "lineUp.js"; kind = Script }

    ] @ Html.semui


let view (model : MTable) =
    body [] [
        require dependencies (
            onBoot "init('__ID__')" (
                Incremental.table (AttributeMap.ofList [clazz "mainTable"; "border" => "0"; "cellspacing" => "0"; style "border-collapse:collapse;"]) <|
                    alist {
                        let! headers = model.header
                        let! visibleOrder = model.visibleOrder
                        let! rows = model.rows
                        let! weights = model.weights
                        let! showOptions = model.showOptions
                        let! colors = model.colors

                        yield thead [] [                      
                            yield tr [] [
                                for visibleName in visibleOrder do
                                    let attr = headers |> Map.find visibleName
                                    let iconClass =
                                        match attr.sortState with
                                            | Asc -> "angle up icon"
                                            | Desc ->"angle down icon"
                                            | Inactive -> ""
                                    let col =
                                        match colors |> Map.tryFind visibleName with
                                            | Some c -> c
                                            | None -> C3b.Black
                                    
                                                        
                                    yield th [onClick (fun _ -> Sort visibleName); style ("background: " + (colorToHex col))] [
                                        text attr.name;
                                        div[clazz "sortingSymbol"] [
                                            i [clazz iconClass][]]
                                    ]
                                ]

                            match showOptions with
                                | false -> ()
                                | true -> 
                                        yield tr [] [
                                            for visibleName in visibleOrder do
                                                let attr = headers |> Map.find visibleName  
                                
                                                let newTarget =
                                                                match attr.kind with
                                                                | Bar Max-> Bar Min
                                                                | Bar Min -> Bar Max
                                                                | _ -> attr.kind
                                
                                                let targetButtonClass =
                                                    match attr.kind with
                                                        | Bar Max -> "ui olive basic mini button"
                                                        | Bar Min -> "ui orange basic mini button"
                                                        | _ -> ""
                                      

                                                match attr.kind with
                                                    | Plain -> yield th [][text ""]
                                                    | _ ->
                                                        yield th [] [                                           
                                                            div[clazz "ui input"] [
                                                                input [clazz "weightInput"; "type" => "number"; onChange (fun s ->                                       
                                                                    let num = System.Double.TryParse(s, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture)
                                                                    let value = 
                                                                        match num with
                                                                            | (true, v) -> v
                                                                            | (false, _) -> 0.0
                                                                    SetWeight (visibleName, value)
                                                                    );
                                                                    "value" =>
                                                                        match weights |> Map.tryFind visibleName with
                                                                            | Some (Some v) -> sprintf "%.2f" v                                                                       
                                                                            |  _ -> "0.0"                                 
                                                                    ]         
                                                            ]
                                                            br []                                           
                                                            button [clazz targetButtonClass;
                                                                    onClick( fun _ -> SetTargetMode (visibleName, newTarget))
                                                                    ] [text (match attr.kind with
                                                                            | Bar Max -> "Max"
                                                                            | Bar Min -> "Min"
                                                                            | _ -> ""
                                                                            )]
                                                        ]
                                            ]                                                                            
                            ]

                        yield tbody [clazz "mainTBody"] [
                            for (row, index) in Seq.zip rows [0..rows.Length-1] do                   
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

                                            td [] [
                                                div [clazz "outer"] [                                                        
                                                    div [clazz "inner"; style (sprintf "width: %.2f%%" percentage)] []
                                                    p [] [
                                                        span [clazz "alignLeft"][text (sprintf "%.2f%%" percentage)]
                                                        span [clazz "alignRight"][text (sprintf "%.2f" v)]
                                                    ]
                                                ]
                                            ]                                
                                        )
                                yield tr [] (columns)
                            ]
                    }
                )
        )

        br []
        Incremental.div (AttributeMap.ofList[clazz "attributesDiv"]) <| (
            alist {
                let! visibleAttributes = model.visibleOrder
                for name in visibleAttributes do
                    yield button [clazz "ui red mini button"; onClick (fun _ -> RemoveAttribute name)][text name]
            }
        )

        br []
        Incremental.div (AttributeMap.ofList[clazz "attributesDiv"]) <| (
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
        button [onClick (fun _ -> NormalizeWeight)] [text "normalize Weights"]

        br[]
        button [onClick (fun _ -> ToggleOptions)] [text "additional Options"]
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
            let dataWithScore = CalculateScore data
            Log.stop()
            dataWithScore
        update = update 
        view = view
    }