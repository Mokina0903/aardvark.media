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
    | SetTargetMode of (string * Kind)
    | SetWeight of (string * float)
    | AddAttributeAt of (string * int) // ASK HARRI
    | Highlight of (string * bool)
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

let getScoreDiv (width: float) (color: string) =
    div [style (sprintf "width: %.2f%%; background : %s" width color)] []

//let CalculateScore model =
//    let updatedRows = 
//        model.rows |> Array.map (fun row -> 
//            let score = 
//                row.values
//                    |> Map.toList
//                    |> List.map (fun (k, v) ->
//                        match model.visibleOrder |> List.contains k with
//                            | true -> 
//                                let color = model.colors |> Map.find k
//                                match model.weights |> Map.tryFind k with
//                                    | Some (Some w) -> // w * (Value.toFloat v)
//                                        let attr = model.header |> Map.find k //todo tryFind
//                                        let stats = attr.stats
//                                        let v = v |>  Value.toFloat
//                                        let width = match attr.kind with
//                                            | Bar Max -> w * ((v - stats.min) * 100.0 / (stats.max - stats.min))
//                                            | Bar Min -> w * (100.0 - ((v - stats.min) * 100.0 / (stats.max - stats.min)))
//                                            | _ -> 0.0
//                                        drawScoreDiv width (colorToHex color)
//                                    | _ -> 0.0
//                            | false -> 0.0
//                    ) 
//            let updatedAttrList = row.values |> Map.map ( fun k v ->
//                match k with
//                    | "Score" -> Value.Float score
//                    | _ -> v)
//            { row with values = updatedAttrList})
//    { model with rows = updatedRows}


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

        | Highlight (key, isSelected) ->
            let header = model.header
            match header |> Map.tryFind key with
                | None -> model
                | Some currentAttr ->    
                    let newHeader = header |> Map.map (fun k a -> 
                        match k = key with
                            | true -> {currentAttr with selected = isSelected}
                            | false -> {a with selected = false})
                    { model with header = newHeader }

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
                                    let (background, fontColor) =
                                        match colors |> Map.tryFind visibleName with
                                            | Some c -> 
                                                let textCol = 
                                                    match c.A with
                                                        | 0uy -> C3b.Black
                                                        | _ -> C3b.White
                                                (c.ToC3b(), textCol)
                                            | None -> (C3b.Black, C3b.White)
                                    let selected =
                                        match attr.selected with
                                        | true -> "selected"
                                        | _ -> ""
                                                        
                                    yield th [clazz selected; onClick (fun _ -> Sort visibleName); style ("background: " + (colorToHex background) + "; color: " + (colorToHex fontColor)); onMouseMove (fun _ -> Highlight (visibleName, true)); onMouseLeave (fun _ -> Highlight (visibleName, false))] [
                                        text attr.name;
                                        div[clazz "sortingSymbol"] [
                                            i [clazz iconClass][]
                                            ]
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
                                                    | Plain | Score -> yield th [][text ""]
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
                                                                                                      
                                                            button [clazz targetButtonClass; clazz "targetButtons"
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
                                             
                                            let color = 
                                                match colors |> Map.tryFind x with
                                                    | Some c -> colorToHex (c.ToC3b())
                                                    | _ -> colorToHex C3b.Black
                                            
                                            let selected =
                                                match attribute.selected with
                                                | true -> "selected"
                                                | _ -> ""

                                            td [] [
                                                div [clazz selected; clazz "outer"] [                                                        
                                                    div [clazz "inner"; style (sprintf "width: %.2f%%; background: %s" percentage color)] []
                                                    p [] [
                                                        span [clazz "alignLeft"][text (sprintf "%.2f%%" percentage)]
                                                        span [clazz "alignRight"][text (sprintf "%.2f" v)]
                                                    ]
                                                ]
                                            ]

                                         | Kind.Score -> 
                                            let scoreSum =
                                                match value with                                                       
                                                            | Float f -> (sprintf "%.2f%%" f)
                                                            | _ -> ""
                                            
                                            let manyDiffs : list<DomNode<_>> = 
                                                visibleOrder |> List.map (fun key -> 
                                                    let value = row.values |> Map.find key
                                                    let attribute = headers |> Map.find key
                                                    let w = 
                                                        match weights |> Map.tryFind key with
                                                            | Some (Some w) -> w
                                                            | _ -> 0.0
                                                    let width = 
                                                        match attribute.kind with
                                                            |Kind.Bar target -> 
                                                                let stats = attribute.stats
                                                                let v = value |>  Value.toFloat 

                                                                match target with
                                                                    | Max -> w * ((v - stats.min) * 100.0 / (stats.max - stats.min))
                                                                    | Min ->  w * (100.0 - ((v - stats.min) * 100.0 / (stats.max - stats.min)))
                                                                | _ -> 0.0
                                                    let color = 
                                                        match colors |> Map.tryFind key with
                                                            | Some c -> colorToHex (c.ToC3b())
                                                            | _ -> colorToHex C3b.Black
                                                    
                                                    let selected =
                                                        match attribute.selected with
                                                        | true -> "selected"
                                                        | _ -> ""
                                            
                                                    div [clazz selected; clazz "attributeScore"; style (sprintf "height: 100%%; width: %.2f%%; background: %s; float: left" width color); onMouseMove (fun _ -> Highlight (key, true)); onMouseLeave (fun _ -> Highlight (key, false))] []
                                                )
                                   
                                            let stackBar = div [clazz "scoreWrapper"; clazz "innerScore"] manyDiffs
                                            div [clazz "outerScore"][
                                                stackBar;
                                                p [][
                                                    span[clazz "alignRight"][text scoreSum]]] // stackedbar...
                                        )
                                yield tr [] (columns)
                            ]
                    }
                )
        )

        Incremental.div (AttributeMap.ofList[]) <| (
            alist {
                let! visibleOrder = model.visibleOrder
                let! headers = model.header
                let! weights = model.weights
                let! colors = model.colors

                for name in visibleOrder do
                    let attribute = headers |> Map.find name
                    let width = 
                        match attribute.kind with
                            |Kind.Bar _ -> match weights |> Map.tryFind name with
                                            | Some (Some w) -> w * 100.0
                                            | _ -> 0.0
                            | _ -> 0.0
                    let (background, fontColor) =
                        match colors |> Map.tryFind name with
                            | Some c -> 
                                let textCol = 
                                    match c.A with
                                        | 0uy -> C3b.Black
                                        | _ -> C3b.White
                                (c.ToC3b(), textCol)
                            | None -> (C3b.Black, C3b.White)
                    match width with
                        | 0.0 -> ()
                        | _ ->
                        //todo make incremental
                            yield div [style (sprintf "height: 30px; width: %.2f%%; background: %s; color: %s; float: left" width (colorToHex background) (colorToHex fontColor))] [text name]
                            yield div [clazz "dragableWeights"; style "height: 30px; width: 2px; float: left; color: red"; onMouseMove (fun _ -> Highlight (name, true)); onMouseLeave (fun _ -> Highlight (name, false))][] //todo how to get hovered element?
            }
        )

        Incremental.div (AttributeMap.ofList[clazz "attributesDiv"]) <| (
            alist {
                let! visibleAttributes = model.visibleOrder
                for name in visibleAttributes do
                    yield button [clazz "ui red mini button"; onClick (fun _ -> RemoveAttribute name)][text name]
            }
        )

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
        
        div [clazz "attributesDiv"] [
            button [onClick (fun _ -> NormalizeWeight)] [text "normalize Weights"]
            button [onClick (fun _ -> ToggleOptions)] [text "additional Options"]
        ]

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