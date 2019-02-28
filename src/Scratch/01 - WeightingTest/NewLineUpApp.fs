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
open Aardvark.Base.Incremental


//let findOption (k : 'k)  (m : amap<'k,IMod<Option<'v>>>) : IMod<Option<'v>> =
//    adaptive {
//        let! v = AMap.tryFind k m
//        match v with
//            | Some v -> 
//                let! v = v
//                match v with
//                    | None -> return None
//                    | Some v -> return Some v
//            | None -> return None
//    }

let findOption (k : 'k) (m : amap<'k, IMod<'v>>) : IMod<'v> =
    adaptive {
        let! v = AMap.tryFind k m
        match v with
            | Some v -> 
                let! v = v
                return v
            | None -> return Unchecked.defaultof<'v>
    }


let CalculateScore model =
    let updatedRows = 
        model.rows |> Array.map (fun row -> 
            let score = 
                row.values
                    |> Map.toList
                    |> List.map (fun (k, v) ->
                        match model.visibleOrder |> List.contains k with
                            | true -> 
                                match model.weights |> HMap.tryFind k with
                                    | Some w -> // w * (Value.toFloat v)
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

let getColors (key: string) (colors: Map<string, C4b>)  =
    match colors |> Map.tryFind key with
                    | Some c -> 
                        let textCol = 
                            match c.A with
                                | 0uy -> C3b.Black
                                | _ -> C3b.White
                        (colorToHex (c.ToC3b()), colorToHex textCol)
                    | None -> (colorToHex C3b.Black, colorToHex C3b.White)

let getValuesByAttribute (key: string) (rows: Row array) =
    let value (row : Row) : float =
        let v = Map.find key row.values
        Value.toFloat v
    Array.map value rows
         
let createHisto (attribute: Attribute) (rows: Row array) (colors: Map<string, C4b>) (binSize: int) =
    let min = attribute.stats.min
    let max = attribute.stats.max
    let boundSize = (max - min) / float (binSize-1)
    let bounds =
        match boundSize = 0.0 with
        | true -> Array.create (binSize - 1) 0.0
        | false -> [|min .. boundSize .. max|]

    let values = getValuesByAttribute attribute.name rows

    let heights : int[] =     
        bounds |> Array.indexed |> Array.map (fun (i, b) ->
            match i with
            | 0 -> values |> Array.filter (fun x -> x <= bounds.[i]) |> Array.length
            | _ -> values |> Array.filter (fun x -> x > bounds.[i-1] && x <= bounds.[i]) |> Array.length                                          
        )                                                                                     
                                    
    let (background, fontColor) =
        getColors attribute.name colors
                
    th [] [
        div [clazz "histoContainer"][
            for h in heights do
                yield div [style (sprintf "width: %i%%; height: %i%%; background: %s" ((100/binSize)-1) ((100/Array.max heights*h)) (background))] []
            ]
    ]

let rec sumTill (weights : hmap<'a, float>) (xs : list<'a>) (c : 'a) = 
    match xs with
        | [] -> 0.0
        | x::xs ->  
            if x = c then 0.0
            else 
                match weights |> HMap.tryFind x with
                | Some a -> 
                    a + sumTill weights xs c
                | _ -> sumTill weights xs c

let getVisibleBarAttributes (header : Map<string,Attribute>) (visibleOrder : list<string>) =
    visibleOrder |> List.choose (fun nameOfAttribute -> 
        match Map.tryFind nameOfAttribute header with
            | Some a -> 
                match a.kind with
                | Bar _ -> Some a
                | _ -> None
            | _ -> None
    )         


let getScoreDiv (width: float) (color: string) =
    div [style (sprintf "width: %.2f%%; background : %s" width color)] []

let myOnChange (cb : bool -> 'msg) = 
        onEvent "onchange" ["event.target.checked"] (List.head >> Pickler.json.UnPickleOfString >> cb)

let onMouseMoveRel (cb : V2d -> 'msg) : Attribute<'msg> =
    onEvent "onmousemove" [" toFixedV2d(relativePerc(event,'container'))"] (List.head >> Pickler.json.UnPickleOfString >> cb)

let sw = System.Diagnostics.Stopwatch.StartNew()

let update (model : Table) (msg : Message) =
    match msg with
        | StartBench -> 
            let whatTo = "Effective pixels"
            let p = 
                proclist {
                    yield Drag whatTo
                    for i in 0 .. 10 do
                        let! _ = Proc.Sleep 100
                        yield MouseMove(V2d(float i / 10.0, 0.0))
                    yield StopDrag 
                }
            { model with threads = ThreadPool.start p model.threads }

        | SetTargetMode (name, targetMode) ->
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
            let newWeights = model.weights |> HMap.add name value
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
            let visibleBarAttributes = getVisibleBarAttributes model.header model.visibleOrder
            let weights = 
                visibleBarAttributes |> List.map (fun attribute  ->
                            (attribute.name, (1.0 / float (visibleBarAttributes).Length)))
                            |> Map.ofList
            CalculateScore { model with weights = HMap.ofMap weights}
            
        | ToggleOptions ->
            { model with showOptions = 
                            match model.showOptions with
                             | true -> false
                             | false -> true
            }
        
        | Drag key ->
            { model with dragedAttribute =  Some key}

        | StopDrag ->
            { model with dragedAttribute = None}
            
        | Done -> 
            printfn "took: %A" (sw.Elapsed.TotalSeconds - model.lastUpdateTime)
            model
        | MouseMove coord ->
            let header = model.header
            let weights = model.weights
            let attribute = model.dragedAttribute
            let visibleOrder = model.visibleOrder
            match attribute with
                | None -> model
                | Some a ->                   
                    match header |> Map.tryFind a with
                        | None -> model
                        | Some currentAttr ->   
                            let ordredAttribs = getVisibleBarAttributes header visibleOrder
                            let prevWeight = sumTill weights model.visibleOrder a
                            let thisWeight = 
                                    match weights |> HMap.tryFind a with
                                        | Some w -> w            
                                        | _ -> 0.0
                            let delta = (coord.X - (prevWeight + thisWeight))
                            let input1 = ordredAttribs |> List.map (fun a ->
                                        let weight = match weights |> HMap.tryFind a.name with
                                                     |Some w -> w
                                                     | _ -> 0.0
                                        a.name, weight)
                            let newWeights =
                                match attribute with
                                | Some a -> model.weightingFunction input1 a delta |> HMap.ofList
                                | None -> weights                             

                            { model with weights = newWeights; lastUpdateTime = sw.Elapsed.TotalSeconds }
                            //let visibleBarAttributes = getVisibleBarAttributes model.header model.visibleOrder
                                    
                            //let prevWeight = sumTill weights model.visibleOrder a
                            //let thisWeight = 
                            //    match weights |> HMap.tryFind a with
                            //    | Some (Some w) -> w            
                            //    | _ -> 0.0
                            //match (thisWeight <= 0.0 && ((coord.X - (prevWeight + thisWeight)) <= 0.0)) with
                            //    | true -> model
                            //    | false ->
                            //        //printfn "this weight now: %.10f" (thisWeight)
                            //        let newWeights = model.weights |> HMap.add a (Some ((coord.X - prevWeight) |> clamp 0.0  1.0))
                            //        //printfn "changed by %.10f, coordX = %.10f" (coord.X - (prevWeight + thisWeight)) coord.X
                            
                            //        let oldBudget = (1.0 - (prevWeight + thisWeight)) 
                            //        let newBudget = (1.0 - coord.X) |> clamp 0.0 1.0                    
                            //        let scale = (newBudget/oldBudget) |> max 0.0 
     
                                                               
                            //        let actualIndex = visibleBarAttributes |> List.findIndex (fun a -> a.name == currentAttr.name)

                            //        //printfn "old: %.10f new %.10f" oldBudget newBudget
                            //        let attributesToNormalize = snd (visibleBarAttributes |> List.splitAt (actualIndex+1)) |> List.map ( fun e -> e.name)                            
                            //        let normalizedNewWeights = 
                            //            newWeights |> HMap.map (fun k a -> 
                            //                match attributesToNormalize|> List.contains k with
                            //                | true -> match a with
                            //                            | Some a ->
                            //                                printfn "attr: %s a: %.10f a*scale %.10f" k a (a * scale |> clamp 0.0 1.0)
                            //                                Some (a * scale |> clamp 0.0 1.0) 
                            //                            | _ -> None
                            //                | false -> a
                            //            )                           
                            //        { model with weights = normalizedNewWeights }

                            
        | _ -> model

        
let dependencies = 
    [
        { name = "lineUp.css"; url = "lineUp.css"; kind = Stylesheet }
        { name = "lineUp.js"; url = "lineUp.js"; kind = Script }

    ] @ Html.semui


let view (model : MTable) =

    require dependencies (
        body [onMouseUp (fun _ _ -> StopDrag); onMouseMoveRel MouseMove] [     
         
            require dependencies (
                onBoot "init('__ID__')" (
                    Incremental.table (AttributeMap.ofList [clazz "mainTable"; "border" => "0"; "cellspacing" => "0"; style "border-collapse:collapse;"]) <|
                        alist {
                            let! headers = model.header
                            let! visibleOrder = model.visibleOrder
                            let! rows = model.rows
                            let! showOptions = model.showOptions
                            let! colors = model.colors

                            yield thead [] [
                                // histograms      
                                yield tr [] [
                                    let binSize = 10
                                    for visibleName in visibleOrder do
                                        let attribute = headers |> Map.find visibleName 
                                        match attribute.kind with
                                        | Bar _ -> yield createHisto attribute rows colors binSize                                           
                                        | _ -> yield th [] []
           
                                ]

                                // Histo per Attribute
                                yield tr [] [
                                    for visibleName in visibleOrder do
                                        let attr = headers |> Map.find visibleName

                                        let iconClass =
                                            match attr.sortState with
                                                | Asc -> "angle up icon"
                                                | Desc ->"angle down icon"
                                                | Inactive -> ""

                                        let (background, fontColor) =
                                            getColors visibleName colors

                                        let selected =
                                            match attr.selected with
                                            | true -> "selected"
                                            | _ -> "selectable"
                                                        
                                        yield th [clazz selected; onClick (fun _ -> Sort visibleName); style ("background: " + (background) + "; color: " + (fontColor)); onMouseMove (fun _ -> Highlight (visibleName, true)); onMouseLeave (fun _ -> Highlight (visibleName, false))] [
                                            text attr.name;
                                            div[clazz "sortingSymbol"] [
                                                i [clazz iconClass][]
                                                ]
                                        ]
                                    ]
                                
                                // Additional Options for prefered weighting target and manual weight setting
                                match showOptions with
                                    | false -> ()
                                    | true -> 
                                            yield tr [] [
                                                for visibleName in visibleOrder do
                                                    let attribute = headers |> Map.find visibleName  
                                
                                                    let newTarget =
                                                                    match attribute.kind with
                                                                    | Bar Max-> Bar Min
                                                                    | Bar Min -> Bar Max
                                                                    | _ -> attribute.kind
                                
                                                    let targetButtonClass =
                                                        match attribute.kind with
                                                            | Bar Max -> "ui olive basic mini button"
                                                            | Bar Min -> "ui orange basic mini button"
                                                            | _ -> ""
                                      
                                                    match attribute.kind with
                                                        | Plain | Score -> yield th [][text ""]
                                                        | _ ->
                                                            yield th [] [                                           
                                                                div[clazz "ui input"] [

                                                                    let onChangeWeight (s : string) = 
                                                                        let num = System.Double.TryParse(s, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture)
                                                                        let value = 
                                                                            match num with
                                                                                | (true, v) -> v
                                                                                | (false, _) -> 0.0
                                                                        SetWeight (visibleName, value)

                                                                    let attribs = 
                                                                        amap {
                                                                            yield clazz "weightInput"; 
                                                                            yield "type" => "number"; 
                                                                            yield onChange onChangeWeight;
                                                                            let! currentWeight = 
                                                                                adaptive {
                                                                                    let! v = AMap.tryFind visibleName model.weights
                                                                                    match v with
                                                                                        | Some v -> return sprintf "%.2f" v                                                                    
                                                                                        | None -> return "0.0"
                                                                                }
                                                                            yield "value" => currentWeight
                                                                        } |> AttributeMap.ofAMap
                                                                    yield Incremental.input attribs        
                                                                ]
                                                                                                      
                                                                button [clazz targetButtonClass; clazz "targetButtons"
                                                                        onClick( fun _ -> SetTargetMode (visibleName, newTarget))
                                                                        ] [text (match attribute.kind with
                                                                                | Bar Max -> "Max"
                                                                                | Bar Min -> "Min"
                                                                                | _ -> ""
                                                                            )
                                                                        ]
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
                                             
                                                let (background, fontColor) =
                                                    getColors attribute.name colors
                                            
                                                let selected =
                                                    match attribute.selected with
                                                    | true -> "selected"
                                                    | _ -> "selectable"

                                                td [] [
                                                    div [clazz selected; clazz "outer"] [                                                        
                                                        div [clazz "inner"; style (sprintf "width: %.2f%%; background: %s; color: %s" percentage (background) (fontColor))] []
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
                                                            adaptive {
                                                                let! v = AMap.tryFind key model.weights
                                                                match v with
                                                                    | Some w -> return w                                                                  
                                                                    | None -> return 0.0
                                                            }
                                                           
                                                        let width = 
                                                            adaptive {
                                                                let! w = w
                                                                match attribute.kind with
                                                                    |Kind.Bar target -> 
                                                                        let stats = attribute.stats
                                                                        let v = value |>  Value.toFloat 

                                                                        match target with
                                                                            | Max -> return w * ((v - stats.min) * 100.0 / (stats.max - stats.min))
                                                                            | Min ->  return w * (100.0 - ((v - stats.min) * 100.0 / (stats.max - stats.min)))
                                                                        | _ -> return 0.0
                                                            }
                                                        let (background, fontColor) =
                                                            getColors attribute.name colors
                                                    
                                                        let selected =
                                                            match attribute.selected with
                                                            | true -> "selected"
                                                            | _ -> "selectable"

                                                        let attributes =
                                                            amap {
                                                                yield clazz selected; yield onMouseMove (fun _ -> Highlight (key, true)); yield onMouseLeave (fun _ -> Highlight (key, false))
                                                                let! w = width
                                                                yield style (sprintf "height: 100%%; width: %.2f%%; background: %s; float: left" w (background));
                                                            } |> AttributeMap.ofAMap
                                            
                                                        Incremental.div attributes AList.empty
                                                    )
                                   
                                                let stackBar = div [clazz "scoreWrapper"; clazz "innerScore"] manyDiffs
                                                div [clazz "outerScore"][
                                                    stackBar;
                                                    p [][
                                                        span[clazz "alignRight"][text scoreSum]]]
                                            )
                                    yield tr [] (columns)
                                ]
                        }
                    )
            )


            Incremental.div (AttributeMap.ofList[clazz "container"]) <| (
                alist {
                    let! visibleOrder = model.visibleOrder
                    let! headers = model.header
                    let visibleBarAttributes = getVisibleBarAttributes headers visibleOrder

                    //let! weights = model.weights
                    let! colors = model.colors

                    let! dragedAttr = model.dragedAttribute

                    for attribute in visibleBarAttributes do
                                            
                        let! width =


                            adaptive {
                                let! w = AMap.tryFind attribute.name model.weights
                                match w with
                                    | None -> return 0.0
                                    | Some w -> return w * 99.9
                            }

                        let (background, fontColor) =
                            getColors attribute.name colors

                        match width with
                            | 0.0 -> ()
                            | _ ->
                                let styleDragableScore =
                                    amap {                                   
                                        let s = sprintf " width: %.2f%%; background: %s; color: %s" (width) (background) (fontColor)
                                        yield style s
                                        yield clazz "draggableWeights"
                                    } |> AttributeMap.ofAMap

                                let styleCursor =
                                    //check if last element is dragged
                                    amap {
                                        match attribute.name == (visibleBarAttributes |> List.toSeq |> Seq.last).name with
                                        | true -> ()                                     
                                        | false ->                                                                                                                                                        
                                                yield onMouseDown (fun _ _ -> Drag (attribute.name))
                                                yield onMouseMoveRel (fun c -> MouseMove c)
                                                yield clazz "weightsDragCursor"
                                    } |> AttributeMap.ofAMap                                  
                           
                                yield div [] [
                                    Incremental.div styleDragableScore <| (
                                        alist {
                                            yield text attribute.name
                                            yield Incremental.div styleCursor AList.empty
                                        }
                                    )                                   
                                ]         
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
                button [onClick (fun _ -> StartBench)] [text "start benchmark"]
            ]

            div [] [
                Incremental.div AttributeMap.empty <|
                    alist {
                        let! w = model.weights |> AMap.toMod
                        yield onBoot "aardvark.processEvent('__ID__', 'finished');" (
                            div [onEvent "finished" [] (fun _ -> Done)] []
                        )
                    }
            ]
        //    alist {
        //        let! a = model.value
        //        for i in 0 .. 10 do
        //            yield button [] [text "ADF"]//text (sprintf "%d" a)]
        //        yield onBoot "aardvark.processEvent('__ID__', 'finished');" (
        //            button [onEvent "finished" [] (fun _ -> Done)] [text "urdar"]
        //        )
        //    }


        ]
    )

let threads (model : Table) = 
    model.threads

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