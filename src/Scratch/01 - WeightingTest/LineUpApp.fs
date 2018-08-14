module LineUp

open Aardvark.UI
open Aardvark.UI.Primitives

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.UI
open Aardvark.UI.Operators

open LineUpModel
open LineUpModel

open Aardvark.Base.LensOperators

type Message =
    | Nop
    | AddAttribute of string
    | RemoveAttribute of string

let update (model : LineUp) (msg : Message) =
    //model
    match msg with
        | AddAttribute s -> 
            let sk = model.visibleAttributes |> HMap.count
            let v = model.visibleAttributes |> HMap.add s { name = s; sortKey = sk; weight = None}
            { model with visibleAttributes = v }
        | RemoveAttribute s -> 
            { model with visibleAttributes = HMap.remove s model.visibleAttributes }
            //LineUp.Lens.config |. Config.attributes <== newAttributes
        | _ -> model

        
let dependencies = 
    [
        { name = "lineUp.css"; url = "lineUp.css"; kind = Stylesheet }

    ] @ Html.semui

// variant with html5 grid layouting (currently not working in our cef)
let view (model : MLineUp) =
   
    let sortedAttribs = 
        model.visibleAttributes 
        |> AMap.toASet 
        |> ASet.sortBy (fun (name,visibleAttribute) -> visibleAttribute.sortKey)

    body [] [
        require dependencies (
            //	<table border="0" style="border-collapse:collapse;" cellspacing="0">
            //onBoot "$('table').tablesort()" (
            Incremental.table (AttributeMap.ofList ["border" => "0"; "cellspacing" => "0"; style "border-collapse:collapse;"]) <|
                alist {
                    let! config = model.config
                    yield thead [] [

                        yield Incremental.tr (AttributeMap.ofList[]) <| (
                            alist{
                                let attribs = model.visibleAttributes
                                for (name,visibleAttribute) in sortedAttribs do
                                    yield th [][text name]                     
                                }
                    
                        )

                 //       yield tr [] [                       
                 //           yield! config.attributes |> List.mapi (fun i r -> 
                 //               let clas =
                 //                   if i = 0 then "leftHeader"
                 //                   elif i = List.length config.attributes - 1 then "rightHeader"
                 //                  else ""                               
                 //               th [clazz clas] [text r.name]                           
                 //           )
                 //       ]
                    ]
                
                    let! table = model.input
                    let! weights = model.weights
                    //let abc = model.visibleAttributes |> HMap.add s { name = s; sortKey = sk; weight = None}                  

                    // preprocessing
                    let rows  = table.rows

                    //for each column which is of kind percantage, compute min, max
                    
                    let stats =
                        [ for a in config.attributes do
                            match a.kind with
                                | Kind.Bar _ -> 
                                    let columnName = a.name
                                    let rowsWithColumn = 
                                        Array.map (fun r -> 
                                            match Map.find columnName r with
                                                | Value.Float v -> v
                                                | Value.Int v -> float v
                                                | Value.Missing -> 0.0
                                                | Value.Name s -> failwithf "wanted bar for string: %s" s
                                        ) rows
                                    // Todo: how to handle empty rows???
                                    let min = Array.min rowsWithColumn
                                    let max = Array.max rowsWithColumn
                                    let avg = Array.average rowsWithColumn
                    
                                    yield a.name, {
                                        min = min
                                        max = max
                                        avg = 0.0
                                    }
                                | _ -> 
                                    yield a.name, Statistics.empty
                      
                        ] |> Map.ofList                 
                
                    //let scores =
                    //    [|
                    //        for r in rows do
                    //            let weights = [
                    //                for w in weights do 
                    //                    let c = List.find (fun (a: Attribute) -> a.name = w.name) config.attributes
                    //                    match c.kind, Map.tryFind c.name r with
                    //                        | Kind.Bar order, Some o -> 
                    //                            let v =
                    //                                match o with 
                    //                                    | Int i -> float i
                    //                                    | Float f -> float f
                    //                                    | Missing -> 0.0
                    //                                    | Value.Name s -> failwithf "wanted bar for string: %s" s
                    //                            let stat = Map.find c.name stats
                    //                            let stats = Map.find c.name stats                                               
                    //                            let percentage = 
                    //                                if order = Target.Max then
                    //                                    (v - stats.min) * 100.0 / (stats.max - stats.min)
                    //                                else
                    //                                    100.0 - ((v - stats.min) * 100.0 / (stats.max - stats.min))
                    //                            let stringWidth = sprintf "width: %.2f%%" percentage
                    //                            yield (w.weight * percentage)                                            
                    //                        | _ -> yield 0.0
                    //            ]
                    //            let overallWeight = weights |> List.sum
                    //            yield overallWeight           
                    //    |]

                    //let scores =
                    //    [|
                    //        for r in rows do
                    //            //let weights = [                                   
                    //            //    let c = model.visibleAttributes
                                    
                    //            //    c |> AMap.
                                    
                    //            //    match c with myStuf
                    //            //        | Kind.Bar order, Some o -> 
                    //            //            let v =
                    //            //                match o with 
                    //            //                    | Int i -> float i
                    //            //                    | Float f -> float f
                    //            //                    | Missing -> 0.0
                    //            //                    | Value.Name s -> failwithf "wanted bar for string: %s" s
                    //            //            let stat = Map.find c.name stats
                    //            //            let stats = Map.find c.name stats                                               
                    //            //            let percentage = 
                    //            //                if order = Target.Max then
                    //            //                    (v - stats.min) * 100.0 / (stats.max - stats.min)
                    //            //                else
                    //            //                    100.0 - ((v - stats.min) * 100.0 / (stats.max - stats.min))
                    //            //            let stringWidth = sprintf "width: %.2f%%" percentage
                    //            //            yield (w.weight * percentage)                                            
                    //            //        | _ -> yield 0.0
                    //            //]
                    //            //let overallWeight = weights |> List.sum
                    //            //yield overallWeight           
                    //    |]

                    let scores =
                        aset {
                            for (name,visibleAttribute) in model.visibleAttributes |> AMap.toASet do
                                yield failwith ""
                        }

                    //let scores = 
                    //    model.visibleAttributes |> AMap.mapM (fun k v -> 
                    //        let attribSum =
                    //            rows |> Array.averageBy (fun r ->
                    //                let value = r |> Map.find k
                    //                let trueValue = 
                    //                    match value with 
                    //                        | Int i -> float i
                    //                        | Float f -> float f
                    //                        | Missing -> 0.0
                    //                        | Value.Name s -> failwithf "wanted bar for string: %s" s
                                
                    //                let otherAttribute = config.attributes |> List.find (fun key -> k = key.name)
                    //                let kind = otherAttribute.kind
                    //                let myStats = stats |> Map.find k

                    //                match otherAttribute.kind with 
                    //                    | Kind.Bar Target.Max -> (trueValue - myStats.min) / (myStats.max - myStats.min)
                    //                    | Kind.Bar Target.Min -> 1.0 - ((trueValue - myStats.min) / (myStats.max - myStats.min))
                    //                    | _ -> 0.0
                    //            )
                    //        v.weight |> Mod.map (fun w -> 
                    //            match w with
                    //                | None -> 0.0
                    //                | Some w -> attribSum * w
                    //        )
                            
                    //    )


                    let scores : array<IMod<float>> =
                        [|
                            for r in rows do
                                failwith ""
                                //yield adaptive {
                                //    let! attributes = model.visibleAttributes |> AMap.toMod
                                //    let sum = attributes |> HMap.toList |> List.sumBy (fun (name,visibleAttribute) -> 
                                //    return failwith ""
                                //}
                        |]
                         

                    //let jsdf = [1;2;3] |> List.map (fun v -> sprintf "%A" v)

                    //let resultArray = ...
                    //for(int i i= ......)
                    //{
                    //    resultarry[i] = sprintf "Jsdfj" inputArray[i];
                    //}

                    let scores = failwith ""

                    let entries =  Array.zip rows scores
                    let sortedRows = Array.sortBy snd entries

                    // map colname and record of min, max
                    yield tbody [] [
                        for (r,score) in sortedRows  do
                            yield Incremental.tr AttributeMap.empty <|
                                alist {
                                    for (name,visibleAttribute) in sortedAttribs do
                                        let c = config.attributes |> List.tryFind  (fun s -> s.name = name)
                                        match c with
                                            | None -> 
                                                printfn "could not find %s, attribs= %A" name config.attributes
                                                yield text "crzay shit"
                                            | Some c -> 
                                                yield td [] [
                                                    // dispatch on kind....
                                                    match c.kind, Map.tryFind c.name r with
                                                        | Kind.Plain, Some (Value.Name s) -> yield text s
                                                        | Kind.Plain, Some (Value.Float f) -> yield text (sprintf "%.2f" f)
                                                        | Kind.Plain, Some (Value.Int i) -> yield text (sprintf "%d" i)
                                                        | Kind.Bar Target.Max, Some o -> 
                                                            let v =
                                                                match o with 
                                                                    | Int i -> float i
                                                                    | Float f -> float f
                                                                    | Missing -> 0.0
                                                                    | Value.Name s -> failwithf "wanted bar for string: %s" s
                                                            let stat = Map.find c.name stats
                                                            let stats = Map.find c.name stats
                                                            let percentage = (v - stats.min) * 100.0 / (stats.max - stats.min)
                                                            let stringWidth = sprintf "width: %.2f%%" percentage
                                                            yield div [clazz "outer"] [                                                        
                                                                div [clazz "inner"; clazz "maxRes"; style stringWidth] []
                                                                p [] [
                                                                    span [clazz "alignLeft"][text (sprintf "%.2f%%" percentage)]
                                                                    span [clazz "alignRight"][text (sprintf "%.2f" v)]
                                                                ]
                                                            ]
                                                        | Kind.Bar Target.Min, Some o -> 
                                                            let v =
                                                                match o with 
                                                                    | Int i -> float i
                                                                    | Float f -> float f
                                                                    | Missing -> 0.0
                                                                    | Value.Name s -> failwithf "wanted bar for string: %s" s
                                                            let stat = Map.find c.name stats
                                                            let stats = Map.find c.name stats
                                                            let percentage = 100.0 - ((v - stats.min) * 100.0 / (stats.max - stats.min))
                                                            let stringWidth = sprintf "width: %.2f%%" percentage
                                                            yield div [clazz "outer"] [                                                        
                                                                div [clazz "inner"; clazz "maxRes"; style stringWidth] []
                                                                p [] [
                                                                    span [clazz "alignLeft"][text (sprintf "%.2f%%" percentage)]
                                                                    span [clazz "alignRight"][text (sprintf "%.2f" v)]
                                                                ]
                                                            ]
                                                        | Kind.Score, _ ->
                                                            yield text (sprintf "%.2f%%" score)
                                                
                                                        | _, _ -> yield text (sprintf "missing value for: %s" c.name)
                                                ]
                            }
                        ]
                      
                }
        )
        //)

        br []
        Incremental.div (AttributeMap.ofList[]) <| (
            alist{
                let! config = model.config
                for a in config.attributes do
                    let! stillHere = AMap.tryFind a.name model.visibleAttributes
                    match stillHere with
                        | None -> yield button [clazz "ui red mini button"; onClick (fun _ -> AddAttribute a.name)][text a.name]          
                        | Some _ -> ()
                }
            )
        br []
        Incremental.div (AttributeMap.ofList[]) <| (
            alist{
                let attribs = model.visibleAttributes
                for (name,visibleAttribute) in sortedAttribs do
                    yield button [clazz "ui green mini button"; onClick (fun _ -> RemoveAttribute name)][text name]
                      
                }
            )
   
        //onBoot "$('.ui.dropdown').dropdown();" (
        //           Incremental.select (AttributeMap.ofList[clazz "ui fluid dropdown"; "multiple" => ""; onChange (fun s -> printf "%s" s; Nop)]) <|                                     
          //              alist{
            //                let! config = model.config
              //              for a in config.attributes do
                //                match a.kind with
                  //                  | Kind.Bar _ ->
                    //                    yield option ["value" => a.name][text a.name]
                      //              | _ -> ()                            
                        //    }
          //  )
    ]

let threads (model : LineUp) = 
    ThreadPool.empty

let app () =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
    {
        unpersist = Unpersist.instance     
        threads = threads 
        initial = 
            Log.startTimed "loading model"
            let data = LineUp.defaultModel()
            Log.stop()
            data
        update = update 
        view = view
    }
