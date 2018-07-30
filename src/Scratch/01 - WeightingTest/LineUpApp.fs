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

type Message = Nop

let update (model : LineUp) (msg : Message) =
    model
        
let dependencies = 
    [
        { name = "lineUp.css"; url = "lineUp.css"; kind = Stylesheet }
    ]

// variant with html5 grid layouting (currently not working in our cef)
let view (model : MLineUp) =
   
    body [] [
        require dependencies (
            //	<table border="0" style="border-collapse:collapse;" cellspacing="0">
            Incremental.table (AttributeMap.ofList ["border" => "0"; "cellspacing" => "0"; style "border-collapse:collapse;"]) <|
                alist {
                    let! config = model.config 
                    yield tr [] (
                        config.attributes |> List.mapi (fun i r -> 
                            let clas =
                                if i = 0 then "leftHeader"
                                elif i = List.length config.attributes - 1 then "rightHeader"
                                else ""
                            th [clazz clas] [text r.name]                           
                        )
                    )
                
                    let! table = model.input

                    // preprocessing
                    let rows  = table.rows |> Array.sortBy (fun r -> match Map.find "Release date" r with | Int s -> s |  _ -> 0)

                    //for each column which is of kind percantage, compute min, max
                    //Max Resolution
                    
                        
                    let maxMaxRes : float = table.rows |> Array.maxBy (Map.find "Max resolution") |> (fun r -> match Map.find "Max resolution" r with | Float s -> s |  _ -> 0.0)
                    let minMaxRes : float = table.rows |> Array.minBy (fun r -> match Map.find "Max resolution" r with | Float s -> s |  _ -> 0.0) |> (fun r -> match Map.find "Max resolution" r with | Float s -> s |  _ -> 0.0)
                    //Low Resolution
                    let maxLowRes : float = table.rows |> Array.maxBy (fun r -> match Map.find "Low resolution" r with | Float s -> s |  _ -> 0.0) |> (fun r -> match Map.find "Low resolution" r with | Float s -> s |  _ -> 0.0)
                    let minLowRes : float = table.rows |> Array.minBy (fun r -> match Map.find "Low resolution" r with | Float s -> s |  _ -> 0.0) |> (fun r -> match Map.find "Low resolution" r with | Float s -> s |  _ -> 0.0)

                    //todo: Nullstelle zu viel!                  
                
                    // map colname and record of min, max
                    for r in  rows  do
                        yield tr [] [
                            for c in config.attributes do
                                yield td [] [
                                    // dispatch on kind....
                                    match Map.tryFind c.name r with
                                        | None -> ()
                                        | Some v -> 
                                            match v with
                                                | Int i ->
                                                    yield text (sprintf "%i" i)
                                                | Float v -> 
                                                    //todo: min,max welcher variable sollen verwendet werden?
                                                    let percentage = (v - minMaxRes) * 100.0 / (maxMaxRes - minMaxRes)
                                                    let stringWidth = sprintf "width: %.2f%%" percentage
                                                    yield div [clazz "outer"] [                                                        
                                                        div [clazz "inner"; clazz "maxRes"; style stringWidth] []
                                                        p [] [
                                                            span [clazz "alignLeft"][text (sprintf "%.2f%%" percentage)]
                                                            span [clazz "alignRight"][text (sprintf "%.2f" v)]
                                                        ]
                                                    ]
                                                | Name s -> 
                                                    yield text s
                                                | _ -> ()
                                ]
                        ]
                }
        )
    ]


let threads (model : LineUp) = 
    ThreadPool.empty

let app =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
    {
        unpersist = Unpersist.instance     
        threads = threads 
        initial = LineUp.defaultModel
        update = update 
        view = view
    }
