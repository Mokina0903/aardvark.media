module LineUp

open Aardvark.UI
open Aardvark.UI.Primitives

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.UI
open Aardvark.UI.Operators

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
                
                    //tr []
                    //    th [clazz  "leftHeader"] [text "Model"]
                    //    th [] [text "Release date"]
                    //    th [] [text "Max resolution"]
                    //    th [clazz  "rightHeader"] [text "Low resolution"]
                    //]
                    let! table = model.input

                    // preprocessing
                    let rows  = table.rows |> Array.sortBy (fun r -> match Map.find "Release date" r with | Float s -> s |  _ -> 0.0)

                    // for each column which is of kind percantage, compute min,max........
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
                                                | Float v -> 
                                                    yield text (sprintf "%.2f" v)
                                                | Name s -> 
                                                    yield text s
                                                | _ -> ()
                                ]
                        ]
                    
                    //tr [] [
                    //    td [][text "Canon PowerShot G1"]
                    //    td [][text "2000"]
                    //    td [] [
                    //        div [clazz "outer"] [
                    //            div [clazz "inner"; clazz "maxRes"] [text "36%"]
                    //        ]
                    //    ]
                    //    td [] [
                    //        div [clazz "outer"] [
                    //            div [clazz "inner"; clazz "lowRes"] [text "45%"]
                    //        ]
                    //    ]
                    //]
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
