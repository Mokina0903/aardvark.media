module App

open Aardvark.UI
open Aardvark.UI.Primitives

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Model
open Aardvark.UI


let rnd = System.Random()

let update (model : SimpleWeighting) (msg : Message) =
    match msg with
        | SetWeight v -> { model with weight = v }
        | MouseMove(coord) -> 
            if model.dragging then { model with weight = coord.X |> clamp 0.0  1.0 }
            else model
        | StartDrag -> { model with dragging = true }
        | StopDrag -> { model with dragging = false }
        

let onMouseMoveRel (cb : V2d -> 'msg) : Attribute<'msg> =
    onEvent "onmousemove" [" toFixedV2d(relativePerc(event,'container'))"] (List.head >> Pickler.json.UnPickleOfString >> cb)


let dependencies = [
    { name = "helpers"; url = "resources/Helpers.js"; kind = Script }
    { name = "WeightingCSS"; url = "resources/Weighting.css"; kind = Stylesheet }
]

// variant with html5 grid layouting (currently not working in our cef)
let view (model : MSimpleWeighting) =
    
    let ourText =
        adaptive {
            let! w = model.weight
            return string w
        }

    let attributesA =
        amap {
            let! weight = model.weight
            let s = sprintf "background-color: blue; width: %f%%; height: 100%%; position: absolute; left: 0px" (weight * 100.0)
            yield style s
        } |> AttributeMap.ofAMap

    let attributesB =
        amap {
            let! weight = model.weight
            let s = sprintf "background-color: red; width: %f%%; height: 100%%; position: absolute; left: %f%%" ((1.0 - weight)*100.0) (weight * 100.0)
            yield style s
        } |> AttributeMap.ofAMap

    let cursor =
        amap {
            let! weight = model.weight
            let s = sprintf "background-color: gray; width: 5%%; height: 100%%; position: absolute; left: %f%%" ((weight-0.025) * 100.0)
            yield onMouseDown (fun _ _ -> StartDrag)
            yield onMouseUp (fun _ _ -> StopDrag)
            yield clazz "mycursor"
            yield style s
        } |> AttributeMap.ofAMap

    body [onMouseUp (fun _ _ -> StopDrag); onMouseMoveRel MouseMove] [
        require (dependencies @ Html.semui) (
            div [] [
                yield button [clazz "ui small button"; onClick (fun _ -> SetWeight 0.5)] [text "Half"]
                yield button [clazz "ui small button"; onClick (fun _ -> SetWeight 1.0)] [text "All A"]
                yield br []
                for i in 0 .. 10 do
                    yield div [ 
                          clazz "container"
                          onMouseMoveRel MouseMove; style "left: 10px; width: 200px; height: 50px; border-style: solid; position:relative"] [
                            Incremental.div attributesA AList.empty
                            Incremental.div attributesB AList.empty
                            Incremental.div cursor AList.empty
                    ]
                    yield br []
                yield Incremental.text ourText
            ]
        )
    ]


let threads (model : SimpleWeighting) = 
    ThreadPool.empty

let app =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
    {
        unpersist = Unpersist.instance     
        threads = threads 
        initial = 
            { 
               weight = 0.5
               cursor = None
               dragging = false
            }
        update = update 
        view = view
    }
