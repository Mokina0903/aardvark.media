﻿namespace Aardvark.UI

open Aardvark.Base
open Aardvark.Base.Incremental

type Attribute<'msg> = string * AttributeValue<'msg>

[<AutoOpen>]
module Attributes =
    let inline attribute (key : string) (value : string) : Attribute<'msg> = 
        key, AttributeValue.String value

    let inline clazz value = attribute "class" value
    
    let inline style value = attribute "style" value
    
    let inline js (name : string) (code : string) : Attribute<'msg> =
        name, 
        AttributeValue.Event { 
            clientSide = fun send id -> code.Replace("__ID__", id)
            serverSide = fun _ _ _ -> []
        }


[<AutoOpen>]
module Events =
    open Aardvark.Application


    let internal button (str : string) =
        match int (float str) with
            | 1 -> MouseButtons.Left
            | 2 -> MouseButtons.Middle
            | 3 -> MouseButtons.Right
            | _ -> MouseButtons.None

    let inline onEvent (eventType : string) (args : list<string>) (cb : list<string> -> 'msg) : Attribute<'msg> = 
        eventType, AttributeValue.Event(Event.ofDynamicArgs args (cb >> List.singleton))

    let onFocus (cb : unit -> 'msg) =
        onEvent "onfocus" [] (ignore >> cb)
        
    let onBlur (cb : unit -> 'msg) =
        onEvent "onblur" [] (ignore >> cb)

    let onMouseEnter (cb : V2i -> 'msg) =
        onEvent "onmouseenter" ["{ X: event.clientX, Y: event.clientY  }"] (List.head >> Pickler.json.UnPickleOfString >> cb)

    let onMouseLeave (cb : V2i -> 'msg) =
        onEvent "onmouseout" ["{ X: event.clientX, Y: event.clientY  }"] (List.head >> Pickler.json.UnPickleOfString >> cb)
        
    let onMouseMove (cb : V2i -> 'msg) = 
        onEvent "onmousemove" ["{ X: event.clientX, Y: event.clientY  }"] (List.head >> Pickler.json.UnPickleOfString >> cb)

    let onMouseClick (cb : V2i -> 'msg) = 
        onEvent "onclick" ["{ X: event.clientX, Y: event.clientY  }"] (List.head >> Pickler.json.UnPickleOfString >> cb)

    let onMouseDoubleClick (cb : V2i -> 'msg) = 
        onEvent "ondblclick" ["{ X: event.clientX, Y: event.clientY  }"] (List.head >> Pickler.json.UnPickleOfString >> cb)
    
    let onContextMenu (cb : unit -> 'msg) = 
        onEvent "oncontextmenu" [] (ignore >> cb)
    
    let onChange (cb : string -> 'msg) = 
        onEvent "onchange" ["event.target.value"] (List.head >> Pickler.json.UnPickleOfString >> cb)

    let onMouseDown (cb : MouseButtons -> V2i -> 'msg) = 
        onEvent 
            "onmousedown" 
            ["event.clientX"; "event.clientY"; "event.which"] 
            (fun args ->
                match args with
                    | x :: y :: b :: _ ->
                        let x = int (float x)
                        let y = int (float y)
                        let b = button b
                        cb b (V2i(x,y))
                    | _ ->
                        failwith "asdasd"
            )

    let onMouseUp (cb : MouseButtons -> V2i -> 'msg) = 
        onEvent 
            "onmouseup" 
            ["event.clientX"; "event.clientY"; "event.which"] 
            (fun args ->
                match args with
                    | x :: y :: b :: _ ->
                        let x = int (float x)
                        let y = int (float y)
                        let b = button b
                        cb b (V2i(x,y))
                    | _ ->
                        failwith "asdasd"
            )

    let onClick (cb : unit -> 'msg) = onEvent "onclick" [] (ignore >> cb)

    let clientEvent (name : string) (cb : string) = js name cb

    let onKeyDown (cb : Keys -> 'msg) =
        "onkeydown" ,
        AttributeValue.Event(
            Event.ofDynamicArgs
                ["event.repeat"; "event.keyCode"]
                (fun args ->
                    match args with
                        | rep :: keyCode :: _ ->
                            if rep <> "true" then
                                let keyCode = int (float keyCode)
                                let key = KeyConverter.keyFromVirtualKey keyCode
                                [ cb key ]
                            else
                                []
                        | _ ->
                            []
                )
        )

    let onKeyUp (cb : Keys -> 'msg) =
        "onkeyup" ,
        AttributeValue.Event(
            Event.ofDynamicArgs
                ["event.keyCode"]
                (fun args ->
                    match args with
                        | keyCode :: _ ->
                            let keyCode = int (float keyCode)
                            let key = KeyConverter.keyFromVirtualKey keyCode
                            [cb key]
                        | _ ->
                            []
                )
        )

    module RenderControl =
        open Aardvark.Base.Geometry

        let onMouseMove (cb : RayPart -> Option<float> -> 'msg) =
            "RenderControl.onmousemove", AttributeValue.RenderControlEvent(fun e -> [cb e.ray (if e.rayT >= 0.0 then Some e.rayT else None)] )
            
    


    let always (att : Attribute<'msg>) =
        let (k,v) = att
        k, Mod.constant (Some v)

    let onlyWhen (m : IMod<bool>) (att : Attribute<'msg>) =
        let (k,v) = att
        k, m |> Mod.map (function true -> Some v | false -> None)