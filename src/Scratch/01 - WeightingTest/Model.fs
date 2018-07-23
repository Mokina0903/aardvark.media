namespace Model

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives

type Message = 
    | SetWeight of float
    | MouseMove of V2d
    | StartDrag
    | StopDrag
     
[<DomainType>]
type SimpleWeighting = 
    {
        weight : float
        cursor : Option<float>
        dragging : bool
    }

type LightingParameters =
    {
        cost : SimpleWeighting
        energy : SimpleWeighting
    }
