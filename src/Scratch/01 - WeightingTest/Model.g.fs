namespace Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Model

[<AutoOpen>]
module Mutable =

    
    
    type MSimpleWeighting(__initial : Model.SimpleWeighting) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Model.SimpleWeighting> = Aardvark.Base.Incremental.EqModRef<Model.SimpleWeighting>(__initial) :> Aardvark.Base.Incremental.IModRef<Model.SimpleWeighting>
        let _weight = ResetMod.Create(__initial.weight)
        let _cursor = MOption.Create(__initial.cursor)
        let _dragging = ResetMod.Create(__initial.dragging)
        
        member x.weight = _weight :> IMod<_>
        member x.cursor = _cursor :> IMod<_>
        member x.dragging = _dragging :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Model.SimpleWeighting) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_weight,v.weight)
                MOption.Update(_cursor, v.cursor)
                ResetMod.Update(_dragging,v.dragging)
                
        
        static member Create(__initial : Model.SimpleWeighting) : MSimpleWeighting = MSimpleWeighting(__initial)
        static member Update(m : MSimpleWeighting, v : Model.SimpleWeighting) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Model.SimpleWeighting> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SimpleWeighting =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let weight =
                { new Lens<Model.SimpleWeighting, System.Double>() with
                    override x.Get(r) = r.weight
                    override x.Set(r,v) = { r with weight = v }
                    override x.Update(r,f) = { r with weight = f r.weight }
                }
            let cursor =
                { new Lens<Model.SimpleWeighting, Microsoft.FSharp.Core.Option<System.Double>>() with
                    override x.Get(r) = r.cursor
                    override x.Set(r,v) = { r with cursor = v }
                    override x.Update(r,f) = { r with cursor = f r.cursor }
                }
            let dragging =
                { new Lens<Model.SimpleWeighting, System.Boolean>() with
                    override x.Get(r) = r.dragging
                    override x.Set(r,v) = { r with dragging = v }
                    override x.Update(r,f) = { r with dragging = f r.dragging }
                }
