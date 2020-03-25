namespace LineUpModel

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open LineUpModel

[<AutoOpen>]
module Mutable =

    
    
    type MVisibleAttribute(__initial : LineUpModel.VisibleAttribute) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<LineUpModel.VisibleAttribute> = Aardvark.Base.Incremental.EqModRef<LineUpModel.VisibleAttribute>(__initial) :> Aardvark.Base.Incremental.IModRef<LineUpModel.VisibleAttribute>
        let _name = ResetMod.Create(__initial.name)
        let _weight = MOption.Create(__initial.weight)
        
        member x.sortKey = __current.Value.sortKey
        member x.name = _name :> IMod<_>
        member x.weight = _weight :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : LineUpModel.VisibleAttribute) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_name,v.name)
                MOption.Update(_weight, v.weight)
                
        
        static member Create(__initial : LineUpModel.VisibleAttribute) : MVisibleAttribute = MVisibleAttribute(__initial)
        static member Update(m : MVisibleAttribute, v : LineUpModel.VisibleAttribute) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<LineUpModel.VisibleAttribute> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module VisibleAttribute =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let sortKey =
                { new Lens<LineUpModel.VisibleAttribute, System.Int32>() with
                    override x.Get(r) = r.sortKey
                    override x.Set(r,v) = { r with sortKey = v }
                    override x.Update(r,f) = { r with sortKey = f r.sortKey }
                }
            let name =
                { new Lens<LineUpModel.VisibleAttribute, System.String>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let weight =
                { new Lens<LineUpModel.VisibleAttribute, Microsoft.FSharp.Core.Option<System.Double>>() with
                    override x.Get(r) = r.weight
                    override x.Set(r,v) = { r with weight = v }
                    override x.Update(r,f) = { r with weight = f r.weight }
                }
    
    
    type MLineUp(__initial : LineUpModel.LineUp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<LineUpModel.LineUp> = Aardvark.Base.Incremental.EqModRef<LineUpModel.LineUp>(__initial) :> Aardvark.Base.Incremental.IModRef<LineUpModel.LineUp>
        let _input = ResetMod.Create(__initial.input)
        let _config = ResetMod.Create(__initial.config)
        let _visibleAttributes = MMap.Create(__initial.visibleAttributes, (fun v -> MVisibleAttribute.Create(v)), (fun (m,v) -> MVisibleAttribute.Update(m, v)), (fun v -> v))
        let _weights = ResetMod.Create(__initial.weights)
        
        member x.input = _input :> IMod<_>
        member x.config = _config :> IMod<_>
        member x.visibleAttributes = _visibleAttributes :> amap<_,_>
        member x.weights = _weights :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : LineUpModel.LineUp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_input,v.input)
                ResetMod.Update(_config,v.config)
                MMap.Update(_visibleAttributes, v.visibleAttributes)
                ResetMod.Update(_weights,v.weights)
                
        
        static member Create(__initial : LineUpModel.LineUp) : MLineUp = MLineUp(__initial)
        static member Update(m : MLineUp, v : LineUpModel.LineUp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<LineUpModel.LineUp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LineUp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let input =
                { new Lens<LineUpModel.LineUp, LineUpModel.Table>() with
                    override x.Get(r) = r.input
                    override x.Set(r,v) = { r with input = v }
                    override x.Update(r,f) = { r with input = f r.input }
                }
            let config =
                { new Lens<LineUpModel.LineUp, LineUpModel.Config>() with
                    override x.Get(r) = r.config
                    override x.Set(r,v) = { r with config = v }
                    override x.Update(r,f) = { r with config = f r.config }
                }
            let visibleAttributes =
                { new Lens<LineUpModel.LineUp, Aardvark.Base.hmap<System.String,LineUpModel.VisibleAttribute>>() with
                    override x.Get(r) = r.visibleAttributes
                    override x.Set(r,v) = { r with visibleAttributes = v }
                    override x.Update(r,f) = { r with visibleAttributes = f r.visibleAttributes }
                }
            let weights =
                { new Lens<LineUpModel.LineUp, Microsoft.FSharp.Collections.List<LineUpModel.Weight>>() with
                    override x.Get(r) = r.weights
                    override x.Set(r,v) = { r with weights = v }
                    override x.Update(r,f) = { r with weights = f r.weights }
                }
