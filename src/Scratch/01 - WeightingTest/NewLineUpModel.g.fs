namespace NewLineUpModel

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open NewLineUpModel

[<AutoOpen>]
module Mutable =

    
    
    type MTable(__initial : NewLineUpModel.Table) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<NewLineUpModel.Table> = Aardvark.Base.Incremental.EqModRef<NewLineUpModel.Table>(__initial) :> Aardvark.Base.Incremental.IModRef<NewLineUpModel.Table>
        let _header = ResetMod.Create(__initial.header)
        let _weights = ResetMod.Create(__initial.weights)
        let _rows = ResetMod.Create(__initial.rows)
        let _visibleOrder = ResetMod.Create(__initial.visibleOrder)
        let _scores = ResetMod.Create(__initial.scores)
        
        member x.header = _header :> IMod<_>
        member x.weights = _weights :> IMod<_>
        member x.rows = _rows :> IMod<_>
        member x.visibleOrder = _visibleOrder :> IMod<_>
        member x.scores = _scores :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : NewLineUpModel.Table) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_header,v.header)
                ResetMod.Update(_weights,v.weights)
                ResetMod.Update(_rows,v.rows)
                ResetMod.Update(_visibleOrder,v.visibleOrder)
                ResetMod.Update(_scores,v.scores)
                
        
        static member Create(__initial : NewLineUpModel.Table) : MTable = MTable(__initial)
        static member Update(m : MTable, v : NewLineUpModel.Table) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<NewLineUpModel.Table> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Table =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let header =
                { new Lens<NewLineUpModel.Table, Microsoft.FSharp.Collections.Map<System.String,NewLineUpModel.Attribute>>() with
                    override x.Get(r) = r.header
                    override x.Set(r,v) = { r with header = v }
                    override x.Update(r,f) = { r with header = f r.header }
                }
            let weights =
                { new Lens<NewLineUpModel.Table, Microsoft.FSharp.Collections.Map<System.String,Microsoft.FSharp.Core.Option<System.Double>>>() with
                    override x.Get(r) = r.weights
                    override x.Set(r,v) = { r with weights = v }
                    override x.Update(r,f) = { r with weights = f r.weights }
                }
            let rows =
                { new Lens<NewLineUpModel.Table, NewLineUpModel.Row[]>() with
                    override x.Get(r) = r.rows
                    override x.Set(r,v) = { r with rows = v }
                    override x.Update(r,f) = { r with rows = f r.rows }
                }
            let visibleOrder =
                { new Lens<NewLineUpModel.Table, Microsoft.FSharp.Collections.List<System.String>>() with
                    override x.Get(r) = r.visibleOrder
                    override x.Set(r,v) = { r with visibleOrder = v }
                    override x.Update(r,f) = { r with visibleOrder = f r.visibleOrder }
                }
            let scores =
                { new Lens<NewLineUpModel.Table, System.Double[]>() with
                    override x.Get(r) = r.scores
                    override x.Set(r,v) = { r with scores = v }
                    override x.Update(r,f) = { r with scores = f r.scores }
                }