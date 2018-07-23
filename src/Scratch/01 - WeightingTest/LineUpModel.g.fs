namespace LineUpModel

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open LineUpModel

[<AutoOpen>]
module Mutable =

    
    
    type MLineUp(__initial : LineUpModel.LineUp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<LineUpModel.LineUp> = Aardvark.Base.Incremental.EqModRef<LineUpModel.LineUp>(__initial) :> Aardvark.Base.Incremental.IModRef<LineUpModel.LineUp>
        let _input = ResetMod.Create(__initial.input)
        let _config = ResetMod.Create(__initial.config)
        
        member x.input = _input :> IMod<_>
        member x.config = _config :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : LineUpModel.LineUp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_input,v.input)
                ResetMod.Update(_config,v.config)
                
        
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
