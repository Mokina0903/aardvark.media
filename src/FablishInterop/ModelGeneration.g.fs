namespace Scratch.DomainTypes2

[<AutoOpen>]
module Generated = 
    open System
    open Aardvark.Base
    open Aardvark.Base.Incremental
    
    module CameraTest = 
        open Aardvark.Base
        open Aardvark.Base.Rendering
        
        [<DomainType>]
        type Model = 
            { mutable _id : Id
              camera : CameraView
              frustum : Frustum
              lookingAround : Option<PixelPosition> }
            
            member x.ToMod(reuseCache : ReuseCache) = 
                { _original = x
                  mcamera = Mod.init (x.camera)
                  mfrustum = Mod.init (x.frustum)
                  mlookingAround = Mod.init (x.lookingAround) }
            
            interface IUnique with
                
                member x.Id 
                    with get () = x._id
                    and set v = x._id <- v
        
        and [<DomainType>] MModel = 
            { mutable _original : Model
              mcamera : ModRef<CameraView>
              mfrustum : ModRef<Frustum>
              mlookingAround : ModRef<Option<PixelPosition>> }
            member x.Apply(arg0 : Model, reuseCache : ReuseCache) = 
                if not (System.Object.ReferenceEquals(arg0, x._original)) then 
                    x._original <- arg0
                    x.mcamera.Value <- arg0.camera
                    x.mfrustum.Value <- arg0.frustum
                    x.mlookingAround.Value <- arg0.lookingAround