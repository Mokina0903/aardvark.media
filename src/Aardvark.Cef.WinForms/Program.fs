﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

#nowarn "9"

open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Windows.Forms
open Microsoft.FSharp.NativeInterop

open Aardvark.Base
open Aardvark.Cef
open Aardvark.Cef.WinForms
open Xilium.CefGlue
open Xilium.CefGlue.WindowsForms

open Aardvark.Cef.Internal.CefExtensions
module IPC = Aardvark.Cef.Internal.IPC

type LoadHandler(parent : BrowserControl) =
    inherit CefLoadHandler()

    override x.OnLoadStart(browser : CefBrowser, frame : CefFrame, transitionType : CefTransitionType) =
        parent.SetBrowser(browser, frame)
        base.OnLoadStart(browser, frame, transitionType)

    override x.OnLoadEnd(browser : CefBrowser, frame : CefFrame, status : int) =
        parent.LoadFinished()
        base.OnLoadEnd(browser, frame, status)

    override x.OnLoadError(browser : CefBrowser, frame : CefFrame, errorCode : CefErrorCode, errorText : string, failedUrl : string) =
        parent.LoadFaulted(errorText, failedUrl)
        base.OnLoadError(browser, frame, errorCode, errorText, failedUrl)

and ResourceHandler(parent : BrowserControl, content : Content) =
    inherit CefResourceHandler()

    let mime, content = 
        match content with
            | Error ->
                "error", [||]
            | Css content ->
                "text/css", System.Text.Encoding.UTF8.GetBytes(content)
            | Javascript content ->
                "text/javascript", System.Text.Encoding.UTF8.GetBytes(content)
            | Html content ->
                "text/html", System.Text.Encoding.UTF8.GetBytes(content)
            | Binary content ->
                "application/octet-stream", content

    let mutable offset = 0L
    let mutable remaining = content.LongLength

    override x.ProcessRequest(request : CefRequest, callback : CefCallback) =
        if mime = "error" then
            false
        else
            callback.Continue()
            callback.Dispose()
            true

    override x.GetResponseHeaders(response : CefResponse, response_length : byref<int64>, redirectUrl : byref<string>) =
        response_length <- content.LongLength
        redirectUrl <- null
        response.MimeType <- mime
        response.Status <- 200
        response.StatusText <- "OK"
        response.Error <- CefErrorCode.None

    override x.ReadResponse(data_out : Stream, bytes_to_read : int, bytes_read : byref<int>, callback : CefCallback) =
        callback.Dispose()
        if remaining > 0L then
            let actual = min (int64 bytes_to_read) remaining

//            let data_out = unbox<UnmanagedMemoryStream> data_out
//            Marshal.Copy(content, int offset, NativePtr.toNativeInt data_out.PositionPointer, int actual)

            data_out.Write(content, int offset, int actual)

            offset <- offset + actual
            remaining <- remaining - actual
            bytes_read <- int actual
            true
        else
            bytes_read <- 0
            false

    override x.CanGetCookie(c : CefCookie) = true
    override x.CanSetCookie(c : CefCookie) = true
    override x.Cancel() = ()

and RequestHandler(parent : BrowserControl) =
    inherit CefRequestHandler()

    override x.GetResourceHandler(browser : CefBrowser, frame : CefFrame, request : CefRequest) =
        match parent.TryGetPage request.Url with
            | (true, page) -> 
                ResourceHandler(parent, page) :> CefResourceHandler
            | _ -> 
                base.GetResourceHandler(browser, frame, request)
        
and BrowserClient(parent : BrowserControl) =
    inherit CefWebClient(parent)

    let loadHandler = LoadHandler(parent)
    let requestHandler = RequestHandler(parent)
    override x.OnProcessMessageReceived(source, proc, msg) =
        parent.OnProcessMessageReceived(source, proc, msg)
        true

    override x.GetLoadHandler() =
        loadHandler :> CefLoadHandler

    override x.GetRequestHandler() =
        requestHandler :> CefRequestHandler

and DevToolsWebClient(parent : BrowserControl) =
    inherit CefClient()

and BrowserControl() as this =
    inherit CefWebBrowser()
    static let queryRx = System.Text.RegularExpressions.Regex @"(\?|\&)(?<name>[^\=]+)\=(?<value>[^\&]*)"

    static let parseQuery (q : string) =
        let mutable res = Map.empty
        for m in queryRx.Matches q do
            let name = m.Groups.["name"].Value
            let value = m.Groups.["value"].Value
            res <- Map.add name value res
        res


    let mutable bf = Unchecked.defaultof<_>
    let ownBrowser = System.Threading.Tasks.TaskCompletionSource<CefBrowser * CefFrame>()

    let pages = System.Collections.Generic.Dictionary<string, Map<string, string> -> Content>()

    let pending = System.Collections.Concurrent.ConcurrentDictionary<IPC.MessageToken, System.Threading.Tasks.TaskCompletionSource<IPC.Reply>>()
    let events = Event<_>()
    let client = BrowserClient(this)

    let mutable devTools = false
    

    let showDevTools (x : BrowserControl) =
        let form = x.FindForm()
        let host = x.Browser.GetHost()
        let wi = CefWindowInfo.Create();
        wi.SetAsPopup(0n, "Developer Tools");
        wi.Width <- 500
        wi.Height <- form.Height
        wi.X <- form.DesktopLocation.X + form.Width
        wi.Y <- form.DesktopLocation.Y
        
        host.ShowDevTools(wi, new DevToolsWebClient(x), new CefBrowserSettings(), new CefPoint(-100, -100));

    let closeDevTools (x : BrowserControl) =
        let host = x.Browser.GetHost()
        host.CloseDevTools()

    member x.RemovePage(url : string) =
        let url = System.Uri(url)
        let path = url.GetLeftPart(System.UriPartial.Path)
        pages.Remove path

    member x.TryGetPage(url : string, [<Out>] pageContent : byref<Content>) =
        let url = System.Uri(url)
        let path = url.GetLeftPart(System.UriPartial.Path)
        match pages.TryGetValue path with
            | (true, c) -> pageContent <- c (parseQuery url.Query); true
            | _ -> false

    member x.Item
        with set (url : string) (content : Map<string, string> -> Content) = 
            let url = System.Uri(url)
            let path = url.GetLeftPart(System.UriPartial.Path)
            pages.[path] <- content

    member x.SetBrowser(browser : CefBrowser, frame : CefFrame) =

        printfn "SetBrowser"
        bf <- (browser, frame)

    member x.LoadFinished() =
        printfn "LoadFinished"
        ownBrowser.SetResult bf

    member x.LoadFaulted(err : string, url : string) =
        printfn "LoadFaulted"

    member x.Start(js : string) =
        async {
            let! (browser, frame) = ownBrowser.Task |> Async.AwaitTask
            browser.Send(CefProcessId.Renderer, IPC.Execute(IPC.MessageToken.Null, js))
        } |> Async.Start

    member x.Run(js : string) =
        async {
            let! (browser, frame) = ownBrowser.Task |> Async.AwaitTask
            printfn "running"
            let token = IPC.MessageToken.New
            let tcs = System.Threading.Tasks.TaskCompletionSource()
            pending.[token] <- tcs

            browser.Send(CefProcessId.Renderer, IPC.Execute(token, js))

            let! res = tcs.Task |> Async.AwaitTask
            return res
        }

    [<CLIEvent>]
    member x.Events = events.Publish

    member x.Client = client

    member x.OnProcessMessageReceived(source : CefBrowser, proc : CefProcessId, msg : CefProcessMessage) =
        match IPC.tryGet<Aardvark.Cef.Internal.Event> msg with
            | Some evt ->
                System.Threading.Tasks.Task.Factory.StartNew (fun () -> events.Trigger evt) |> ignore
            | None -> 
                match IPC.tryGet<IPC.Reply> msg with
                    | Some reply ->
                        match pending.TryRemove reply.Token with
                            | (true, tcs) -> tcs.SetResult reply
                            | _ -> ()

                    | _ ->
                        ()

    member x.ShowDevTools() =
        if not devTools then
            devTools <- true

            if ownBrowser.Task.IsCompleted then
                showDevTools x
            else
                x.BrowserCreated.Add (fun _ -> 
                    if devTools then
                        showDevTools x
                )
            
    member x.CloseDevTools() =
        if devTools then
            devTools <- false
            
            if ownBrowser.Task.IsCompleted then
                closeDevTools x

    override x.CreateWebClient() =
        client :> CefWebClient

open System
open System.IO
open System.Diagnostics
open Aardvark.Base.Incremental
open Aardvark.Rendering
open Aardvark.Rendering.GL
open Aardvark.Application
open Aardvark.Application.WinForms
open OpenTK.Graphics.OpenGL



type CefRenderControl(runtime : IRuntime, parent : BrowserControl, id : string) =

    static let queryRx = System.Text.RegularExpressions.Regex @"(\?|\&)(?<name>[^\=]+)\=(?<value>[^\&]*)"

    let backgroundColor = Mod.init C4f.Black
    let currentSize = Mod.init V2i.II
    let signature =
        runtime.CreateFramebufferSignature(
            1, [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba8
                DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
            ]
        )

    let framebuffer = runtime.CreateFramebuffer(signature, Set.ofList [], currentSize)
    do framebuffer.Acquire()

    //let colorTexture = framebuffer.GetOutputTexture(DefaultSemantic.Colors)
    //let colorImage = currentSize |> Mod.map (fun s -> PixImage<byte>(Col.Format.RGBA, s))
    let clearTask = runtime.CompileClear(signature, backgroundColor, Mod.constant 1.0)
    let mutable renderTask = RenderTask.empty
    let time = Mod.custom (fun _ -> DateTime.Now)
    
    let mutable frameCounter = 0
    let invalidateTime = Stopwatch()
    let renderTime = Stopwatch()
    let downloadTime = Stopwatch()
    let transferTime = Stopwatch()
    let presentTime = Stopwatch()
    let totalTime = Stopwatch()

    let outputThing = AdaptiveObject()
    let renderResult =
        Mod.custom (fun self ->
            renderTime.Start()
            use t = runtime.ContextLock

            let fbo = framebuffer.GetValue self
            clearTask.Run(self, RenderToken.Empty, OutputDescription.ofFramebuffer fbo)
            renderTask.Run(self, RenderToken.Empty, OutputDescription.ofFramebuffer fbo)

            renderTime.Stop()

            downloadTime.Start()
            let fbo = unbox<Framebuffer> fbo
            let color = unbox<Renderbuffer> fbo.Attachments.[DefaultSemantic.Colors]



            let size = color.Size
            let sizeInBytes = 4 * size.X * size.Y 

            GL.BindFramebuffer(FramebufferTarget.Framebuffer, fbo.Handle)
            let b = GL.GenBuffer()
            GL.BindBuffer(BufferTarget.PixelPackBuffer, b)
            GL.BufferStorage(BufferTarget.PixelPackBuffer, 4n * nativeint sizeInBytes, 0n, BufferStorageFlags.MapReadBit)
            
            GL.ReadPixels(0, 0, size.X, size.Y, PixelFormat.Rgba, PixelType.UnsignedByte, 0n)

            let ptr = GL.MapBuffer(BufferTarget.PixelPackBuffer, BufferAccess.ReadOnly)
            let image = PixImage<byte>(Col.Format.RGBA, size)
            Marshal.Copy(ptr, image.Volume.Data, 0, int sizeInBytes)
            GL.UnmapBuffer(BufferTarget.PixelPackBuffer) |> ignore
            
            GL.BindBuffer(BufferTarget.PixelPackBuffer, 0)
            GL.DeleteBuffer(b)
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
            downloadTime.Stop()
            image
        )


    let queue = new System.Collections.Concurrent.BlockingCollection<Choice<unit, System.Threading.Tasks.TaskCompletionSource<PixImage<byte>>>>()

    let renderer = 
        async {
            do! Async.SwitchToNewThread()
            let mutable lastToken = AdaptiveToken.Top
            while true do
                let e = queue.Take()
                try
                    match e with
                        | Choice2Of2 tcs ->
                            let t = AdaptiveToken.Top
                            let res = renderResult.GetValue(t.WithCaller outputThing)
                            lastToken <- t
                            tcs.SetResult res
                        | Choice1Of2 () ->
                            lastToken.Release()
                            outputThing.OutOfDate <- false
                            transact (fun () -> time.MarkOutdated())
                with e ->
                    Log.warn "%A" e
        }

    do Async.Start renderer

    let cb = 
        outputThing.AddMarkingCallback(fun () ->
            totalTime.Start()
            invalidateTime.Start()
            parent.Start (sprintf "render('%s');" id)
        )

    do  parent.Events.Add (fun e ->
            if e.sender = id then
                if e.name = "received" then
                    transferTime.Stop()
                    presentTime.Start()

                elif e.name = "rendered" then
                    queue.Add(Choice1Of2 ())
                    presentTime.Stop()
                    totalTime.Stop()

                    frameCounter <- frameCounter + 1

                    if frameCounter >= 100 then
                        let total = invalidateTime.MicroTime + renderTime.MicroTime + downloadTime.MicroTime + transferTime.MicroTime + presentTime.MicroTime
                        let unaccounted = totalTime.MicroTime - total
                        let percent (m : MicroTime) =
                            100.0 * m / totalTime.MicroTime

                        

                        Log.start "timing"
                        Log.line "invalidate:  %A (%.2f%%)" (invalidateTime.MicroTime / float frameCounter) (percent invalidateTime.MicroTime)
                        Log.line "render:      %A (%.2f%%)" (renderTime.MicroTime / float frameCounter) (percent renderTime.MicroTime)
                        Log.line "download:    %A (%.2f%%)" (downloadTime.MicroTime / float frameCounter) (percent downloadTime.MicroTime)
                        Log.line "transfer:    %A (%.2f%%)" (transferTime.MicroTime / float frameCounter) (percent transferTime.MicroTime)
                        Log.line "present:     %A (%.2f%%)" (presentTime.MicroTime / float frameCounter) (percent presentTime.MicroTime)
                        Log.line "unaccounted: %A (%.2f%%)" (unaccounted / float frameCounter) (percent unaccounted)
                        Log.line "total:       %A (%.2ffps)" (totalTime.MicroTime / float frameCounter) (float frameCounter / totalTime.Elapsed.TotalSeconds)
                        Log.stop()

                        invalidateTime.Reset()
                        renderTime.Reset()
                        downloadTime.Reset()
                        transferTime.Reset()
                        presentTime.Reset()
                        totalTime.Reset()
                        frameCounter <- 0

        )

    let binaryData (query : Map<string, string>) =
        invalidateTime.Stop()
        match Map.tryFind "w" query, Map.tryFind "h" query with
            | Some w, Some h ->
                match Int32.TryParse w, Int32.TryParse h with
                    | (true, w), (true, h) ->
                        let size = V2i(w,h)
                        if size <> currentSize.Value then
                            transact (fun () -> currentSize.Value <- size)

                        let tcs = System.Threading.Tasks.TaskCompletionSource<PixImage<byte>>()
                        queue.Add(Choice2Of2 tcs)

                        let image = tcs.Task.Result
                        transferTime.Start()
                        Binary image.Volume.Data
                    | _ ->
                        Error
            | _ ->
                Error
                        

    let url = sprintf "http://aardvark.local/render/%s" id

    let keyboard = EventKeyboard()
    let mouse = EventMouse(false)
    let mutable lastPos = PixelPosition()

    let pos x y =
        let res = PixelPosition(x,y,currentSize.Value.X, currentSize.Value.Y)
        lastPos <- res
        res

    let button b =
        match b with
            | 0 -> MouseButtons.Left
            | 1 -> MouseButtons.Middle
            | 2 -> MouseButtons.Right
            | _ -> MouseButtons.None

    do parent.[url] <- binaryData


    let eventSubscription =
        parent.Events.Subscribe (fun e ->
            if e.sender = id then
                let s = currentSize.Value

                match e.name with
                    | "keydown" ->
                        let key = Int32.Parse e.args.[0] |> KeyConverter.keyFromVirtualKey
                        keyboard.KeyDown(key)

                    | "keyup" ->
                        let key = Int32.Parse e.args.[0] |> KeyConverter.keyFromVirtualKey
                        keyboard.KeyUp(key)

                    | "keypress" ->
                        let c = e.args.[0].[0]
                        keyboard.KeyPress(c)
                        
                    | "click" ->
                        let x = Int32.Parse e.args.[0]
                        let y = Int32.Parse e.args.[1]
                        let b = Int32.Parse e.args.[2] |> button
                        mouse.Click(pos x y, b)

                    | "dblclick" ->
                        let x = Int32.Parse e.args.[0]
                        let y = Int32.Parse e.args.[1]
                        let b = Int32.Parse e.args.[2] |> button
                        mouse.DoubleClick(pos x y, b)

                    | "mousedown" ->
                        let x = Int32.Parse e.args.[0]
                        let y = Int32.Parse e.args.[1]
                        let b = Int32.Parse e.args.[2] |> button
                        mouse.Down(pos x y, b)

                    | "mouseup" ->
                        let x = Int32.Parse e.args.[0]
                        let y = Int32.Parse e.args.[1]
                        let b = Int32.Parse e.args.[2] |> button
                        mouse.Up(pos x y, b)
                        
                    | "mousemove" ->
                        let x = Int32.Parse e.args.[0]
                        let y = Int32.Parse e.args.[1]
                        mouse.Move(pos x y)
                        
                    | "mouseenter" ->
                        let x = Int32.Parse e.args.[0]
                        let y = Int32.Parse e.args.[1]
                        mouse.Enter(pos x y)
                        
                    | "mouseout" ->
                        let x = Int32.Parse e.args.[0]
                        let y = Int32.Parse e.args.[1]
                        mouse.Leave(pos x y)
                        
                    | "mousewheel" ->
                        let delta = Double.Parse(e.args.[0], System.Globalization.CultureInfo.InvariantCulture)
                        mouse.Scroll(lastPos, delta)
                       
                    | _ ->
                        ()



        )

    member x.Background
        with get() = backgroundColor.Value
        and set v = transact (fun () -> backgroundColor.Value <- v)

    member x.Dispose() =
        eventSubscription.Dispose()
        parent.RemovePage url |> ignore
        framebuffer.Release()
        clearTask.Dispose()
        renderTask.Dispose()
        renderTask <- RenderTask.empty

    member x.RenderTask
        with get() = renderTask
        and set v = 
            renderTask.Dispose()
            renderTask <- v

    member x.Sizes = currentSize :> IMod<_>

    member x.FramebufferSignature = signature

    member x.Time = time

    member x.Samples = 1
    
    member x.Runtime = runtime
    
    member x.Keyboard = keyboard :> IKeyboard
    member x.Mouse = mouse :> IMouse


    interface IRenderTarget with
        member x.FramebufferSignature = x.FramebufferSignature
        member x.RenderTask 
            with get() = x.RenderTask
            and set t = x.RenderTask <- t
        member x.Runtime = x.Runtime
        member x.Sizes = x.Sizes
        member x.Samples = x.Samples
        member x.Time = x.Time

    interface IRenderControl with
        member x.Keyboard = x.Keyboard
        member x.Mouse = x.Mouse

    interface IDisposable with
        member x.Dispose() = x.Dispose()





//
//type ChromeApplication(runtime : IRuntime) =
//    
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

[<EntryPoint>]
let main argv = 
    Ag.initialize()
    Aardvark.Init()
    

    let app = new OpenGlApplication()
    let server = new AardvarkServer(app.Runtime, 8888);

    server.["yeah"] <- fun (yeah : IRenderControl) ->
        let view = CameraView.lookAt (V3d.III * 6.0) V3d.Zero V3d.OOI
        let proj = yeah.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))

        let view =
            view |> DefaultCameraController.control yeah.Mouse yeah.Keyboard yeah.Time

        let sg =
            Sg.box' C4b.Red (Box3d(-V3d.III, V3d.III))
                |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
                |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.simpleLighting
                }

        app.Runtime.CompileRender(yeah.FramebufferSignature, sg)


    Console.ReadLine() |> ignore
    Environment.Exit 0

    Chromium.init' false
    // create a browser and a nested renderControl
    use ctrl = new CefWebBrowser()
    ctrl.StartUrl <- "http://localhost:8888/"

    // create a form containing the browser
    use form = new Form()
    form.Width <- 1024
    form.Height <- 768
    ctrl.Dock <- DockStyle.Fill
    form.Controls.Add ctrl
    
    

    // run it
    Application.Run form
    Chromium.shutdown()

    0 