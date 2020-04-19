namespace NewLineUpModel

type Target = Min | Max
type Kind = Score | Plain | Bar of Target
type SortState = Asc | Desc | Inactive


type Value =
    | Missing
    | Int of int
    | Float of float
    | String of string

module Value = 
    let toFloat value = 
        match value with
            | String s -> 0.0
            | Float f -> f
            | Int i -> float i
            | Missing -> 0.0

type Statistics = 
    {
        min : float
        max : float
    }

type Attribute = 
    {
        name : string
        kind : Kind
        stats: Statistics
        sortState: SortState
        selected: bool
    }

type RowDescription = list<string * (string -> Value)>

type Row =
    {
        values : Map<string, Value>
    }


 open Aardvark.Base.Incremental
 open Aardvark.Base


 type WeightingFunction = list<string * float> -> string -> float -> list<string * float>

 module WeightingFunctions =

    //let absolutSplit (xs : list<float>) (oldValue : float) (delta : float) =
    //    let left,right = List.partition (fun v -> v <= oldValue) xs
    //    let leftDelta = -delta / (List.length left |> double)
    //    let rightDelta = delta / (List.length right |> double)
    //    (List.map ((+)leftDelta) left) @ List.map ((+)rightDelta) right

    /// > absolutSplit2 ["a",0.25;"b",0.25;"c",0.25;"d",0.25] "b" 0.1;;
    // val it : (string * float) list =
    //   [("a", 0.3); ("b", 0.3); ("c", 0.2); ("d", 0.2)]
    let absolutSplit2 (weights : list<string * float>) (draggedElement : string) (delta : float) =
        let index = List.findIndex (fun (name,_) -> name = draggedElement) weights
        let lefts = List.take (index+1) weights // contains element with index (inclusive)
        let rights = List.skip (index+1) weights
        // if postivie -> left becomes larger. this is the case we handle (other case is handled by negative delta)
        let deltaOnLeftSide = float delta / (float (List.length lefts))
        let deltaOnRightSide = - (float delta / (float (List.length rights)))
        let unnormalizedList = (List.map (fun (n,w) -> n, (w + deltaOnLeftSide) |> clamp 0.0 1.0) lefts) @ (List.map (fun (n,w) -> n, (w + deltaOnRightSide) |> clamp 0.0 1.0) rights)
        let factor = unnormalizedList |> List.sumBy snd
        unnormalizedList |> List.map (fun (n, w) -> n, w / factor)


type Message =
    | Nop
    | SetTargetMode of (string * Kind)
    | SetWeight of (string * float)
    | AddAttributeAt of (string * int)
    | AddAttribute of string
    | RemoveAttribute of string
    | CalculateScore
    | NormalizeWeight
    | ToggleOptions
    | Sort of string
    | Highlight of (string * bool)
    | Drag of string
    | StopDrag
    | MouseMove of V2d
    | Done
    | StartBench 

[<DomainType>]
type Table =
    {
        header : Map<string, Attribute>
        weights : Map<string, float> // pre filterd list contains only attribute of type BAR (numerical values)
        rows : array<Row>
        visibleOrder : List<string>
        showOptions: bool
        colors : Map<string, C4b>
        dragedAttribute: Option<string>
        weightingFunction : WeightingFunction

        lastUpdateTime : float
        threads : ThreadPool<Message>
    }

module Parsing =
    open System.Globalization
    open System.Numerics
    open System.IO
    open Aardvark.Base

    let example1 = @"..\..\..\src\Scratch\01 - WeightingTest\resources\Camera_reduced.csv" //this path... orly?

    let getRowDescription (nameLine : string) (typeLine: string) : RowDescription  =
        let names = nameLine.Split(';') |> Array.toList
        printfn  "%A" names
        let types = typeLine.Split(';') |> Array.toList
        let pairs = List.zip names types
        let nameParser =
            List.map (fun (n,t) ->
                let parserFunction (s : string) =
                    match t,s with
                        | _, "" -> Missing
                        | "STRING",s -> Value.String s
                        | "INTEGER",s -> 
                            match System.Int32.TryParse(s) with 
                                | (true,v) -> Int v
                                | _ -> failwithf "could not parse: %s (%s)" s n
                        | "DOUBLE",s -> 
                            match System.Double.TryParse(s.Replace(".",",")) with 
                                | (true,v) -> Float v
                                | _ -> failwithf "could not parse: %s (%s)" s n
                        | _ -> failwithf "no parser for: %s" s
                n, parserFunction
            ) pairs
        nameParser
    
    let getHeader (nameLine : string) (typeLine: string) : Map<string, Attribute> =
        let names = nameLine.Split(';') |> Array.toList
        let types = typeLine.Split(';') |> Array.toList
        let scoreAttribute = 
            {
                name  = "Score"
                kind = Score
                stats  = {min = 0.0; max = 0.0}
                sortState = Inactive
                selected = false
            }

        let pairs = List.zip names types     
        pairs |> List.mapi (fun i (n, t) -> 
            let k = 
                match t with
                    | "STRING" -> Plain
                    | "INTEGER" -> Bar Target.Max
                    | "DOUBLE" ->
                        match n with
                            | "Weight" | "Dimension" | "Price" -> Bar Target.Min 
                            | _ -> Bar Target.Max

                    | _ -> Plain
            let atr = 
                {
                    name  = n
                    kind = k
                    stats  = {min = 0.0; max = 0.0} //calculate here
                    sortState = Inactive
                    selected = false
                }
            (n, atr)
        ) |> Map.ofList |> Map.add "Score" scoreAttribute 

    let parseRow (desc : RowDescription) (line : string) : Row =  
        {
            //add initial score
            values = 
                line.Split(';') 
                    |> Array.toList
                    |> List.map2 (fun (name,parser) (stringValue) -> name, (parser stringValue)) desc              
                    |> Map.ofList
                    |> Map.add "Score" (Value.Float 0.0)
        }

    let parse (fileName : string) =
        let lines = File.ReadAllLines fileName
        if lines.Length >=2 then
            let description = getRowDescription lines.[0] lines.[1]
            let rows =
                [| for i in 2 .. lines.Length - 1 do                   
                    yield parseRow description lines.[i]
                |]
            
            let h = getHeader lines.[0] lines.[1]

            let computeStatistics (k : string) (v : Attribute) : Attribute =
                
                let p (row : Row) : float =
                    let m = Map.find k row.values
                    Value.toFloat m

                let values = Array.map p rows

                { v with 
                    stats = 
                        {
                            min = Array.min values
                            max = Array.max values
                        }
                }

            let visibleOrd = 
                "Score" :: (h
                    |> Map.filter (fun k _ -> k <> "Score")
                    |> Map.toList
                    |> List.map (fun (k, v) -> k))
            {
                header = Map.map computeStatistics h   
                rows = rows
                weights = 
                    h   
                    |> Map.filter ( fun k a -> a.kind = Bar Target.Max || a.kind = Bar Target.Min) 
                    |> Map.map (fun k a -> 1.0/(float visibleOrd.Length-2.0)) 

                visibleOrder = visibleOrd
                showOptions = true
                colors =
                    let seqH = h |> Map.toSeq |> Seq.zip [0..h.Count-1]
                    let colors = 
                        [| 
                            C4b(179,88,6,255)
                            C4b(127,59,8,255)
                            C4b(224,130,20,0)
                            C4b(253,184,99,0)
                            C4b(254,224,182,0)
                            C4b(247,247,247,0)
                            C4b(216,218,235,0)
                            C4b(178,171,210,0)
                            C4b(128,115,172,0)
                            C4b(84,39,136,255)
                            C4b(45,0,75,255)
                        |]
                    
                    seqH 
                        |> Seq.map (fun (i,h) -> ((fst h), colors.[i % colors.Length]))
                        |> Map.ofSeq
                dragedAttribute = None    
                weightingFunction = WeightingFunctions.absolutSplit2
                lastUpdateTime = 0.0
                threads = ThreadPool.empty
            }
        else 
            failwith "not enough lines"


module NewLineUp =
    open Aardvark.Base.Geometry

    let defaultModel () : Table =  
        Parsing.parse Parsing.example1

    //let defaultModelTest () =
       
    //    let headerList = 
    //        [
    //            //("model", { name = "Model"; stats = {min = 0.0; max = 0.0}; kind = Plain })
    //            //("year", { name = "Year"; stats = {min = 1980.0; max = 2020.0}; kind = Bar Max})
    //            //("date", { name = "Release date"; stats = {min = 0.0; max = 0.0}; kind = Plain })
    //            //("maxRes", { name = "Max resolution"; stats = {min = 0.0; max = 0.0}; kind = Bar Max })
    //            //("lowRes", { name = "Low resolution"; stats = {min = 0.0; max = 0.0}; kind = Bar Max })
    //            //("effectivePixel", { name = "Effective pixels"; stats = {min = 0.0; max = 0.0}; kind = Bar Max })
    //            //("zoomWide", { name = "Zoom wide (W)"; stats = {min = 0.0; max = 0.0}; kind = Bar Max})
    //            //("zoomTele", { name = "Zoom tele (T)"; stats = {min = 0.0; max = 0.0};kind = Bar Max})
    //            //("normalFocus", { name = "Normal focus range"; stats = {min = 0.0; max = 0.0}; kind = Bar Max})
    //            //("macroFocus", { name = "Macro focus range"; stats = {min = 0.0; max = 0.0}; kind = Bar Max})
    //            //("storage", { name = "Storage included"; stats = {min = 0.0; max = 0.0}; kind = Bar Max})
    //            //("weight", { name = "Weight (inc. batteries)"; stats = {min = 0.0; max = 0.0}; kind = Bar Min})
    //            //("dimension", { name = "Dimensions"; stats = {min = 0.0; max = 0.0}; kind = Bar Min})
    //            //("price", { name = "Price"; stats = {min = 0.0; max = 0.0}; kind = Bar Min })
    //        ]

    //    let data = 
    //        [|
    //            { values = Map.ofList [("model", String "Hugo"); ("score", Float 1000.0); ("year", Int 2010)] }
    //            { values = Map.ofList [("model", String "Blub"); ("score", Float 6516.0); ("year", Int 1980)] }
    //            { values = Map.ofList [("model", String "Blah"); ("score", Float 1.0); ("year", Int 2020)] }
    //        |]

    //    {
    //        header = Map.ofList headerList
    //        rows = data
    //        weights = Map.ofList [ ("model", None); ("score", None); ("year", Some 1.0) ]
    //        visibleOrder = [ "model"; "score"; "year"]
    //        scores = [| 0.0; 0.0; 0.0 |]
    //    }
        
        //let attribs = 
        //    [
        //        { name = "Score"; kind = Score}
        //        { name = "Model"; kind = Plain }
        //        { name = "Release date"; kind = Plain }
        //        { name = "Max resolution"; kind = Bar Max }
        //        { name = "Low resolution"; kind = Bar Max }
        //        { name = "Effective pixels"; kind = Bar Max }
        //        { name = "Zoom wide (W)"; kind = Bar Max}
        //        { name = "Zoom tele (T)"; kind = Bar Max}
        //        { name = "Normal focus range"; kind = Bar Max}
        //        { name = "Macro focus range"; kind = Bar Max}
        //        { name = "Storage included"; kind = Bar Max}
        //        { name = "Weight (inc. batteries)"; kind = Bar Min}
        //        { name = "Dimensions"; kind = Bar Min}
        //        { name = "Price"; kind = Bar Min }
                            
        //    ]
        //{
        //    input = Parsing.parse Parsing.example1
        //    config = 
        //        {
        //            attributes = 
        //                attribs
        //        }
        //    visibleAttributes =
        //        [
        //            { name = "Score"; kind = Score}
        //            { name = "Model"; kind = Plain }
        //            { name = "Release date"; kind = Plain }
        //            { name = "Max resolution"; kind = Bar Max }
        //            { name = "Low resolution"; kind = Bar Max }
        //        ]|> List.mapi (fun i a -> a.name, { name = a.name; sortKey = i; weight = Some 0.5 }) |> HMap.ofList
            
        //    weights = [{ name = "Price"; weight = 0.8}]
        //}