namespace NewLineUpModel

type Target = Min | Max
type Kind = Plain | Bar of Target

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
    }

type RowDescription = list<string * (string -> Value)>

type Row =
    {
        values : Map<string, Value>
    }


 open Aardvark.Base.Incremental
[<DomainType>]
type Table =
    {
        header : Map<string, Attribute>
        weights : Map<string, Option<float>>
        rows : array<Row>
        visibleOrder : List<string>
        scores : array<float>
    }

module Parsing =
    open System.Globalization
    open System.Numerics
    open System.IO

    let example1 = @"C:\Users\wissmann\Desktop\cameras_reduced.csv"

    let getRowDescription (nameLine : string) (typeLine: string) : RowDescription  =
        let names = nameLine.Split(';') |> Array.toList
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
    
    let getStatistics (name : string) : Statistics  =
        
        failwith ""

    let getHeader (nameLine : string) (typeLine: string) : Map<string, Attribute> =
        let names = nameLine.Split(';') |> Array.toList
        let types = typeLine.Split(';') |> Array.toList
        let pairs = List.zip names types

        pairs |> List.mapi (fun i (n, t) -> 
            let k = 
                match t with
                    | "STRING" -> Plain
                    | "INTEGER" -> Bar Max
                    | "DOUBLE" -> Bar Max
                    | _ -> Plain
            let atr = 
                {
                    name  = n
                    kind = k
                    stats  = {min = 0.0; max = 0.0}
                }
            (n, atr)
        ) |> Map.ofList

    let parseRow (desc : RowDescription) (line : string) : Row =  
        {
            values = 
                line.Split(';') 
                    |> Array.toList
                    |> List.map2 (fun (name,parser) (stringValue) -> name, (parser stringValue)) desc
                    |> Map.ofList
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

            let newHeader = Map.map computeStatistics h

            {
                header = newHeader
                rows = rows
                weights = h |> Map.map (fun key _ -> Some 1.0)
                visibleOrder = h |> Map.toList |> List.map (fun (k, v) -> k)
                scores = rows |> Array.map (fun x -> 0.0)
            }
        else 
            failwith "not enough lines"


module NewLineUp =
    open Aardvark.Base.Geometry

    let defaultModel () : Table =
        Parsing.parse Parsing.example1

    let defaultModelTest () =
       
        let headerList = 
            [
                ("model", { name = "Model"; stats = {min = 0.0; max = 0.0}; kind = Plain })
                ("year", { name = "Year"; stats = {min = 1980.0; max = 2020.0}; kind = Bar Max})
                //("date", { name = "Release date"; stats = {min = 0.0; max = 0.0}; kind = Plain })
                //("maxRes", { name = "Max resolution"; stats = {min = 0.0; max = 0.0}; kind = Bar Max })
                //("lowRes", { name = "Low resolution"; stats = {min = 0.0; max = 0.0}; kind = Bar Max })
                //("effectivePixel", { name = "Effective pixels"; stats = {min = 0.0; max = 0.0}; kind = Bar Max })
                //("zoomWide", { name = "Zoom wide (W)"; stats = {min = 0.0; max = 0.0}; kind = Bar Max})
                //("zoomTele", { name = "Zoom tele (T)"; stats = {min = 0.0; max = 0.0};kind = Bar Max})
                //("normalFocus", { name = "Normal focus range"; stats = {min = 0.0; max = 0.0}; kind = Bar Max})
                //("macroFocus", { name = "Macro focus range"; stats = {min = 0.0; max = 0.0}; kind = Bar Max})
                //("storage", { name = "Storage included"; stats = {min = 0.0; max = 0.0}; kind = Bar Max})
                //("weight", { name = "Weight (inc. batteries)"; stats = {min = 0.0; max = 0.0}; kind = Bar Min})
                //("dimension", { name = "Dimensions"; stats = {min = 0.0; max = 0.0}; kind = Bar Min})
                //("price", { name = "Price"; stats = {min = 0.0; max = 0.0}; kind = Bar Min })
            ]

        let data = 
            [|
                { values = Map.ofList [("model", String "Hugo"); ("score", Float 1000.0); ("year", Int 2010)] }
                { values = Map.ofList [("model", String "Blub"); ("score", Float 6516.0); ("year", Int 1980)] }
                { values = Map.ofList [("model", String "Blah"); ("score", Float 1.0); ("year", Int 2020)] }
            |]

        {
            header = Map.ofList headerList
            rows = data
            weights = Map.ofList [ ("model", None); ("score", None); ("year", Some 1.0) ]
            visibleOrder = [ "model"; "score"; "year"]
            scores = [| 0.0; 0.0; 0.0 |]
        }
        
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