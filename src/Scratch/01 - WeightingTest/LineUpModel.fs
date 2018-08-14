
namespace LineUpModel


open System
open System.IO

type Message =
    | AddAttribute

type Value =
    | Int of int
    | Float of float
    | Name  of string
    | Missing

type RowDescription = list<string * (string -> Value)>
type Row            = Map<string,Value>
type Table = 
    {
        rowDescription : RowDescription
        rows           : array<Row>
    }

type Statistics =
    {
        min : float
        max : float
        avg : float
    }


module Statistics =
    let empty = { min = 0.0; max = 0.0; avg = 0.0 }


module Parsing =
    open System.Globalization
    open System.Numerics

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
                        | "STRING",s -> Name s
                        | "INTEGER",s -> 
                            match Int32.TryParse(s) with 
                                | (true,v) -> Int v
                                | _ -> failwithf "could not parse: %s (%s)" s n
                        | "DOUBLE",s -> 
                            match Double.TryParse(s.Replace(".",",")) with 
                                | (true,v) -> Float v
                                | _ -> failwithf "could not parse: %s (%s)" s n
                        | _ -> failwithf "no parser for: %s" s
                n, parserFunction
            ) pairs
        nameParser

    let parseRow (desc : RowDescription) (line : string) : Row =
        line.Split(';') 
        |> Array.toList
        |> List.map2 (fun (name,parser) (stringValue) -> name, (parser stringValue)) desc
        |> Map.ofList

    let parse (fileName : string) =
        let lines = File.ReadAllLines fileName
        if lines.Length >=2 then
            let description = getRowDescription lines.[0] lines.[1]
            let rows =
                [| for i in 2 .. lines.Length - 1 do
                    yield parseRow description lines.[i]
                |]
            {
                rowDescription = description
                rows = rows
            }
        else 
            failwith "not enough lines"


open Aardvark.Base.Incremental
open Aardvark.Base

type Target = Min | Max
type Kind = Plain | Bar of Target | Score

type Attribute = 
    {
        name : string
        kind : Kind
    }

type Config =
    {
        attributes : list<Attribute>
    }

type Weight =
    {
        name: string
        weight: float
    }

[<DomainType>]
type VisibleAttribute =
    {
        [<NonIncremental>]
        sortKey: int
        name: string
        weight: Option<float>
    }

[<DomainType>]
type LineUp =
    {
        input : Table
        config : Config
        visibleAttributes: hmap<string, VisibleAttribute>
        weights: list<Weight>
    }
                   
module LineUp =
    let defaultModel () =
        let attribs = 
            [
                { name = "Score"; kind = Score}
                { name = "Model"; kind = Plain }
                { name = "Release date"; kind = Plain }
                { name = "Max resolution"; kind = Bar Max }
                { name = "Low resolution"; kind = Bar Max }
                { name = "Effective pixels"; kind = Bar Max }
                { name = "Zoom wide (W)"; kind = Bar Max}
                { name = "Zoom tele (T)"; kind = Bar Max}
                { name = "Normal focus range"; kind = Bar Max}
                { name = "Macro focus range"; kind = Bar Max}
                { name = "Storage included"; kind = Bar Max}
                { name = "Weight (inc. batteries)"; kind = Bar Min}
                { name = "Dimensions"; kind = Bar Min}
                { name = "Price"; kind = Bar Min }
                            
            ]
        {
            input = Parsing.parse Parsing.example1
            config = 
                {
                    attributes = 
                        attribs
                }
            visibleAttributes =
                [
                    { name = "Score"; kind = Score}
                    { name = "Model"; kind = Plain }
                    { name = "Release date"; kind = Plain }
                    { name = "Max resolution"; kind = Bar Max }
                    { name = "Low resolution"; kind = Bar Max }
                ]|> List.mapi (fun i a -> a.name, { name = a.name; sortKey = i; weight = Some 0.5 }) |> HMap.ofList
            
            weights = [{ name = "Price"; weight = 0.8}]
        }