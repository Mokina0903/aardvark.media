
namespace LineUpModel


open System
open System.IO

type Value =
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

module Parsing =
    open System.Globalization

    let example1 = @"C:\Users\wissmann\Desktop\cameras.csv"

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
                        | "DOUBLE",s -> 
                            match Double.TryParse(s) with 
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

type Kind = PlainNumber | PlainString | Bar

type Attribute = 
    {
        name : string
        kind : Kind
    }

type Config =
    {
        attributes : list<Attribute>
    }

[<DomainType>]
type LineUp =
    {
        input : Table
        config : Config
    }
                    //th [clazz  "leftHeader"] [text "Model"]
                    //th [] [text "Release date"]
                    //th [] [text "Max resolution"]
                    //th [clazz  "rightHeader"] [text "Low resolution"]
module LineUp =
    let defaultModel =
        {
            input = Parsing.parse Parsing.example1
            config = 
                {
                    attributes = [
                        { name = "Model"; kind = PlainString }
                        { name = "Release date"; kind = PlainNumber }
                        { name = "Max resolution"; kind = PlainNumber } //bar
                        { name = "Low resolution"; kind = PlainNumber } //bar
                    ]
                }
        }
