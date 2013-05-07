﻿module DataSectionTests

open DataSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "0000000808090f0f"
let dataSection = readDataSection reader

[<TestFixture>]
type DataSectionTests() =
    [<Test>]
    member test.SectionLength() =
        dataSection.SectionLength |> should equal 8
        
    [<Test>]
    member test.SectionNumber() =
        dataSection.SectionNumber |> should equal 8

    [<Test>]
    member test.Data() = 
        dataSection.Data |> should equal [| 9y ; 15y ; 15y |]