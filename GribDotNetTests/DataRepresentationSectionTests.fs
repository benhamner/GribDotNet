module DataRepresentationSectionTests

open DataRepresentationSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "0000000d050000000900010f0f"
let dataRepresentationSection = readDataRepresentationSection reader

[<TestFixture>]
type DataRepresentationSectionTests() =
    [<Test>]
    member test.SectionLength() =
        dataRepresentationSection.SectionLength |> should equal 13
        
    [<Test>]
    member test.SectionNumber() =
        dataRepresentationSection.SectionNumber |> should equal 5
        
    [<Test>]
    member test.NumberOfDatapoints() =
        dataRepresentationSection.NumberOfDataPoints |> should equal 9
        
    [<Test>]
    member test.DataRepresentationTemplateNumber() =
        dataRepresentationSection.DataRepresentationTemplateNumber |> should equal 1us

    [<Test>]
    member test.DataRepresentationTemplate() = 
        dataRepresentationSection.DataRepresentationTemplate |> should equal [| 15y ; 15y |]
