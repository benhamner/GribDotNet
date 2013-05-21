module DataRepresentationSectionTests

open DataRepresentationSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "0000000900010f0f"
let dataRepresentationSection = readDataRepresentationSection reader 13u

[<TestFixture>]
type DataRepresentationSectionTests() =
    [<Test>]
    member test.SectionLength() =
        dataRepresentationSection.SectionLength |> should equal 13
        
    [<Test>]
    member test.NumberOfDatapoints() =
        dataRepresentationSection.NumberOfDataPoints |> should equal 9
        
    [<Test>]
    member test.DataRepresentationTemplateNumber() =
        dataRepresentationSection.DataRepresentationTemplateNumber |> should equal 1us

    [<Test>]
    member test.DataRepresentationTemplate() = 
        dataRepresentationSection.DataRepresentationTemplate |> should equal (Other (1us, [| 15uy ; 15uy |]))
