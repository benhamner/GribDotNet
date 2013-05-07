module GridDefinitionSectionTests

open GridDefinitionSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "0000001105080000000900000001020304"
let gridDefinitionSection = readGridDefinitionSection reader

[<TestFixture>]
type GridDefinitionSectionTests() =
    [<Test>]
    member test.SectionLength() =
        gridDefinitionSection.SectionLength |> should equal 17
        
    [<Test>]
    member test.SectionNumber() =
        gridDefinitionSection.SectionNumber |> should equal 5
        
    [<Test>]
    member test.SourceOfGridDefinition() =
        gridDefinitionSection.SourceOfGridDefinition |> should equal 0x08

    [<Test>]
    member test.NumberOfDatapoints() =
        gridDefinitionSection.NumberOfDataPoints |> should equal 9
        
    [<Test>]
    member test.NumberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints() =
        gridDefinitionSection.NumberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints |> should equal 0us

    [<Test>]
    member test.InterpolationOfListOfNumbersDefiningNumberOfPoints() =
        gridDefinitionSection.InterpolationOfListOfNumbersDefiningNumberOfPoints |> should equal 0us

    [<Test>]
    member test.GridDefinitionTemplateNumber() =
        gridDefinitionSection.GridDefinitionTemplateNumber |> should equal 1us
        
    [<Test>]
    member test.GridDefinitionTemplatePlusList() =
        gridDefinitionSection.GridDefinitionTemplatePlusList |> should equal [|0x02; 0x03; 0x04|]
