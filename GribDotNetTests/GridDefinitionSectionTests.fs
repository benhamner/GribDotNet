module GridDefinitionSectionTests

open GridDefinitionSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "080000000900000001020304"
let gridDefinitionSection = readGridDefinitionSection reader 17u

[<TestFixture>]
type GridDefinitionSectionTests() =
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
    member test.GridDefinitionTemplate() =
        gridDefinitionSection.GridDefinitionTemplate |> should equal (Other (1us, [|02uy; 03uy; 04uy|]))