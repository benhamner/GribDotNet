module ProductDefinitionSectionTests

open GribDotNet

open IndicatorSection
open ProductDefinitionSection

open GribDotNet

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "000100020a0a0a0f"
let productDefinitionSection = readProductDefinitionSection reader 13u Discipline.Unknown

[<TestFixture>]
type ProductDefinitionSectionTests() =
    [<Test>]
    member test.SectionLength() =
        productDefinitionSection.SectionLength |> should equal 13

    [<Test>]
    member test.NumberOfCoordinateValuesAfterTemplate() =
        productDefinitionSection.NumberOfCoordinateValuesAfterTemplate |> should equal 1us
        
    [<Test>]
    member test.ProductDefinitionTemplateNumber() =
        productDefinitionSection.ProductDefinitionTemplateNumber |> should equal 2us

    [<Test>]
    member test.ProductDefinitionTemplate() = 
        productDefinitionSection.ProductDefinitionTemplate |> should equal (Other [| 10uy ; 10uy ; 10uy |])
        
    [<Test>]
    member test.ListOfCoordinateValues() = 
        productDefinitionSection.ListOfCoordinateValues |> should equal [| 15y |]

