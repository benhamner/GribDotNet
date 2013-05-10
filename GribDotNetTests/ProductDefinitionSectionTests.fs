module ProductDefinitionSectionTests

open ProductDefinitionSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "000100000a0a0a0f"
let productDefinitionSection = readProductDefinitionSection reader 13u

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
        productDefinitionSection.ProductDefinitionTemplateNumber |> should equal 0us

    [<Test>]
    member test.ProductDefinitionTemplate() = 
        productDefinitionSection.ProductDefinitionTemplate |> should equal [| 10y ; 10y ; 10y |]
        
    [<Test>]
    member test.ListOfCoordinateValues() = 
        productDefinitionSection.ListOfCoordinateValues |> should equal [| 15y |]
