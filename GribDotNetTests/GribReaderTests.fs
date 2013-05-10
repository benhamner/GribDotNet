module GribReaderTests

open GribReader

open GridDefinitionSection

open NUnit.Framework
open FsUnit

let path = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let grib = readIndividualGribFromPath path
let gribs = readGribsFromPath path

[<TestFixture>]
type GribReaderTests() =
    [<Test>]
    member test.IdentificationSection() =
        grib.IdentificationSection.SectionLength |> should equal 21u

    [<Test>]
    member test.GridDefinitionTemplateType() =
        grib.GridDefinitionSection.GridDefinitionTemplateType |> should equal LambertConformal

    [<Test>]
    member test.GribsTest() =
        gribs.Length |> should be (greaterThan -1)