module GribReaderTests

open GribReader

open GridDefinitionSection

open NUnit.Framework
open FsUnit

let path = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let grib = readGribFromPath path

[<TestFixture>]
type GribReaderTests() =
    [<Test>]
    member test.IndicatorSection() =
        grib.IndicatorSection.InitialText |> should equal "GRIB"B

    [<Test>]
    member test.IdentificationSection() =
        grib.IdentificationSection.SectionLength |> should equal 21u

    [<Test>]
    member test.GridDefinitionTemplateType() =
        grib.GridDefinitionSection.GridDefinitionTemplateType |> should equal LambertConformal
