module GribReaderTests

open GribReader

open NUnit.Framework
open FsUnit

let path = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let grib = readGribFromPath path

[<TestFixture>]
type GribReaderTests() =
    [<Test>]
    member test.InitialText() =
        grib.IndicatorSection.InitialText |> should equal "GRIB"B
