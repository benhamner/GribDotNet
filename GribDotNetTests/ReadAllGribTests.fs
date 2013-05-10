module ReadAllGribTests

open GribReader

open NUnit.Framework
open FsUnit

let path = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let reader = (new System.IO.BinaryReader(System.IO.File.OpenRead(path)))
let allGribs = readAllGribs reader
let length = allGribs.Length

[<TestFixture>]
type GribReaderTests() =
    [<Test>]
    member test.IndicatorSection() =
        allGribs.Length |> should be (greaterThan -1)