module SectionTests

open GribDotNet

open Section

open NUnit.Framework
open FsUnit

let path = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let sections = Section.readAllSectionsFromPath path

[<TestFixture>]
type SectionTests() =
    [<Test>]
    member test.AllSectionTests() =
        sections.Length |> should be (greaterThan -1)