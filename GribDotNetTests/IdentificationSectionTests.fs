module IdentificationSectionTests

open IdentificationSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "00000015010007000002010107dd03111700000001"
let identificationSection = readIdentificationSection reader

[<TestFixture>]
type IdentificationSectionTests() =
    [<Test>]
    member test.SectionLength() =
        identificationSection.SectionLength |> should equal 21

    [<Test>]
    member test.SignificanceOfReferenceTime() =
        identificationSection.SignficanceOfReferenceTime |> should equal StartOfForecast

    [<Test>]
    member test.ReferenceTime() =
        identificationSection.ReferenceTime |> should equal (new System.DateTime(year=2013, month=3, day=18, hour=23, minute=0, second=0))
