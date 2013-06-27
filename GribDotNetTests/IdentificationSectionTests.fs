module IdentificationSectionTests

open GribDotNet

open IdentificationSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "0007000002010107dd03111700000001"
let identificationSection = readIdentificationSection reader 21u

[<TestFixture>]
type IdentificationSectionTests() =
    [<Test>]
    member test.SignificanceOfReferenceTime() =
        identificationSection.SignficanceOfReferenceTime |> should equal StartOfForecast

    [<Test>]
    member test.ReferenceTime() =
        identificationSection.ReferenceTime |> should equal (new System.DateTime(year=2013, month=3, day=18, hour=23, minute=0, second=0))
