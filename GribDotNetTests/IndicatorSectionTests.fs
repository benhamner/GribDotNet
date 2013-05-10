module IndicatorSectionTests

open IndicatorSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "0000000200000000000088f8"
let indicatorSection = readIndicatorSection reader

[<TestFixture>]
type IndicatorSectionTests() =
    [<Test>]
    member test.EditionNumber() =
        indicatorSection.EditionNumber |> should equal 2

    [<Test>]
    member test.Discipline() =
        indicatorSection.Discipline |> should equal Discipline.Meteorological

    [<Test>]
    member test.TotalMessageLength() =
        indicatorSection.TotalMessageLength |> should equal 35064

