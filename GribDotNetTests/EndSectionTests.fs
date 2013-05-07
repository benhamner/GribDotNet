module EndSectionTests

open EndSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "37373737"
let endSection = readEndSection reader

[<TestFixture>]
type EndSectionTests() =
    [<Test>]
    member test.EndText() =
        endSection.EndText |> should equal "7777"B
        