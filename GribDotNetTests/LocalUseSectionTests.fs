module LocalUseSectionTests

open LocalUseSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "09"
let localUseSection = readLocalUseSection reader 6u

[<TestFixture>]
type LocalUseSectionTests() =
    [<Test>]
    member test.LocalUse() = 
        localUseSection.LocalUse |> should equal [| 9y |]
