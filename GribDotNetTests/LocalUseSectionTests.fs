module LocalUseSectionTests

open LocalUseSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "000000060309"
let localUseSection = readLocalUseSection reader

[<TestFixture>]
type LocalUseSectionTests() =
    [<Test>]
    member test.SectionLength() =
        localUseSection.SectionLength |> should equal 6
        
    [<Test>]
    member test.SectionNumber() =
        localUseSection.SectionNumber |> should equal 3

    [<Test>]
    member test.LocalUse() = 
        localUseSection.LocalUse |> should equal [| 9y |]
