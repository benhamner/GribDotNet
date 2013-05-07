module BitMapSectionTests

open BitMapSection

open FromHex

open NUnit.Framework
open FsUnit

let reader = streamFromHex "000000090701"
let bitMapSection = readBitMapSection reader

[<TestFixture>]
type BitMapSectionTests() =
    [<Test>]
    member test.SectionLength() =
        bitMapSection.SectionLength |> should equal 9
        
    [<Test>]
    member test.SectionNumber() =
        bitMapSection.SectionNumber |> should equal 7

    [<Test>]
    member test.BitMapIndicator() = 
        bitMapSection.BitMapIndicator |> should equal 0x01

    [<Test>]
    member test.BitMap() = 
        bitMapSection.BitMap |> should equal (Array.create(0) 0uy)
