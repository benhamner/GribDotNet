module BitMapSectionTests

open GribDotNet

open BitMapSection

open FromHex

open NUnit.Framework
open FsUnit

[<TestFixture>]
type BitMapSectionTests() =

    [<Test>]
    member test.BitMapIndicatorNonzero() = 
        let reader = streamFromHex "01"
        let bitMapSection = readBitMapSection reader 6u
        bitMapSection.BitMapIndicator |> should equal 0x01

    [<Test>]
    member test.BitMapIndicatorZeroAndNoBitMap() = 
        let reader = streamFromHex "00"
        let bitMapSection = readBitMapSection reader 6u
        bitMapSection.BitMap |> should equal (Array.create(0) 0uy)

    [<Test>]
    member test.BitMapPresent() = 
        let reader = streamFromHex "00010203"
        let bitMapSection = readBitMapSection reader 9u
        bitMapSection.BitMap |> should equal [|0x01; 0x02; 0x03|]
