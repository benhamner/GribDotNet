module GribDotNet.BitMapSection

type BitMapSection = {
    SectionLength: uint32;
    BitMapIndicator: byte;
    BitMap: byte[];
}

let readBitMapSection (reader:System.IO.BinaryReader) sectionLength =
    let bitMapIndicator  = reader.ReadByte()
    let bitMap = 
        match bitMapIndicator with
        | 0uy -> reader.ReadBytes((int) (sectionLength - 6u))
        | _ -> [||]

    {
        SectionLength = sectionLength;
        BitMapIndicator = bitMapIndicator;
        BitMap = bitMap
    }
