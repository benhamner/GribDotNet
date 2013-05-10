module BitMapSection

type BitMapSection = {
    SectionLength: uint32;
    SectionNumber: byte;
    BitMapIndicator: byte;
    BitMap: byte[];
}

let readBitMapSection (reader:System.IO.BinaryReader) =
    let sectionLength = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    let sectionNumber = reader.ReadByte()
    System.Diagnostics.Debug.WriteLine(sprintf "Bit Map Section Number %d" sectionNumber)
    let bitMapIndicator  = reader.ReadByte()
    let bitMap = if bitMapIndicator=0uy
                 then reader.ReadBytes((int) (sectionLength - 5u))
                 else Array.zeroCreate(0)

    {
        SectionLength = sectionLength;
        SectionNumber = sectionNumber;
        BitMapIndicator = bitMapIndicator;
        BitMap = bitMap
    }
