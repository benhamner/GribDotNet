module DataSection

type DataSection = {
    SectionLength: uint32;
    SectionNumber: byte;
    Data: byte[];
}

let readDataSection (reader:System.IO.BinaryReader) =
    let sectionLength = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    let sectionNumber = reader.ReadByte()
    let data = reader.ReadBytes((int) (sectionLength - 5u))
    
    {
        SectionLength = sectionLength;
        SectionNumber = sectionNumber;
        Data = data
    }
