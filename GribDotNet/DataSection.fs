module DataSection

type DataSection = {
    SectionLength: uint32;
    Data: byte[];
}

let readDataSection (reader:System.IO.BinaryReader) sectionLength =
    let data = reader.ReadBytes((int) (sectionLength - 5u))
    
    {
        SectionLength = sectionLength;
        Data = data
    }
