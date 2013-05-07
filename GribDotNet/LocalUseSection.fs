module LocalUseSection

type LocalUseSection = {
    SectionLength: uint32;
    SectionNumber: byte;
    LocalUse: byte[];
}

let readLocalUseSection (reader:System.IO.BinaryReader) =
    let sectionLength = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    let sectionNumber = reader.ReadByte()
    let localUse = reader.ReadBytes((int) (sectionLength - 5u))
    
    {
        SectionLength = sectionLength;
        SectionNumber = sectionNumber;
        LocalUse = localUse
    }

let blankLocalUseSection = 
    {
        SectionLength = 0u;
        SectionNumber = 0uy;
        LocalUse = (Array.zeroCreate(0))
    }