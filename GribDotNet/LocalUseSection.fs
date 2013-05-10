module LocalUseSection

type LocalUseSection = {
    SectionLength: uint32;
    LocalUse: byte[];
}

let readLocalUseSection (reader:System.IO.BinaryReader) sectionLength =
    let localUse = reader.ReadBytes((int) (sectionLength - 5u))
    
    {
        SectionLength = sectionLength;
        LocalUse = localUse
    }