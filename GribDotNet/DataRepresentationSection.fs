module DataRepresentationSection

type DataRepresentationSection = {
    SectionLength: uint32;
    NumberOfDataPoints: uint32;
    DataRepresentationTemplateNumber: uint16;
    DataRepresentationTemplate: byte[]
}

let readDataRepresentationSection (reader:System.IO.BinaryReader) sectionLength =
    let numberOfDataPoints = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    let dataRepresentationTemplateNumber = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let dataRepresentationTemplate = reader.ReadBytes((int) (sectionLength - 11u))

    {
        SectionLength = sectionLength;
        NumberOfDataPoints = numberOfDataPoints;
        DataRepresentationTemplateNumber = dataRepresentationTemplateNumber;
        DataRepresentationTemplate = dataRepresentationTemplate
    }
