module DataRepresentationSection

type DataRepresentationTemplate5_40 = {
    ReferenceValue: uint32
    BinaryScaleFactor: uint16
    DecimalScaleFactor: uint16
    NumberOfBitsRequiredToHoldTheResultingScaledAndReferencedDataValues: byte
    TypeOfOriginalFieldValues: byte
    TypeOfCompressionUsed: byte
    TargetCompressionRatio: byte
}

type DataRepresentationTemplate =
    | Template5_40 of DataRepresentationTemplate5_40
    | Other of uint16 * byte[]

let readDataRepresentationTemplate5_40 (reader:System.IO.BinaryReader) = {
    ReferenceValue = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    BinaryScaleFactor = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    DecimalScaleFactor = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    NumberOfBitsRequiredToHoldTheResultingScaledAndReferencedDataValues = reader.ReadByte()
    TypeOfOriginalFieldValues = reader.ReadByte()
    TypeOfCompressionUsed = reader.ReadByte()
    TargetCompressionRatio = reader.ReadByte()
}

let readDataRepresentationTemplate (reader:System.IO.BinaryReader) templateNumber templateLength = 
    match templateNumber with
    | 40us ->
        let template = readDataRepresentationTemplate5_40 reader
        Template5_40 template
    | _ -> Other (templateNumber, (reader.ReadBytes(templateLength)))

type DataRepresentationSection = {
    SectionLength: uint32;
    NumberOfDataPoints: uint32;
    DataRepresentationTemplateNumber: uint16;
    DataRepresentationTemplate: DataRepresentationTemplate
}

let readDataRepresentationSection (reader:System.IO.BinaryReader) sectionLength =
    let numberOfDataPoints = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    let dataRepresentationTemplateNumber = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let dataRepresentationTemplate = readDataRepresentationTemplate reader dataRepresentationTemplateNumber ((int) (sectionLength - 11u))

    {
        SectionLength = sectionLength;
        NumberOfDataPoints = numberOfDataPoints;
        DataRepresentationTemplateNumber = dataRepresentationTemplateNumber;
        DataRepresentationTemplate = dataRepresentationTemplate
    }
