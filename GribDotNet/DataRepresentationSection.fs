module DataRepresentationSection

type DataRepresentationTemplate5_40 = {
    ReferenceValue: float32
    BinaryScaleFactor: int16
    DecimalScaleFactor: int16
    NumberOfBitsRequiredToHoldTheResultingScaledAndReferencedDataValues: byte
    TypeOfOriginalFieldValues: byte
    TypeOfCompressionUsed: byte
    TargetCompressionRatio: byte
}

type DataRepresentationTemplate =
    | Template5_40 of DataRepresentationTemplate5_40
    | Other of uint16 * byte[]

let asTemplate5_40 dataRepresentationTemplate =
    match dataRepresentationTemplate with
    | Template5_40 x -> x
    | _ -> failwith "Not a Data Representation Template 5.40!"

let bytesToInt16 bigEndianBytes =
    let littleEndianBytes = Array.rev bigEndianBytes
    let lastIndex = Array.length littleEndianBytes - 1
    let mostSignificantByte = littleEndianBytes.[lastIndex]
    let isNegative = mostSignificantByte >= 128uy
    if isNegative then
        littleEndianBytes.[lastIndex] <- mostSignificantByte - 128uy
    let result = System.BitConverter.ToInt16(littleEndianBytes, 0) * if isNegative then -1s else 1s
    result

let readDataRepresentationTemplate5_40 (reader:System.IO.BinaryReader) =
    {
        ReferenceValue = System.BitConverter.ToSingle(reader.ReadBytes(4) |> Array.rev,0)
        BinaryScaleFactor = bytesToInt16 (reader.ReadBytes(2))
        DecimalScaleFactor = bytesToInt16 (reader.ReadBytes(2))
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
    let numberOfDataPoints = reader.ReadUInt32()
    let dataRepresentationTemplateNumber = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0) // reader.ReadUInt16() - this entry appears big endian 0x2800
    let dataRepresentationTemplate = readDataRepresentationTemplate reader dataRepresentationTemplateNumber ((int) (sectionLength - 11u))

    {
        SectionLength = sectionLength;
        NumberOfDataPoints = numberOfDataPoints;
        DataRepresentationTemplateNumber = dataRepresentationTemplateNumber;
        DataRepresentationTemplate = dataRepresentationTemplate
    }
