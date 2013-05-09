module GridDefinitionSection

type GridDefinitionTemplateType =
   | LambertConformal
   | Other of uint16

let gridDefinitionTemplateTypeFromShort (x:uint16) =
    match x with
    | 0us -> LambertConformal
    | x -> Other x

type GridDefinitionSection = {
    SectionLength: uint32;
    SectionNumber: byte;
    SourceOfGridDefinition: byte;
    NumberOfDataPoints: uint32;
    NumberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints: byte;
    InterpolationOfListOfNumbersDefiningNumberOfPoints: byte;
    GridDefinitionTemplateType: GridDefinitionTemplateType;
    GridDefinitionTemplatePlusList: byte[];
}

let readGridDefinitionSection (reader:System.IO.BinaryReader) =
    let sectionLength = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    let sectionNumber = reader.ReadByte()
    let sourceOfGridDefinition = reader.ReadByte()
    let numberOfDataPoints = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    let numberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints = reader.ReadByte()
    let interpolationOfListOfNumbersDefiningNumberOfPoints = reader.ReadByte()
    let gridDefinitionTemplateNumber = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let gridDefinitionTemplateType = gridDefinitionTemplateTypeFromShort gridDefinitionTemplateNumber
    let gridDefinitionTemplate = reader.ReadBytes((int) (sectionLength - 14u))

    {
        SectionLength = sectionLength;
        SectionNumber = sectionNumber;
        SourceOfGridDefinition = sourceOfGridDefinition;
        NumberOfDataPoints = numberOfDataPoints;
        NumberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints = numberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints;
        InterpolationOfListOfNumbersDefiningNumberOfPoints = interpolationOfListOfNumbersDefiningNumberOfPoints;
        GridDefinitionTemplateType = gridDefinitionTemplateType;
        GridDefinitionTemplatePlusList = gridDefinitionTemplate;
    }
