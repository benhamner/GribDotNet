module GridDefinitionSection

type LambertConformalTemplate = {
    ShapeOfEarth: byte;
    ScaleFactorOfRadiusOfSphericalEarth: byte;
    ScaleValueOfRadiusOfSphericalEarth: uint32;
    ScaleFactorOfMajorAxisOfOblateSpheroidEarth: byte;
    ScaleValueOfMajorAxisOfOblateSpheroidEarth: uint32;
    ScaleFactorOfMinorAxisOfOblateSpheroidEarth: byte;
    ScaleValueOfMinorAxisOfOblateSpheroidEarth: uint32;
    NumberOfPointsOnXAxis: uint32;
    NumberOfPointsOnYAxis: uint32;
    LatitudeOfFirstGridPoint: uint32;
    LongitudeOfFirstGridPoint: uint32;
    ResolutionAndComponentFlags: byte;
    LatitudeWhereDxAndDyAreSpecified: uint32;
    LongitudeOfMeridianParallelToYAxisAlongWhichLatitudeIncreasesAsTheYCoordinateIncreases: uint32;
    XDirectionGridLength: uint32;
    YDirectionGridLength: uint32;
    ProjectionCenterFlag: byte;
    ScanningMode: byte;
    FirstLatitudeFromThePoleAtWhichTheSecantConeCutsTheSphere: uint32;
    SecondLatitudeFromThePoleAtWhichTheSecantConeCutsTheSphere: uint32;
    LatitudeOfSouthernPoleOfProjection: uint32;
    LongitudeOfSouthernPoleOfProjection: uint32;
}

type GridDefinitionTemplate =
    | LambertConformal of LambertConformalTemplate
    | Other of uint16 * byte[]

let asLambertComformalTemplate gridTemplate =
    match gridTemplate with
    | LambertConformal x -> x
    | _ -> failwith "Not Lambert Conformal Template!"

let readLambertConformalTemplate (reader:System.IO.BinaryReader) = {
    ShapeOfEarth = reader.ReadByte();
    ScaleFactorOfRadiusOfSphericalEarth = reader.ReadByte();
    ScaleValueOfRadiusOfSphericalEarth = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    ScaleFactorOfMajorAxisOfOblateSpheroidEarth = reader.ReadByte();
    ScaleValueOfMajorAxisOfOblateSpheroidEarth = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    ScaleFactorOfMinorAxisOfOblateSpheroidEarth = reader.ReadByte();
    ScaleValueOfMinorAxisOfOblateSpheroidEarth = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    NumberOfPointsOnXAxis = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    NumberOfPointsOnYAxis = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    LatitudeOfFirstGridPoint = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    LongitudeOfFirstGridPoint = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    ResolutionAndComponentFlags = reader.ReadByte();
    LatitudeWhereDxAndDyAreSpecified = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    LongitudeOfMeridianParallelToYAxisAlongWhichLatitudeIncreasesAsTheYCoordinateIncreases = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    XDirectionGridLength = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    YDirectionGridLength = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    ProjectionCenterFlag = reader.ReadByte();
    ScanningMode = reader.ReadByte();
    FirstLatitudeFromThePoleAtWhichTheSecantConeCutsTheSphere = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    SecondLatitudeFromThePoleAtWhichTheSecantConeCutsTheSphere = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    LatitudeOfSouthernPoleOfProjection = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    LongitudeOfSouthernPoleOfProjection = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
}

let readGridDefinitionTemplate (reader:System.IO.BinaryReader) templateNumber templateLength = 
    match templateNumber with
    | 30us ->
        let template = readLambertConformalTemplate reader
        LambertConformal template
    | _ -> Other (templateNumber, (reader.ReadBytes(templateLength)))

type GridDefinitionSection = {
    SectionLength: uint32;
    SourceOfGridDefinition: byte;
    NumberOfDataPoints: uint32;
    NumberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints: byte;
    InterpolationOfListOfNumbersDefiningNumberOfPoints: byte;
    GridDefinitionTemplate: GridDefinitionTemplate;
    OptionalListOfNumbersDefiningNumberOfPoints: byte[];
}

let readGridDefinitionSection (reader:System.IO.BinaryReader) sectionLength =
    let sourceOfGridDefinition = reader.ReadByte()
    let numberOfDataPoints = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    let numberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints = reader.ReadByte()
    let interpolationOfListOfNumbersDefiningNumberOfPoints = reader.ReadByte()
    
    let gridDefinitionTemplateNumber = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let templateLength = (int) sectionLength - 14 - (int) numberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints;
    let gridDefinitionTemplate = readGridDefinitionTemplate reader gridDefinitionTemplateNumber templateLength
    let optionalListOfNumbersDefiningNumberOfPoints = reader.ReadBytes((int) numberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints)

    {
        SectionLength = sectionLength;
        SourceOfGridDefinition = sourceOfGridDefinition;
        NumberOfDataPoints = numberOfDataPoints;
        NumberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints = numberOfOctetsForOptionalListOfNumbersDefiningNumberOfPoints;
        InterpolationOfListOfNumbersDefiningNumberOfPoints = interpolationOfListOfNumbersDefiningNumberOfPoints;
        GridDefinitionTemplate = gridDefinitionTemplate;
        OptionalListOfNumbersDefiningNumberOfPoints = optionalListOfNumbersDefiningNumberOfPoints;
    }
