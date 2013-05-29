module ProductDefinitionSection

open IndicatorSection

type FixedSurfaceType =
    | GroundOrWaterSurface
    | CloudBaseLevel
    | LevelOfCloudTops
    | LevelOfZeroDegreeCelsiusIsotherm
    | LevelOfAdiabaticCodensationLiftedFromTheSurface
    | MaximumWindLevel
    | Tropopause
    | IsobaricSurface
    | MeanSeaLevel
    | SpecificAltitudeAboveMeanSeaLevel
    | SpecificHeightLevelAboveGround
    | LevelAtSpecifiedPressureDifferenceFromGroundToLevel
    | EntireAtmosphere
    | HighestTroposphericFreezingLevel
    | ConvectiveCloudTopLevel
    | LowestLevelOfTheWetBulbZero
    | EquilibriumLevel
    | HybridLevel
    | Other of byte

let toFixedSurfaceType byte =
    match byte with
    | 1uy   -> GroundOrWaterSurface
    | 2uy   -> CloudBaseLevel
    | 3uy   -> LevelOfCloudTops
    | 4uy   -> LevelOfZeroDegreeCelsiusIsotherm
    | 5uy   -> LevelOfAdiabaticCodensationLiftedFromTheSurface
    | 6uy   -> MaximumWindLevel
    | 7uy   -> Tropopause
    | 100uy -> IsobaricSurface
    | 101uy -> MeanSeaLevel
    | 102uy -> SpecificAltitudeAboveMeanSeaLevel
    | 103uy -> SpecificHeightLevelAboveGround
    | 105uy -> HybridLevel
    | 108uy -> LevelAtSpecifiedPressureDifferenceFromGroundToLevel
    | 200uy -> EntireAtmosphere
    | 204uy -> HighestTroposphericFreezingLevel
    | 243uy -> ConvectiveCloudTopLevel
    | 245uy -> LowestLevelOfTheWetBulbZero
    | 247uy -> EquilibriumLevel
    | _     -> Other byte

type ProductCategory = 
    | Temperature
    | Moisture
    | Momentum
    | Mass
    | ThermodynamicStabilityIndicies
    | ForecastRadarImagery
    | PhysicalAtmposhpericProperties
    | Other of byte

let toProductCategory discipline byte =
    match discipline with
    | Meteorological ->
        match byte with
        | 0uy -> Temperature
        | 1uy -> Moisture
        | 2uy -> Momentum
        | 3uy -> Mass
        | 7uy -> ThermodynamicStabilityIndicies
        | 16uy -> ForecastRadarImagery
        | 19uy -> PhysicalAtmposhpericProperties
        | _ -> Other byte
    | _ -> Other byte

type Product = 
    | Temperature
    | PotentialTemperature
    | SpecificHumidity
    | RelativeHumidity
    | WindDirection
    | WindSpeed
    | UComponentOfWind
    | VComponentOfWind
    | VerticalVelocityPressure
    | Pressure
    | GeopotentialHeight
    | Other of byte

let toProduct : Discipline -> ProductCategory -> byte -> Product = fun discipline category parameterNumber ->
    match discipline with 
    | Meteorological ->
        match category with
        | ProductCategory.Temperature ->
            match parameterNumber with
            | 0uy -> Product.Temperature
            | 2uy -> PotentialTemperature
            | _ -> Other parameterNumber
        | ProductCategory.Moisture ->
            match parameterNumber with
            | 0uy  -> SpecificHumidity
            | 1uy -> RelativeHumidity
            | _ -> Other parameterNumber
        | Momentum -> 
            match parameterNumber with
            | 0uy -> WindDirection
            | 1uy -> WindSpeed
            | 2uy -> UComponentOfWind
            | 3uy -> VComponentOfWind
            | 8uy -> VerticalVelocityPressure
            | _ -> Other parameterNumber
        | Mass -> 
            match parameterNumber with
            | 0uy -> Pressure
            | 5uy -> GeopotentialHeight
            | _ -> Other parameterNumber
        | _ -> Other parameterNumber
    | _ -> Other parameterNumber

type ProductDefinitionTemplateType0 = {
    ParameterCategory: ProductCategory;
    ParameterNumber: Product;
    TypeOfGeneratingProcess: byte;
    BackgroundGeneratingProcessIdentifier: byte;
    AnalaysisOrForecastGeneratingProcessIdentified: byte;
    HoursOfObservationalDataCutoffAfterReferenceTime: uint16;
    MinutesOfObservationalDataCutoffAfterRefenceTime: byte;
    IndicatorOfUnitOfTimeRange: byte;
    ForecastTime: uint32;
    TypeOfFirstFixedSurface: FixedSurfaceType;
    ScaleFactorOfFirstFixedSurface: byte;
    ScaledValueOfFirstFixedSurvace: uint32;
    TypeOfSecondFixedSurface: FixedSurfaceType;
    ScaleFactorOfSecondFixedSurface: byte;
    ScaledValueOfSecondFixedSurvace: uint32;
}

type ProductDefinitionTemplate =
    | Type0 of ProductDefinitionTemplateType0
    | Other of byte[]

type ProductDefinitionTemplate with
    member this.IsTypeZero =
        match this with
        | Type0 _ -> true
        | _ -> false
    member this.GetTypeZero() = 
        match this with
        | Type0 t -> Some(t)
        | _ -> None

let readProductDefinitionTemplateType0 (reader:System.IO.BinaryReader) discipline =
    let parameterCategory = toProductCategory discipline (reader.ReadByte())
    let parameterNumber = toProduct discipline parameterCategory (reader.ReadByte())
    {
        ParameterCategory = parameterCategory;
        ParameterNumber = parameterNumber;
        TypeOfGeneratingProcess = reader.ReadByte();
        BackgroundGeneratingProcessIdentifier = reader.ReadByte();
        AnalaysisOrForecastGeneratingProcessIdentified = reader.ReadByte();
        HoursOfObservationalDataCutoffAfterReferenceTime = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0);
        MinutesOfObservationalDataCutoffAfterRefenceTime = reader.ReadByte();
        IndicatorOfUnitOfTimeRange = reader.ReadByte();
        ForecastTime = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
        TypeOfFirstFixedSurface = toFixedSurfaceType (reader.ReadByte());
        ScaleFactorOfFirstFixedSurface = reader.ReadByte();
        ScaledValueOfFirstFixedSurvace = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
        TypeOfSecondFixedSurface = toFixedSurfaceType (reader.ReadByte());
        ScaleFactorOfSecondFixedSurface = reader.ReadByte();
        ScaledValueOfSecondFixedSurvace = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
    }

type ProductDefinitionSection = {
    SectionLength: uint32;
    NumberOfCoordinateValuesAfterTemplate: uint16;
    ProductDefinitionTemplateNumber: uint16;
    ProductDefinitionTemplate: ProductDefinitionTemplate;
    ListOfCoordinateValues: byte[]; 
}

let readProductDefinitionTemplate (reader:System.IO.BinaryReader) templateNumber templateLength discipline = 
    match templateNumber with
    | 0us ->
        let template = readProductDefinitionTemplateType0 reader discipline
        Type0 template
    | _ -> Other (reader.ReadBytes(templateLength))

let readProductDefinitionSection (reader:System.IO.BinaryReader) sectionLength discipline =
    let numberOfCoordinateValuesAfterTemplate = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let productDefinitionTemplateNumber = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let productDefinitionTemplateLength = (int) (sectionLength - 9u - (uint32) numberOfCoordinateValuesAfterTemplate)

    let productDefinitionTemplate = readProductDefinitionTemplate reader productDefinitionTemplateNumber productDefinitionTemplateLength discipline

    let listOfCoordinateValues = reader.ReadBytes((int) numberOfCoordinateValuesAfterTemplate)

    {
        SectionLength = sectionLength;
        NumberOfCoordinateValuesAfterTemplate = numberOfCoordinateValuesAfterTemplate;
        ProductDefinitionTemplateNumber = productDefinitionTemplateNumber;
        ProductDefinitionTemplate = productDefinitionTemplate;
        ListOfCoordinateValues = listOfCoordinateValues
    }
