module ProductDefinitionSection

open IndicatorSection

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
    | RelativeHumidity
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
            | 1uy -> RelativeHumidity
            | _ -> Other parameterNumber
        | Momentum -> 
            match parameterNumber with
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
    TypeOfFirstFixedSurface: byte;
    ScaleFactorOfFirstFixedSurface: byte;
    ScaledValueOfFirstFixedSurvace: uint32;
    TypeOfSecondFixedSurface: byte;
    ScaleFactorOfSecondFixedSurface: byte;
    ScaledValueOfSecondFixedSurvace: uint32;
}

type ProductDefinitionTemplate =
    | Type0 of ProductDefinitionTemplateType0
    | Other of byte[]

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
        TypeOfFirstFixedSurface = reader.ReadByte();
        ScaleFactorOfFirstFixedSurface = reader.ReadByte();
        ScaledValueOfFirstFixedSurvace = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0);
        TypeOfSecondFixedSurface = reader.ReadByte();
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
