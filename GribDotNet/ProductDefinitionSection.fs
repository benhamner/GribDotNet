module ProductDefinitionSection

type ProductDefinitionTemplateType0 = {
    ParameterCategory: byte;
    ParameterNumber: byte;
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

type ProductDefinitionSection = {
    SectionLength: uint32;
    NumberOfCoordinateValuesAfterTemplate: uint16;
    ProductDefinitionTemplateNumber: uint16;
    ProductDefinitionTemplate: byte[];
    ListOfCoordinateValues: byte[];
}

let readProductDefinitionSection (reader:System.IO.BinaryReader) sectionLength =
    let numberOfCoordinateValuesAfterTemplate = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let productDefinitionTemplateNumber = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    
    let productDefinitionTemplateLength = (int) (sectionLength - 9u - (uint32) numberOfCoordinateValuesAfterTemplate)

    let productDefinitionTemplate = reader.ReadBytes(productDefinitionTemplateLength)
    let listOfCoordinateValues = reader.ReadBytes((int) numberOfCoordinateValuesAfterTemplate)

    {
        SectionLength = sectionLength;
        NumberOfCoordinateValuesAfterTemplate = numberOfCoordinateValuesAfterTemplate;
        ProductDefinitionTemplateNumber = productDefinitionTemplateNumber;
        ProductDefinitionTemplate = productDefinitionTemplate;
        ListOfCoordinateValues = listOfCoordinateValues
    }
