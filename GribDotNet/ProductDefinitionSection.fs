module ProductDefinitionSection

type ProductDefinitionSection = {
    SectionLength: uint32;
    SectionNumber: byte;
    NumberOfCoordinateValuesAfterTemplate: uint16;
    ProductDefinitionTemplateNumber: uint16;
    ProductDefinitionTemplate: byte[];
    ListOfCoordinateValues: byte[];
}

let readProductDefinitionSection (reader:System.IO.BinaryReader) =
    let sectionLength = System.BitConverter.ToUInt32(Array.rev(reader.ReadBytes(4)), 0)
    let sectionNumber = reader.ReadByte()
    let numberOfCoordinateValuesAfterTemplate = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let productDefinitionTemplateNumber = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    
    let productDefinitionTemplateLength = (int) (sectionLength - 9u - (uint32) numberOfCoordinateValuesAfterTemplate)

    let productDefinitionTemplate = reader.ReadBytes(productDefinitionTemplateLength)
    let listOfCoordinateValues = reader.ReadBytes((int) numberOfCoordinateValuesAfterTemplate)
    System.Diagnostics.Debug.WriteLine(sprintf "Product Definition Section Number %d" sectionNumber)

    {
        SectionLength = sectionLength;
        SectionNumber = sectionNumber;
        NumberOfCoordinateValuesAfterTemplate = numberOfCoordinateValuesAfterTemplate;
        ProductDefinitionTemplateNumber = productDefinitionTemplateNumber;
        ProductDefinitionTemplate = productDefinitionTemplate;
        ListOfCoordinateValues = listOfCoordinateValues
    }
