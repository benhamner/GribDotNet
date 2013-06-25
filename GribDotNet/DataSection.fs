module DataSection

open DataRepresentationSection

type DataSection = {
    SectionLength: uint32;
    Data: byte[];
}

let readDataSection (reader:System.IO.BinaryReader) sectionLength =
    let data = reader.ReadBytes((int) (sectionLength - 5u))
    
    {
        SectionLength = sectionLength;
        Data = data
    }

let decodeJpegData (template:DataRepresentationTemplate5_40) (section:DataSection) =
    let result =
        JpegDecoder.decodeJpegGrid
            template.ReferenceValue
            template.BinaryScaleFactor
            template.DecimalScaleFactor
            template.NumberOfBitsRequiredToHoldTheResultingScaledAndReferencedDataValues
            section.Data
    result