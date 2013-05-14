module GribReader

open IndicatorSection
open IdentificationSection
open LocalUseSection
open GridDefinitionSection
open ProductDefinitionSection
open DataRepresentationSection
open BitMapSection
open DataSection
open Section

// Some Grib sets have multiples of Sections 4-7. This type holds these
type DataProduct = {
    ProductDefinitionSection: ProductDefinitionSection;
    DataRepresentationSection: DataRepresentationSection;
    BitMapSection: BitMapSection;
    DataSection: DataSection;
}

type GribRecord = {
    IndicatorSection: IndicatorSection;
    IdentificationSection: IdentificationSection;
    LocalUseSection: LocalUseSection option;
    GridDefinitionSection: GridDefinitionSection;
    DataProducts: List<DataProduct>
}

exception GribReadError of string

let rec readDataProducts (reader:System.IO.BinaryReader) discipline =
    match readSection reader discipline with
    | ProductDefinition productDefinitionSection ->
        let dataRepresentationSection =
            match readSection reader discipline with
            | DataRepresentation section -> section
            | _ -> raise (GribReadError("Expected Data Representation Section"))

        let bitMapSection =
            match readSection reader discipline with
            | BitMap section -> section
            | _ -> raise (GribReadError("Expected Bit Map Section"))

        let dataSection =
            match readSection reader discipline with
            | Data section -> section
            | _ -> raise (GribReadError("Expected Data Section"))

        let productDefinition = {
            ProductDefinitionSection = productDefinitionSection;
            DataRepresentationSection = dataRepresentationSection;
            BitMapSection = bitMapSection;
            DataSection = dataSection
        }

        productDefinition :: readDataProducts reader discipline
    | End -> []
    | _ -> raise (GribReadError("Expected A Product Definition or End Section"))

let readGribRecord (reader:System.IO.BinaryReader) = 
    let indicatorSection =
        match readSection reader Discipline.Unknown with
        | Indicator section -> section
        | _ -> raise (GribReadError("Expected Indicator Section"))

    let identificationSection =
        match readSection reader indicatorSection.Discipline with
        | Identification section -> section
        | _ -> raise (GribReadError("Expected Identification Section"))

    let localUseSection, gridDefinitionSection =
        match readSection reader indicatorSection.Discipline with
        | LocalUse localUseSection ->
            match readSection reader indicatorSection.Discipline with
            | GridDefinition gridDefinitionSection -> Some localUseSection, gridDefinitionSection
            | _ -> raise (GribReadError("Expected Grid Definition Section"))
        | GridDefinition section -> None, section
        | _ -> raise (GribReadError("Expected Local Use or Grid Definition Sections"))

    let dataProducts = readDataProducts reader indicatorSection.Discipline

    {
        IndicatorSection = indicatorSection
        IdentificationSection = identificationSection
        LocalUseSection = localUseSection
        GridDefinitionSection = gridDefinitionSection
        DataProducts = dataProducts
    }

let readGrib (reader:System.IO.BinaryReader) = 
    [while reader.BaseStream.Position < reader.BaseStream.Length do yield readGribRecord reader]

let readGribFromPath (path:string) = 
    let reader = (new System.IO.BinaryReader(System.IO.File.OpenRead(path)))
    try
        readGrib reader
    finally
        reader.Close()

let readGribRecordFromPath (path:string) = 
    let reader = (new System.IO.BinaryReader(System.IO.File.OpenRead(path)))
    try
        readGribRecord reader
    finally
        reader.Close()