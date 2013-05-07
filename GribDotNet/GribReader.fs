module GribReader

open IndicatorSection
open IdentificationSection
open LocalUseSection
open GridDefinitionSection
open ProductDefinitionSection
open DataRepresentationSection
open BitMapSection
open DataSection
open EndSection

type Grib = {
    IndicatorSection: IndicatorSection;
    IdentificationSection: IdentificationSection;
    LocalUseSection: LocalUseSection;
    GridDefinitionSection: GridDefinitionSection;
    ProductDefinitionSection: ProductDefinitionSection;
    DataRepresentationSection: DataRepresentationSection;
    BitMapSection: BitMapSection;
    DataSection: DataSection;
    EndSection: EndSection
}

let readGrib (reader:System.IO.BinaryReader) = 
    let indicatorSection = readIndicatorSection reader
    let identificationSection = readIdentificationSection reader
    // Our test file doesn't use the local use section. Need to find a clean way to handle this
    let localUseSection = blankLocalUseSection
    let gridDefinitionSection = readGridDefinitionSection reader
    let productDefinitionSection = readProductDefinitionSection reader
    let dataRepresentationSection = readDataRepresentationSection reader
    let bitMapSection = readBitMapSection reader
    let dataSection = readDataSection reader
    let endSection = readEndSection reader

    {
        IndicatorSection = indicatorSection
        IdentificationSection = identificationSection
        LocalUseSection = localUseSection
        GridDefinitionSection = gridDefinitionSection
        ProductDefinitionSection = productDefinitionSection
        DataRepresentationSection = dataRepresentationSection
        BitMapSection = bitMapSection
        DataSection = dataSection
        EndSection = endSection
    }

let readGribFromPath (path:string) = 
    let reader = (new System.IO.BinaryReader(System.IO.File.OpenRead(path)))
    try
        let grib = readGrib reader
        grib
    finally
        reader.Close()