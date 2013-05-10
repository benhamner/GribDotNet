module Section

open IndicatorSection
open IdentificationSection
open LocalUseSection
open GridDefinitionSection
open ProductDefinitionSection
open DataRepresentationSection
open BitMapSection
open DataSection

type Section =
 | Indicator of IndicatorSection
 | Identification of IdentificationSection
 | LocalUse of LocalUseSection
 | GridDefinition of GridDefinitionSection
 | ProductDefinition of ProductDefinitionSection
 | DataRepresentation of DataRepresentationSection
 | BitMap of BitMapSection
 | Data of DataSection
 | End

exception InvalidSectionNumberError of string

let readSectionByNumber : System.IO.BinaryReader -> byte -> uint32 -> Section = fun reader sectionNumber sectionLength ->
    match sectionNumber with
    | 1uy -> Identification (readIdentificationSection reader sectionLength)
    | 2uy -> LocalUse (readLocalUseSection reader sectionLength)
    | 3uy -> GridDefinition (readGridDefinitionSection reader sectionLength)
    | 4uy -> ProductDefinition (readProductDefinitionSection reader sectionLength)
    | 5uy -> DataRepresentation (readDataRepresentationSection reader sectionLength)
    | 6uy -> BitMap (readBitMapSection reader sectionLength)
    | 7uy -> Data (readDataSection reader sectionLength)
    | _   -> raise (InvalidSectionNumberError(sprintf "The section number %d is invalid" ((int) sectionNumber)))

let readSection (reader:System.IO.BinaryReader) = 
    let firstFour = reader.ReadBytes(4)
    let sectionLength = System.BitConverter.ToUInt32(Array.rev(firstFour), 0)
    let numBytesToRead = ((int) sectionLength) - 5
    match firstFour with
    | "GRIB"B -> Indicator (readIndicatorSection reader)
    | "7777"B -> End
    | _ -> let sectionNumber = reader.ReadByte()
           readSectionByNumber reader sectionNumber sectionLength

let readAllSections (reader:System.IO.BinaryReader) =
    [while reader.BaseStream.Position < reader.BaseStream.Length do yield readSection reader]

let readAllSectionsFromPath (path:string) = 
    let reader = (new System.IO.BinaryReader(System.IO.File.OpenRead(path), System.Text.Encoding.ASCII))
    try
        let sections = readAllSections reader
        sections
    finally
        reader.Close()