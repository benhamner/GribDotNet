module Section

open IndicatorSection
open IdentificationSection
open LocalUseSection
open GridDefinitionSection
open ProductDefinitionSection
open DataRepresentationSection
open BitMapSection
open DataSection
open EndSection

type Section =
 | Indicator of IndicatorSection
 | Identification of IdentificationSection
 | LocalUse of LocalUseSection
 | GridDefinition of GridDefinitionSection
 | ProductDefinition of ProductDefinitionSection
 | DataRepresentation of DataRepresentationSection
 | BitMap of BitMapSection
 | End of EndSection

type Section2 =
 | Indicator of byte[]
 | Section of byte[]
 | End of byte[]

let readSection (reader:System.IO.BinaryReader) (i:int) = 
    let firstFour = reader.ReadBytes(4)
    let sectionLength = System.BitConverter.ToUInt32(Array.rev(firstFour), 0)
    let numBytesToRead = ((int) sectionLength) - 5
    //System.Diagnostics.Debug.WriteLine(sprintf "Section %d, Position: %d / %d" i reader.BaseStream.Position reader.BaseStream.Length)
    //System.Diagnostics.Debug.WriteLine(sprintf "Number of Bytes to Read %d" numBytesToRead)
    match firstFour with
    | "GRIB"B -> 
        System.Diagnostics.Debug.Write("Start Grib: ")
        Section2.Indicator (Array.concat [firstFour;(reader.ReadBytes(12))])
    | "7777"B -> 
        System.Diagnostics.Debug.WriteLine("End Grib")
        Section2.End (firstFour)
    | _ -> 
        let sectionNumber = reader.ReadBytes(1)
        System.Diagnostics.Debug.Write(sprintf "%d " ((int) sectionNumber.[0]))
        Section2.Section (Array.concat [firstFour; sectionNumber; (reader.ReadBytes(numBytesToRead))])

let readAllSections (reader:System.IO.BinaryReader) =
    let i = ref 0
    [while reader.BaseStream.Position < reader.BaseStream.Length do 
        i := !i+1
        yield readSection reader !i]
    //[for i in 1 .. 1000 -> readSection reader i]

let readAllSectionsFromPath (path:string) = 
    let reader = (new System.IO.BinaryReader(System.IO.File.OpenRead(path), System.Text.Encoding.ASCII))
    try
        let sections = readAllSections reader
        sections
    finally
        reader.Close()