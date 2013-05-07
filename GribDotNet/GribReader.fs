module GribReader

open IndicatorSection
open IdentificationSection
open LocalUseSection

type Grib = {
    IndicatorSection: IndicatorSection;
    IdentificationSection: IdentificationSection;
    LocalUseSection: LocalUseSection;
}

let readGrib (reader:System.IO.BinaryReader) = 
    {
        IndicatorSection = readIndicatorSection reader
        IdentificationSection = readIdentificationSection reader
        LocalUseSection = readLocalUseSection reader
    }

let readGribFromPath (path:string) = 
    let reader = (new System.IO.BinaryReader(System.IO.File.OpenRead(path)))
    let grib = readGrib reader
    reader.Close()
    grib