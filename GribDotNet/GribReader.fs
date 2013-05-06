module GribReader

open IndicatorSection
open IdentificationSection

type Grib = {
    IndicatorSection: IndicatorSection;
}

let readGrib (reader:System.IO.BinaryReader) = 
    {
        IndicatorSection = readIndicatorSection reader
    }

let readGribFromPath (path:string) = 
    readGrib (new System.IO.BinaryReader(System.IO.File.OpenRead(path)))