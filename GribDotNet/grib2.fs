module GribDotNet

type GribIndicatorSection = {
    EditionNumber: int
}

type Grib2 = {
    IndicatorSection: GribIndicatorSection
}

let readGrib2 (reader:System.IO.BinaryReader) = 0