module IndicatorSection

exception GribIndicatorReadError of string

type Discipline =
   | Meteorological
   | Hydrological
   | LandSurface
   | Space
   | SpaceWeather
   | Reserved
   | Oceanographic
   | ReservedForLocalUse
   | Missing

let disciplineFromByte (x:byte) =
    match x with
    | 0uy -> Meteorological
    | 1uy -> Hydrological
    | 2uy -> LandSurface
    | 3uy -> Space
    | 4uy -> SpaceWeather
    | 10uy -> Oceanographic
    | x when 192uy <= x && x < 255uy -> ReservedForLocalUse
    | 255uy -> Missing
    | _ -> Reserved

type IndicatorSection = {
    InitialText: byte[];
    Reserved: byte[];
    Discipline: Discipline;
    EditionNumber: int
    TotalMessageLength: uint64
}

let readIndicatorSection (reader:System.IO.BinaryReader) =
    let initialText = reader.ReadBytes(4)
    let reserved = reader.ReadBytes(2)
    let discipline = reader.ReadByte()
    let editionNumber = reader.ReadByte()
    let totalMessageLength = System.BitConverter.ToUInt64(Array.rev(reader.ReadBytes(8)), 0)
    if initialText <> "GRIB"B then raise (GribIndicatorReadError("First 4 bytes not GRIB"))
    else {
        InitialText = initialText;
        Reserved =  reserved;
        Discipline = disciplineFromByte discipline;
        EditionNumber = (int) editionNumber;
        TotalMessageLength = totalMessageLength;
    }
