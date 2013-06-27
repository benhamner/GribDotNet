module GribDotNet.IndicatorSection

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
   | Unknown

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
    Reserved: byte[];
    Discipline: Discipline;
    EditionNumber: int
    TotalMessageLength: uint64
}

let readIndicatorSection (reader:System.IO.BinaryReader) =
    let reserved = reader.ReadBytes(2)
    let discipline = reader.ReadByte()
    let editionNumber = reader.ReadByte()
    let totalMessageLength = System.BitConverter.ToUInt64(Array.rev(reader.ReadBytes(8)), 0)
    {
        Reserved =  reserved;
        Discipline = disciplineFromByte discipline;
        EditionNumber = (int) editionNumber;
        TotalMessageLength = totalMessageLength;
    }
