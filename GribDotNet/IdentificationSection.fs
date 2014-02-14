module GribDotNet.IdentificationSection

type SignficanceOfReferenceTime =
   | Analysis
   | StartOfForecast
   | VerifyingTimeOfForecast
   | ObservationTime
   | Reserved
   | ReservedForLocalUse
   | Missing

let signficanceOfReferenceTimeFromByte (x:byte) =
    match x with
    | 0uy -> Analysis
    | 1uy -> StartOfForecast
    | 2uy -> VerifyingTimeOfForecast
    | 3uy -> ObservationTime
    | x when 192uy <= x && x < 255uy -> ReservedForLocalUse
    | 255uy -> Missing
    | _ -> Reserved

type IdentificationSection = {
    SectionLength: uint32;
    OriginatingCenter: uint16;
    OriginatingSubcenter: uint16;
    MasterTablesVersionNumber: byte;
    LocalTablesVersionNumber: byte;
    SignficanceOfReferenceTime: SignficanceOfReferenceTime;
    ReferenceTime: System.DateTime;
    ProductionStatusOfProcessedData: byte;
    TypeOfProcessedData: byte;  
    Reserved: byte[]
}

let readIdentificationSection (reader:System.IO.BinaryReader) sectionLength =
    let originatingCenter = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let originatingSubcenter = System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0)
    let masterTablesVersionNumber = reader.ReadByte()
    let localTablesVersionNumber = reader.ReadByte()
    let significanceOfReferenceTime = signficanceOfReferenceTimeFromByte (reader.ReadByte())
    let year = (int) (System.BitConverter.ToUInt16(Array.rev(reader.ReadBytes(2)), 0))
    let month = (int) (reader.ReadByte())
    let day = (int) (reader.ReadByte())
    let hour = (int) (reader.ReadByte())
    let minute = (int) (reader.ReadByte())
    let second = (int) (reader.ReadByte())
    let referenceTime = (new System.DateTime(year=year, month=month, day=day, hour=hour, minute=minute, second=second));
    let productionStatusOfProcessedData = reader.ReadByte()
    let typeOfProcessedData = reader.ReadByte()
    let reserved = reader.ReadBytes((int) (sectionLength-21u))

    {
        SectionLength = sectionLength;
        OriginatingCenter = originatingCenter;
        OriginatingSubcenter = originatingSubcenter;
        MasterTablesVersionNumber = masterTablesVersionNumber;
        LocalTablesVersionNumber = localTablesVersionNumber;
        SignficanceOfReferenceTime = significanceOfReferenceTime;
        ReferenceTime = referenceTime;
        ProductionStatusOfProcessedData = productionStatusOfProcessedData;
        TypeOfProcessedData = typeOfProcessedData;  
        Reserved = reserved;
    }