module IdentificationSection

type SignficanceOfReferenceTime =
   | Analysis
   | StartOfForecast
   | VerifytingTimeOfForecast
   | ObservationTime
   | Reserved
   | ReservedForLocalUse
   | Missing

type IdentificationSection = {
    SectionLength: int;
    SectionNumber: int;
    OriginatingCenter: int;
    OriginatingSubcenter: int;
    MasterTablesVersionNumber: int;
    LocalTablesVersionNumber: int;
    SignficanceOfReferenceTime: SignficanceOfReferenceTime;
    ReferenceTime: System.DateTime;
    ProductionStatusOfProcessedData: int;
    TypeOfProcessedData: int;  
    Reserved: byte[]
}
