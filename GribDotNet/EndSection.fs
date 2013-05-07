module EndSection

exception GribEndReadError of string

type EndSection = {
    EndText: byte[]
}

let readEndSection (reader:System.IO.BinaryReader) =
    let endText = reader.ReadBytes(4)
    if endText <> "7777"B then raise (GribEndReadError("Ending 4 bytes not 7777"))
    else {
        EndText = endText;
    }
