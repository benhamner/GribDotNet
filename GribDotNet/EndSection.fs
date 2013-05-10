module EndSection

exception GribEndReadError of string

type EndSection = {
    EndText: byte[]
}

let readEndSection (reader:System.IO.BinaryReader) =
    let peekChar = reader.PeekChar()
    System.Diagnostics.Debug.WriteLine(sprintf "Peek Char %d" peekChar)
    if peekChar = (int) '7' then
        let endText = reader.ReadBytes(4)
        {
            EndText = endText
        }
    else
        let endText = reader.ReadBytes(100000)
        let peekChar2 = reader.PeekChar()
        System.Diagnostics.Debug.WriteLine(sprintf "Next Peek Char %d" peekChar2)
        {
            EndText = endText
        }
    //if endText <> "7777"B then raise (GribEndReadError("Ending 4 bytes not 7777"))
    //else {
    //    EndText = endText;
    //}
