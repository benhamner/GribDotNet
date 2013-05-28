module ParsingUtilities

let bytesToInt16 bigEndianBytes =
    let littleEndianBytes = Array.rev bigEndianBytes
    let lastIndex = Array.length littleEndianBytes - 1
    let mostSignificantByte = littleEndianBytes.[lastIndex]
    let isNegative = mostSignificantByte >= 128uy
    if isNegative then
        littleEndianBytes.[lastIndex] <- mostSignificantByte - 128uy
    let result = System.BitConverter.ToInt16(littleEndianBytes, 0) * if isNegative then -1s else 1s
    result

let bytesToUInt32 bigEndianBytes =
    let littleEndianBytes = Array.rev bigEndianBytes
    let result = System.BitConverter.ToUInt32(littleEndianBytes, 0)
    result

let bytesToUInt16 bigEndianBytes =
    let littleEndianBytes = Array.rev bigEndianBytes
    let result = System.BitConverter.ToUInt16(littleEndianBytes, 0)
    result