module JpegDecoder

open System.Drawing

let bitmapToGrid referenceValue binaryScaling decimalScaling numBitsStoringValue (bitmap:Bitmap) =
    let height = bitmap.Height
    let width = bitmap.Width
    let binaryScaling = 2.0**binaryScaling
    let decimalScaling = 10.0**(-decimalScaling)
    let addend = referenceValue * decimalScaling
    let multiplicand = binaryScaling * decimalScaling * (2.0 ** numBitsStoringValue)
    let result = Array.init height (fun _ -> Array.create width 0.0) // height x width jagged array
    for h = 0 to height - 1 do
        let row = result.[h]
        for w = 0 to width - 1 do
            row.[w] <- float (bitmap.GetPixel(w, h).GetBrightness()) * multiplicand + addend
    result

let bytesToBitmap bytes = 
    let maybeBitmap =
        try
            CSJ2K.J2kImage.FromBytes(bytes) :?> Bitmap |> Some
        with
            | :? System.InvalidOperationException -> None
    maybeBitmap

let decodeJpegGrid (referenceValue:float32) (binaryScaling:int16) (decimalScaling:int16) (numBitsStoringValue: byte) bytes =
    let maybeBitmap = bytesToBitmap bytes
    let result = Option.map (bitmapToGrid (float referenceValue) (float binaryScaling) (float decimalScaling) (float numBitsStoringValue)) maybeBitmap
    result