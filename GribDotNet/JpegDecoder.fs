module JpegDecoder

open System.Drawing

let bitmapToGrid referenceValue binaryScaling decimalScaling (bitmap:Bitmap) =
    let height = bitmap.Height
    let width = bitmap.Width
    let binaryScaling = 2.0**binaryScaling
    let decimalScaling = 10.0**(-decimalScaling)
    let addend = referenceValue * decimalScaling
    let multiplicand = binaryScaling * decimalScaling
    let result = Array.create height (Array.create width 0.0) // height x width jagged array
    for h = 0 to height - 1 do
        let row = result.[h]
        for w = 0 to width - 1 do
            row.[w] <- float (bitmap.GetPixel(w, h).GetBrightness()) * multiplicand + addend
    result

let decodeJpegGrid (referenceValue:float32) (binaryScaling:int16) (decimalScaling:int16) bytes =
    let maybeBitmap =
        try
            CSJ2K.J2kImage.FromBytes(bytes) :?> Bitmap |> Some
        with
            | :? System.InvalidOperationException -> None
    let result = Option.map (bitmapToGrid (float referenceValue) (float binaryScaling) (float decimalScaling)) maybeBitmap
    result