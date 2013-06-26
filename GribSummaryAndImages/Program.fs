// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let path = __SOURCE_DIRECTORY__ + "\\..\\GribDotNetTests\\testData\\13051215.rap.t15z.awp130bgrbf00.grib2"
    let imagePath = @"C:\Temporary\GribDotNet"
    let grib = GribReader.readGribFromPath path
    Summary.summarizeGrib grib
    Summary.saveImagesFromGrib imagePath grib

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
