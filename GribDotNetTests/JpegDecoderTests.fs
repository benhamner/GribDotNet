module JpegDecoderTests

open GribDotNet

open NUnit.Framework
open FsUnit

open System.Drawing
open System.Collections.Generic

open GribReader
open GridDefinitionSection
open ProductDefinitionSection

let path1 = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let path2 = __SOURCE_DIRECTORY__ + "\\testData\\13051215.rap.t15z.awp130bgrbf00.grib2"

let grib1 = readGribFromPath path1
let grib2 = readGribFromPath path2

[<TestFixture>]
type JpegDecoderTests() =
    let verbose = true
    let expectedWidth = 451u // For our data
    let expectedHeight = 337u // For our data
    let expectedTemplateNumber = 40us // The products we care about use template 40 (5.40)
    let temperatureProducts = new HashSet<Product>([Product.Temperature])
    let windProducts = new HashSet<Product>([Product.UComponentOfWind; Product.VComponentOfWind])

    let retrieveRelevantGrids (requestedProduct:Product) targetGrib =
        let relevantProducts =
            targetGrib |> List.collect (fun v -> (v.DataProducts)) |>
            List.map
                (fun x ->
                    (x.ProductDefinitionSection.ProductDefinitionTemplate,
                        x.DataRepresentationSection.DataRepresentationTemplate,
                        x.DataSection.Data)) |>
            List.filter
                    (fun (t,r,d) ->
                        t.IsTypeZero && requestedProduct = (Option.get (t.GetTypeZero())).ParameterNumber) |>
            List.map
                (fun (t,r,d) -> t.GetTypeZero() |> Option.get, DataRepresentationSection.asTemplate5_40 r, d)
        let relevantGrids =
            relevantProducts |>
            List.map
                (fun (t,r,d) ->
                    let vertical = VerticalCoordinateDecoder.decodeVertical t.TypeOfFirstFixedSurface (float t.ScaledValueOfFirstFixedSurface)
                    let altitude =
                        match vertical with
                        | Choice1Of2(x) -> x
                        | Choice2Of2(y) -> -1.0 //failwith (sprintf "Could not decode vertical coordinate %s" (y.ToString()))
                    (altitude,
                        JpegDecoder.decodeJpegGrid r.ReferenceValue r.BinaryScaleFactor r.DecimalScaleFactor r.NumberOfBitsRequiredToHoldTheResultingScaledAndReferencedDataValues d |> Option.get))
        relevantGrids

    let retrieveRelevantData (requestedProducts:HashSet<Product>) targetGrib =
        let relevantProducts =
            targetGrib |> List.collect (fun v -> v.DataProducts) |>
                List.filter
                    (fun x ->
                        let t = x.ProductDefinitionSection.ProductDefinitionTemplate in
                        t.IsTypeZero && requestedProducts.Contains((Option.get (t.GetTypeZero())).ParameterNumber))
        if verbose then
            printf "Relevant products: %d/%d\n" (List.length relevantProducts) (List.sumBy (fun x -> List.length x.DataProducts) targetGrib)
        let relevantData =
            relevantProducts |> List.map (
                fun x ->
                    (DataRepresentationSection.asTemplate5_40 x.DataRepresentationSection.DataRepresentationTemplate,
                        x.DataSection.Data))
        if verbose then
            printf "Relevant data sections: %d\n" (List.length relevantData)
        // The products we care about only have 1 data section each
        List.length relevantData |> should equal (List.length relevantProducts)
        for product in relevantProducts do
            product.DataRepresentationSection.DataRepresentationTemplateNumber |> should equal expectedTemplateNumber
        relevantData

    let retrieveRelevantBitmaps (requestedProducts:HashSet<Product>) targetGrib =
        let relevantData = retrieveRelevantData requestedProducts targetGrib
        let relevantBitmaps = List.map (fun(x,y)->(x,CSJ2K.J2kImage.FromBytes(y) :?> Bitmap)) relevantData
        relevantBitmaps

    let checkHasHue (bitmap:Bitmap) =
        let mutable flag = false
        for h = 0 to bitmap.Height - 1 do
            for w = 0 to bitmap.Width - 1 do
                if bitmap.GetPixel(w, h).GetHue() > 0.0f then
                    flag <- true
        flag
    let checkHasSaturation (bitmap:Bitmap) =
        let mutable flag = false
        for h = 0 to bitmap.Height - 1 do
            for w = 0 to bitmap.Width - 1 do
                if bitmap.GetPixel(w, h).GetSaturation() > 0.0f then
                    flag <- true
        flag

    [<Test>]
    member test.``Check test data is correct and parses``() =
        let targetGrib = grib2
        let points = List.map (fun x -> float x.GridDefinitionSection.NumberOfDataPoints) targetGrib
        List.max points |> should equal (expectedWidth*expectedHeight)
        List.min points |> should equal (expectedWidth*expectedHeight)
        let widths = List.map (fun x -> (asLambertComformalTemplate x.GridDefinitionSection.GridDefinitionTemplate).NumberOfPointsOnXAxis) targetGrib
        List.max widths |> should equal expectedWidth
        List.min widths |> should equal expectedWidth
        let heights = List.map (fun x -> (asLambertComformalTemplate x.GridDefinitionSection.GridDefinitionTemplate).NumberOfPointsOnYAxis) targetGrib
        List.max heights |> should equal expectedHeight
        List.min heights |> should equal expectedHeight

        let bitmaps = retrieveRelevantBitmaps temperatureProducts targetGrib
        let bitmapHeights = List.map (fun (x:Bitmap) -> float x.Height) (List.unzip bitmaps |> snd)
        List.max bitmapHeights |> should equal expectedHeight
        List.min bitmapHeights |> should equal expectedHeight
        let bitmapWidths = List.map (fun (x:Bitmap) -> float x.Width) (List.unzip bitmaps |> snd)
        List.max bitmapWidths |> should equal expectedWidth
        List.min bitmapWidths |> should equal expectedWidth

        let hasHue = List.exists (fun (_,b) -> checkHasHue b) bitmaps
        hasHue |> should equal false
        let hasSaturation = List.exists (fun (_,b) -> checkHasSaturation b) bitmaps
        hasSaturation |> should equal false
        
    [<Test>]
    member test.TemperatureDecoding() =
        let targetGrib = grib2
        let temperatureGrids =
            retrieveRelevantData temperatureProducts targetGrib |>
            List.map (fun (t, b) -> JpegDecoder.decodeJpegGrid t.ReferenceValue t.BinaryScaleFactor t.DecimalScaleFactor t.NumberOfBitsRequiredToHoldTheResultingScaledAndReferencedDataValues b |> Option.get)

        for grid in temperatureGrids do
            for row in grid do
                for cell in row do
                    cell |> should greaterThan 180.0 // Should be over -80C
                    cell |> should lessThan 320.0 // Should be under 20C

    [<Test>]
    member test.TemperatureAtAltitudes() =
        //let targetGrib = readGribFromPath "C:/Temporary/KittyHawk/13020100.rap.t00z.awp130bgrbf00.grib2"
        let targetGrib = grib2
        let temperatureGrids = retrieveRelevantGrids Product.Temperature targetGrib
        for (altitude, grid) in temperatureGrids do
            printfn "%f: %f" altitude (grid.[1].[225])
          
    [<Test>]
    member test.WindAtAltitudes() =
        //let targetGrib = readGribFromPath "C:/Temporary/KittyHawk/13020100.rap.t00z.awp130bgrbf00.grib2"
        let targetGrib = grib2
        let temperatureGrids = retrieveRelevantGrids Product.UComponentOfWind targetGrib
        printfn "Origin"
        for (altitude, grid) in temperatureGrids do
            printfn "%f: %f" altitude (grid.[1].[1])
        printfn "Middle"
        for (altitude, grid) in temperatureGrids do
            printfn "%f: %f" altitude (grid.[150].[200])
        printfn "Edge"
        for (altitude, grid) in temperatureGrids do
            printfn "%f: %f" altitude (grid.[300].[300])

    [<Test>]
    member test.WindSpeedDecoding() =
        let targetGrib = grib2
        let temperatureGrids =
            retrieveRelevantData (new HashSet<Product>([Product.WindSpeed])) targetGrib |>
            List.map (fun (t, b) -> JpegDecoder.decodeJpegGrid t.ReferenceValue t.BinaryScaleFactor t.DecimalScaleFactor t.NumberOfBitsRequiredToHoldTheResultingScaledAndReferencedDataValues b |> Option.get)
        let mutable maxValue = 0.0
        let mutable minValue = 0.0
        for grid in temperatureGrids do
            for row in grid do
                for cell in row do
                    if cell > maxValue then
                        maxValue <- cell
                    if cell < minValue then
                        minValue <- cell
                    //cell |> should greaterThan 190.0 // Should be over -80C
                    //cell |> should lessThan 290.0 // Should be under 20C
        printf "Maximum wind: %f m/s\n" maxValue
        printf "Minimum wind: %f m/s\n" minValue

    [<Test>]
    member test.WindComponentsDecoding() =
        let targetGrib = grib2
        let temperatureGrids =
            retrieveRelevantData windProducts targetGrib |>
            List.map (fun (t, b) -> JpegDecoder.decodeJpegGrid t.ReferenceValue t.BinaryScaleFactor t.DecimalScaleFactor t.NumberOfBitsRequiredToHoldTheResultingScaledAndReferencedDataValues b |> Option.get)
        let mutable maxValue = 0.0
        let mutable minValue = 0.0
        for grid in temperatureGrids do
            for row in grid do
                for cell in row do
                    if cell > maxValue then
                        maxValue <- cell
                    if cell < minValue then
                        minValue <- cell
                    //cell |> should greaterThan 190.0 // Should be over -80C
                    //cell |> should lessThan 290.0 // Should be under 20C
        printf "Maximum wind: %f m/s\n" maxValue
        printf "Minimum wind: %f m/s\n" minValue

