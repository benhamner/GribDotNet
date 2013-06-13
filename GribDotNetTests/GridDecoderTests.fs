module GridDecoderTests

open GribReader
open GridDefinitionSection
open ProductDefinitionSection

open NUnit.Framework
open FsUnit

let path1 = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let path2 = __SOURCE_DIRECTORY__ + "\\testData\\13051215.rap.t15z.awp130bgrbf00.grib2"

let grib1 = readGribFromPath path1
let grib2 = readGribFromPath path2

[<TestFixture>]
type GridDecoderTests() =
    let verbose = true
    let expectedXPoints = 451u
    let expectedYPoints = 337u
    let expectedStartLatitude = 16281000u
    let expectedStartLongitude = 233862000u
    let expectedShape = 6u
    let expectedScanning = 64uy // Increasing x and y, rows adjacent
    let expectedComponents = 8uy // U and V along grid
    let expectedDistanceDelta = 13545000u
    let expectedStandardParallel = 25000000u
    let expectedReferenceLatitude = 25000000u
    let expectedReferenceLongitude = 265000000u

    [<Test>]
    member test.LambertConformal() = 
        let targetGrib = grib1
        let lamberts =
            List.map
                (fun x->x.GridDefinitionSection.GridDefinitionTemplate |> GridDefinitionSection.asLambertComformalTemplate)
                targetGrib
        printf "Count: %d\n" (List.length lamberts)
        let shapes = lamberts |> List.map (fun x -> x.ShapeOfEarth)
        let scanning = lamberts |> List.map (fun x -> x.ScanningMode)
        let components = lamberts |> List.map (fun x -> x.ResolutionAndComponentFlags)
        let nxs = lamberts |> List.map (fun x -> x.NumberOfPointsOnXAxis)
        let nys = lamberts |> List.map (fun x -> x.NumberOfPointsOnYAxis)
        let lats = lamberts |> (List.map (fun x -> x.LatitudeOfFirstGridPoint))
        let longs = lamberts |> (List.map (fun x -> x.LongitudeOfFirstGridPoint))
        let dxs = lamberts |> (List.map (fun x -> x.XDirectionGridLength))
        let dys = lamberts |> (List.map (fun x -> x.YDirectionGridLength))
        let standardParallels1 = lamberts |> (List.map (fun x -> x.FirstLatitudeFromThePoleAtWhichTheSecantConeCutsTheSphere))
        let standardParallels2 = lamberts |> (List.map (fun x -> x.SecondLatitudeFromThePoleAtWhichTheSecantConeCutsTheSphere))
        let referenceLatitude = lamberts |> (List.map (fun x -> x.LatitudeWhereDxAndDyAreSpecified))
        let referenceLongitude = lamberts |> (List.map (fun x -> x.LongitudeOfMeridianParallelToYAxisAlongWhichLatitudeIncreasesAsTheYCoordinateIncreases))
        if verbose then
            let floatMean xs = xs |> List.averageBy (fun x-> float x)
            printf "Earth shape: %f\n" (floatMean shapes)
            printf "Scanning: %f\n" (floatMean scanning)
            printf "Components: %f\n" (floatMean components)
            printf "X points: %f\n" (nxs|>List.averageBy (fun x -> float x))
            printf "Y points: %f\n" (nys|>List.averageBy (fun x -> float x))
            printf "Latitude: %f\n" (lats|>List.averageBy (fun x -> float x * 1e-6))
            printf "Longitude: %f\n" (longs|>List.averageBy (fun x -> float x * 1e-6))
            printf "dxs: %f\n" (dxs|>List.averageBy (fun x -> float x * 1e-3))
            printf "dys: %f\n" (dys|>List.averageBy (fun x -> float x * 1e-3))
            printf "Standard Parallel 1: %f\n" (standardParallels1|>List.averageBy (fun x -> float x * 1e-6))
            printf "Standard Parallel 2: %f\n" (standardParallels2|>List.averageBy (fun x -> float x * 1e-6))
            printf "Ref Latitude: %f\n" (referenceLatitude|>List.averageBy (fun x -> float x * 1e-6))
            printf "Ref Longtitude: %f\n" (referenceLongitude|>List.averageBy (fun x -> float x * 1e-6))
        List.max shapes |> should equal expectedShape
        List.min shapes |> should equal expectedShape
        List.max scanning |> should equal expectedScanning
        List.min scanning |> should equal expectedScanning
        List.max components |> should equal expectedComponents
        List.min components |> should equal expectedComponents
        List.max nxs |> should equal expectedXPoints
        List.min nxs |> should equal expectedXPoints
        List.max nys |> should equal expectedYPoints
        List.min nys |> should equal expectedYPoints
        List.max lats |> should equal expectedStartLatitude
        List.min longs |> should equal expectedStartLongitude
        List.min dxs |> should equal expectedDistanceDelta
        List.max dxs |> should equal expectedDistanceDelta
        List.min dys |> should equal expectedDistanceDelta
        List.max dys |> should equal expectedDistanceDelta
        List.min standardParallels1 |> should equal expectedStandardParallel
        List.max standardParallels1 |> should equal expectedStandardParallel
        List.min standardParallels2 |> should equal expectedStandardParallel
        List.max standardParallels2 |> should equal expectedStandardParallel
        List.min referenceLatitude |> should equal expectedReferenceLatitude
        List.max referenceLatitude |> should equal expectedReferenceLatitude
        List.min referenceLongitude |> should equal expectedReferenceLongitude
        List.max referenceLongitude |> should equal expectedReferenceLongitude

    [<Test>]
    member test.``Decode grid locations`` () =
        let targetGrib = grib1
        let lamberts =
            List.map
                (fun x->x.GridDefinitionSection.GridDefinitionTemplate |> GridDefinitionSection.asLambertComformalTemplate)
                targetGrib
        printf "Count: %d\n" (List.length lamberts)
        let single = GridDefinitionSection.decodeLatitudeLongitude (List.head lamberts)
        let printIt (lat, long) = printf "%f, %f\n" (float lat) (float long |> fun x -> if x > 180.0 then x-360.0 else x)
        printIt single.[0].[0]
        printIt single.[0].[450]
        printIt single.[336].[0]
        printIt single.[336].[450]
        let checkRow row =
            let latitudes = Array.map (fst >> float) row
            let maximumLatitude = Array.max latitudes
            let minimumLatitude = Array.min latitudes
            maximumLatitude-minimumLatitude |> should lessThanOrEqualTo 5.0 // Latitudes should be similar
            let longitudes = Array.map (snd >> float) row
            let lastLongitude = longitudes.[Array.length longitudes - 1]
            let maximumLongitude = Array.max longitudes
            maximumLongitude |> should lessThanOrEqualTo lastLongitude // Longitudes should be increasing
        Array.iteri (fun i r -> checkRow r) single
        (*
    [<Test>]
    member test.``Decode products``() = 
        let targetGrib = grib2
        let requestedProducts = new System.Collections.Generic.HashSet<Product>([Product.UComponentOfWind; Product.VComponentOfWind])
        //let requestedProducts = new System.Collections.Generic.HashSet<Product>([Product.Temperature])
        let isRequestedProduct x =
            let t = x.ProductDefinitionSection.ProductDefinitionTemplate in
            t.IsTypeZero && requestedProducts.Contains((Option.get (t.GetTypeZero())).ParameterNumber)
        let products =
            targetGrib |> List.collect (fun x -> x.DataProducts) |>
            List.map (fun x -> (x.ProductDefinitionSection, x.ProductDefinitionSection.ProductDefinitionTemplate)) |>
            List.filter (fun (_,x) -> x.IsTypeZero) |>
            List.map (fun (x,y) -> (x, Option.get (y.GetTypeZero()))) |>
            List.filter (fun (x,y) -> requestedProducts.Contains(y.ParameterNumber))
        printf "Products: %d\n" (List.length products)
        let filtered = products |> List.filter (fun (x,y) -> y.TypeOfFirstFixedSurface=HybridLevel)
        printf "Filtered: %d\n" (List.length filtered)
        let results =
            filtered |> List.map (fun (x,y) -> y.ScaledValueOfFirstFixedSurface) |>
            Seq.groupBy (fun x -> x) |> Seq.map (fun (x,y) -> (x, Seq.length y)) |> Seq.sortBy fst |>
            Seq.map (fun x -> x.ToString()) |> String.concat ", "
        printf "%s\n" results

        for sigmaLevel in [1..50] do
            printf "%d - %f\n" sigmaLevel (VerticalCoordinateDecoder.decodeSigma sigmaLevel)
            *)