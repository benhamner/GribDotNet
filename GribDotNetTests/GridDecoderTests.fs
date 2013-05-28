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
    let expectedDistanceDelta = 13545000u
    let expectedStandardParallel = 25000000u
    [<Test>]
    member test.LambertConformal() = 
        let targetGrib = grib1
        let lamberts =
            List.map
                (fun x->x.GridDefinitionSection.GridDefinitionTemplate |> GridDefinitionSection.asLambertComformalTemplate)
                targetGrib
        printf "Count: %d\n" (List.length lamberts)
        let shapes = lamberts |> List.map (fun x -> x.ShapeOfEarth)
        let nxs = lamberts |> List.map (fun x -> x.NumberOfPointsOnXAxis)
        let nys = lamberts |> List.map (fun x -> x.NumberOfPointsOnYAxis)
        let lats = lamberts |> (List.map (fun x -> x.LatitudeOfFirstGridPoint))
        let longs = lamberts |> (List.map (fun x -> x.LongitudeOfFirstGridPoint))
        let dxs = lamberts |> (List.map (fun x -> x.XDirectionGridLength))
        let dys = lamberts |> (List.map (fun x -> x.YDirectionGridLength))
        let standardParallels1 = lamberts |> (List.map (fun x -> x.FirstLatitudeFromThePoleAtWhichTheSecantConeCutsTheSphere))
        let standardParallels2 = lamberts |> (List.map (fun x -> x.SecondLatitudeFromThePoleAtWhichTheSecantConeCutsTheSphere))
        if verbose then
            let floatMean xs = xs |> List.averageBy (fun x-> float x)
            printf "Earth shape: %f\n" (floatMean shapes)
            printf "X points: %f\n" (nxs|>List.averageBy (fun x -> float x))
            printf "Y points: %f\n" (nys|>List.averageBy (fun x -> float x))
            printf "Latitude: %f\n" (lats|>List.averageBy (fun x -> float x * 1e-6))
            printf "Longitude: %f\n" (longs|>List.averageBy (fun x -> float x * 1e-6))
            printf "dxs: %f\n" (dxs|>List.averageBy (fun x -> float x * 1e-3))
            printf "dys: %f\n" (dys|>List.averageBy (fun x -> float x * 1e-3))
            printf "Standard Parallel 1: %f\n" (standardParallels1|>List.averageBy (fun x -> float x * 1e-6))
            printf "Standard Parallel 2: %f\n" (standardParallels2|>List.averageBy (fun x -> float x * 1e-6))
        List.max shapes |> should equal expectedShape
        List.min shapes |> should equal expectedShape
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