module GribReaderTests

open GribDotNet

open GribReader
open GridDefinitionSection
open ProductDefinitionSection

open NUnit.Framework
open FsUnit

let path1 = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let path2 = __SOURCE_DIRECTORY__ + "\\testData\\13051215.rap.t15z.awp130bgrbf00.grib2"

let gribRecord1 = readGribRecordFromPath path1
let gribRecord2 = readGribRecordFromPath path2

let grib1 = readGribFromPath path1
let grib2 = readGribFromPath path2

let isLambertConformal template =
    match template with
    | LambertConformal _ -> true
    | _ -> false

[<TestFixture>]
type GribReaderTests() =
    [<Test>]
    member test.IdentificationSection() =
        gribRecord1.IdentificationSection.SectionLength |> should equal 21u
        gribRecord2.IdentificationSection.SectionLength |> should equal 21u

    [<Test>]
    member test.GridDefinitionTemplateTypeSingleRecord() =
        isLambertConformal gribRecord1.GridDefinitionSection.GridDefinitionTemplate |> should be True
        isLambertConformal gribRecord2.GridDefinitionSection.GridDefinitionTemplate |> should be True
        
    [<Test>]
    member test.GridDefinitionTemplateType() =
        Set.ofList [for g in grib1 -> isLambertConformal g.GridDefinitionSection.GridDefinitionTemplate] |> should equal (Set.ofList [true])
        Set.ofList [for g in grib2 -> isLambertConformal g.GridDefinitionSection.GridDefinitionTemplate] |> should equal (Set.ofList [true])

    [<Test>]
    member test.GribsTest() =
        grib1.Length |> should be (greaterThan -1)
        grib2.Length |> should be (greaterThan -1)

    [<Test>]
    member test.ProductCountsGribFile1() =
        let productDefinitionSections = List.concat [for g in grib1 -> [for p in g.DataProducts -> p.ProductDefinitionSection]]
        let temps = productDefinitionSections |> List.choose (fun p -> match p.ProductDefinitionTemplate with | Type0 t -> Some t | _ -> None)
        let prods = [|for t in temps -> (t.ParameterCategory, t.ParameterNumber)|]
        let productCountList = prods |> Seq.groupBy Operators.id |> Seq.map (fun (x,y) -> (x,Seq.length y)) |> Seq.toList |> List.sortBy (fun x -> -(snd x))
        let productCounts = productCountList |> Map.ofList

        productCounts.TryFind (ProductCategory.Temperature, Product.Temperature) |> should equal (Some 46)
        productCounts.TryFind (ProductCategory.Moisture, Product.RelativeHumidity) |> should equal (Some 46)
        productCounts.TryFind (ProductCategory.Momentum, Product.UComponentOfWind) |> should equal (Some 46)
        productCounts.TryFind (ProductCategory.Momentum, Product.VComponentOfWind) |> should equal (Some 46)
        productCounts.TryFind (ProductCategory.Mass, Product.GeopotentialHeight) |> should equal (Some 45)
        productCounts.TryFind (ProductCategory.Momentum, Product.VerticalVelocityPressure) |> should equal (Some 43)
        productCounts.TryFind (ProductCategory.Mass, Product.Pressure) |> should equal (Some 5)
        productCounts.TryFind (ProductCategory.Temperature, Product.PotentialTemperature) |> should equal (Some 2)
        
    [<Test>]
    member test.ProductCountsGribFile2() =
        let productDefinitionSections = List.concat [for g in grib2 -> [for p in g.DataProducts -> p.ProductDefinitionSection]]
        let temps = productDefinitionSections |> List.choose (fun p -> match p.ProductDefinitionTemplate with | Type0 t -> Some t | _ -> None)
        let prods = [|for t in temps -> (t.ParameterCategory, t.ParameterNumber)|]
        let productCountList = prods |> Seq.groupBy Operators.id |> Seq.map (fun (x,y) -> (x,Seq.length y)) |> Seq.toList |> List.sortBy (fun x -> -(snd x))
        let productCounts = productCountList |> Map.ofList

        productCounts.TryFind (ProductCategory.Temperature, Product.Temperature) |> should equal (Some 52)
        productCounts.TryFind (ProductCategory.Momentum, Product.UComponentOfWind) |> should equal (Some 52)
        productCounts.TryFind (ProductCategory.Momentum, Product.VComponentOfWind) |> should equal (Some 52)
        productCounts.TryFind (ProductCategory.Mass, Product.Pressure) |> should equal (Some 51)
        productCounts.TryFind (ProductCategory.Mass, Product.GeopotentialHeight) |> should equal (Some 51)
        productCounts.TryFind (ProductCategory.Moisture, Product.SpecificHumidity) |> should equal (Some 51)
        productCounts.TryFind (ProductCategory.Momentum, Product.VerticalVelocityPressure) |> should equal (Some 50)