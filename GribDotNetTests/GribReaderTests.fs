module GribReaderTests

open GribReader

open GridDefinitionSection
open ProductDefinitionSection

open NUnit.Framework
open FsUnit

let path = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let gribRecord = readGribRecordFromPath path
let grib = readGribFromPath path

let isLambertConformal template =
    match template with
    | LambertConformal _ -> true
    | _ -> false

[<TestFixture>]
type GribReaderTests() =
    [<Test>]
    member test.IdentificationSection() =
        gribRecord.IdentificationSection.SectionLength |> should equal 21u
        
    [<Test>]
    member test.GridDefinitionTemplateTypeSingleRecord() =
        isLambertConformal gribRecord.GridDefinitionSection.GridDefinitionTemplate |> should be True
        
    [<Test>]
    member test.GridDefinitionTemplateType() =
        Set.ofList [for g in grib -> isLambertConformal g.GridDefinitionSection.GridDefinitionTemplate] |> should equal (Set.ofList [true])

    [<Test>]
    member test.GribsTest() =
        grib.Length |> should be (greaterThan -1)

    [<Test>]
    member test.ProductCounts() =
        let productDefinitionSections = List.concat [for g in grib -> [for p in g.DataProducts -> p.ProductDefinitionSection]]
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
