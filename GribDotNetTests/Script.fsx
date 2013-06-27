// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "..\\packages\\FsUnit.1.2.1.0\\lib\\Net40\\FsUnit.NUnit.dll"
#r "..\\GribDotNet\\bin\\Debug\\GribDotNet.dll"
#r "C:\\Program Files (x86)\NUnit 2.6.2\\bin\\framework\\nunit.framework.dll"
#load "GribReaderTests.fs"

open GribDotNet
open ProductDefinitionSection
open GribReader

let path = __SOURCE_DIRECTORY__ + "\\testData\\13051215.rap.t15z.awp130bgrbf00.grib2"
let grib = readGribFromPath path

let disciplines = Set.ofList [for g in grib -> g.IndicatorSection.Discipline]
let editionNumbers = Set.ofList [for g in grib -> g.IndicatorSection.EditionNumber]
let referenceTimes = Set.ofList [for g in grib -> g.IdentificationSection.ReferenceTime]
let masterTablesVersionNumbers = Set.ofList [for g in grib -> g.IdentificationSection.MasterTablesVersionNumber]
let productionStatusesOfProcessedData = Set.ofList [for g in grib -> g.IdentificationSection.ProductionStatusOfProcessedData]
let originatingCenters = Set.ofList [for g in grib -> g.IdentificationSection.OriginatingCenter]
let typesOfProcessedData = Set.ofList [for g in grib -> g.IdentificationSection.TypeOfProcessedData]
let gridDefinitionNumberOfDataPoints = Set.ofList [for g in grib -> g.GridDefinitionSection.NumberOfDataPoints]
let gridDefinitionTemplates = Set.ofList [for g in grib -> g.GridDefinitionSection.GridDefinitionTemplate]
let gridDefinitionSections = Set.ofList [for g in grib -> g.GridDefinitionSection]

let dataProducts = List.concat [for g in grib -> [for p in g.DataProducts -> p]]
let dataRepresentationSections = [for p in dataProducts -> p.DataRepresentationSection]
let dataRepresentationTemplateNumbers = Set.ofList [for d in dataRepresentationSections -> d.DataRepresentationTemplateNumber]

Set.ofList [for d in dataRepresentationSections -> d.NumberOfDataPoints]

let productDefinitionTemplateNumbers = Set.ofList (List.concat [for g in grib -> [for p in g.DataProducts -> p.ProductDefinitionSection.ProductDefinitionTemplateNumber]])
// let productDefinitionParameterCategories = Set.ofList (List.concat [for g in grib -> [for p in g.DataProducts -> p.ProductDefinitionSection.ProductDefinitionTemplate.[0]]])

let productDefinitionSections = List.concat [for g in grib -> [for p in g.DataProducts -> p.ProductDefinitionSection]]
let temps = productDefinitionSections |> List.choose (fun p -> match p.ProductDefinitionTemplate with | Type0 t -> Some t | _ -> None)

let fixedSurfaceTypes = Set.ofList [for t in temps -> t.TypeOfFirstFixedSurface]

let prods = [|for t in temps -> (t.ParameterCategory, t.ParameterNumber)|]

let productCounts = prods |> Seq.groupBy Operators.id |> Seq.map (fun (x,y) -> (x,Seq.length y)) |> Seq.toList |> List.sortBy (fun x -> -(snd x))

let uWindProductTemplates = (temps |> List.choose (fun t -> match t.ParameterCategory with | Momentum -> (if (t.ParameterNumber = Product.UComponentOfWind) then Some t else None) | _ -> None))
uWindProductTemplates.Length

[for p in uWindProductTemplates -> p.TypeOfFirstFixedSurface, p.ScaleFactorOfFirstFixedSurface, p.ScaledValueOfFirstFixedSurface, p.TypeOfSecondFixedSurface, p.ForecastTime, p.IndicatorOfUnitOfTimeRange]

// Define your library scripting code here

