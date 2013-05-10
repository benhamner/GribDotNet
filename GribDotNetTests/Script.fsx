// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "..\\packages\\FsUnit.1.2.1.0\\lib\\Net40\\FsUnit.NUnit.dll"
#r "..\\GribDotNet\\bin\\Debug\\GribDotNet.dll"
#r "C:\\Program Files (x86)\NUnit 2.6.2\\bin\\framework\\nunit.framework.dll"
#load "GribReaderTests.fs"

open GribReader

let path = __SOURCE_DIRECTORY__ + "\\testData\\rap_130_20130318_2300_018.grb2"
let gribs = readGribsFromPath path

let disciplines = Set.ofList [for g in gribs -> g.IndicatorSection.Discipline]
let editionNumbers = Set.ofList [for g in gribs -> g.IndicatorSection.EditionNumber]
let referenceTimes = Set.ofList [for g in gribs -> g.IdentificationSection.ReferenceTime]
let masterTablesVersionNumbers = Set.ofList [for g in gribs -> g.IdentificationSection.MasterTablesVersionNumber]
let productionStatusesOfProcessedData = Set.ofList [for g in gribs -> g.IdentificationSection.ProductionStatusOfProcessedData]
let originatingCenters = Set.ofList [for g in gribs -> g.IdentificationSection.OriginatingCenter]
let typesOfProcessedData = Set.ofList [for g in gribs -> g.IdentificationSection.TypeOfProcessedData]
let gridDefinitionTemplateTypes = Set.ofList [for g in gribs -> g.GridDefinitionSection.GridDefinitionTemplateType]
let gridDefinitionNumberOfDataPoints = Set.ofList [for g in gribs -> g.GridDefinitionSection.NumberOfDataPoints]
let productDefinitionTemplateNumbers = Set.ofList (List.concat [for g in gribs -> [for p in g.DataProducts -> p.ProductDefinitionSection.ProductDefinitionTemplateNumber]])


// Define your library scripting code here

