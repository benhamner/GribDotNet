module Summary

open GribReader
open Microsoft.FSharp.Reflection

let internal toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, a -> case.Name + (if a.Length>0 then " " + a.[0].ToString() else "")

let internal count (els:'a list) = 
    els
    |> Seq.groupBy id
    |> Seq.map (fun (key, sq) -> key, Seq.length sq)
    |> Seq.toList
    |> List.sortBy (fun (key, count) -> -count)

let printCounts (infoString:string) els = 
    printfn "---%s COUNTS---" (infoString.ToUpper())
    for el, count in count els do
        printfn "%s: %s, Count: %d" infoString (toString el) count
    printfn ""

let summarizeGrib (grib:GribRecord list) =
    printfn "Total Number of Records: %d" (List.length grib)

    let disciplines = [for record in grib -> record.IndicatorSection.Discipline]
    printCounts "Discipline" disciplines

    let products = List.concat [for record in grib -> record.DataProducts]
    printfn "Total Number of Data Products: %d" (List.length products)

    let templatesTypeZero = products |> List.choose (fun p -> match p.ProductDefinitionSection.ProductDefinitionTemplate with | ProductDefinitionSection.ProductDefinitionTemplate.Type0 t -> Some (t,p) | _ -> None)
    printfn "Total Number of Templates Type 0: %d" (List.length templatesTypeZero)
    printCounts "Category" [for t, p in templatesTypeZero -> t.ParameterCategory]
    printCounts "Product" [for t, p in templatesTypeZero -> t.ParameterNumber]

let idString discipline product =
    match product.ProductDefinitionSection.ProductDefinitionTemplate with
        | ProductDefinitionSection.ProductDefinitionTemplate.Type0 t -> (toString discipline) + "\n" + (toString t.ParameterCategory) + "\n" + (toString t.ParameterNumber) + " " + (t.ScaledValueOfFirstFixedSurface.ToString())
        | _ -> (toString discipline) + " Unknown Product Template"

let idStringSortKey discipline product =
    match product.ProductDefinitionSection.ProductDefinitionTemplate with
        | ProductDefinitionSection.ProductDefinitionTemplate.Type0 t -> ((toString discipline) + " " + (toString t.ParameterCategory) + " " + (toString t.ParameterNumber) + " ", t.ScaledValueOfFirstFixedSurface)
        | _ -> ((toString discipline) + " Unknown Product Template", 0u)

let saveImagesFromGrib imagePath (grib:GribRecord list) = 
    let products = 
        List.concat [for record in grib -> [for product in record.DataProducts -> record.IndicatorSection.Discipline, product]]
        |> List.sortBy (fun (discipline,product) -> idStringSortKey discipline product)
        |> List.filter (fun (discipline,product) -> 
            match product.DataRepresentationSection.DataRepresentationTemplate with
            | DataRepresentationSection.DataRepresentationTemplate.Template5_40 t -> true
            | _ -> false)
        |> List.filter (fun (d,p) ->
            match JpegDecoder.bytesToBitmap p.DataSection.Data with
            | Some bitmap -> true
            | None -> false)

    for i, (discipline, product) in List.mapi (fun i p -> (i,p)) products do
        match product.DataRepresentationSection.DataRepresentationTemplate with
        | DataRepresentationSection.Template5_40 t ->
            match JpegDecoder.bytesToBitmap product.DataSection.Data with
            | Some bitmap ->
                bitmap.RotateFlip(System.Drawing.RotateFlipType.RotateNoneFlipY)
                let graphic = System.Drawing.Graphics.FromImage(bitmap);

                graphic.SmoothingMode <- System.Drawing.Drawing2D.SmoothingMode.AntiAlias
                graphic.InterpolationMode <- System.Drawing.Drawing2D.InterpolationMode.HighQualityBicubic;
                graphic.PixelOffsetMode <- System.Drawing.Drawing2D.PixelOffsetMode.HighQuality;
                let rectf = new System.Drawing.RectangleF(float32 20.0,float32 240.0,float32 400.0,float32 120.0)
                graphic.DrawString(idString discipline product, new System.Drawing.Font("Thaoma",float32 16.0), System.Drawing.Brushes.Blue, rectf)
                graphic.Flush()

                bitmap.Save(System.IO.Path.Combine(imagePath,sprintf "%d.png" i), System.Drawing.Imaging.ImageFormat.Png)
            | None -> ()
        | _ -> ()