// This is the inital entry point, will pull events from a logfile and dump a location map of the first match from the file.

open System
open System.IO
open SixLabors.ImageSharp;
open SixLabors.ImageSharp.Processing;
open SixLabors.ImageSharp.Drawing.Processing;


let logFileroot = Environment.GetEnvironmentVariable("APEX_JEPSEN_POC_LOG_PATH")
let filePath =  if logFileroot = null then "./exampleInput.log" else logFileroot
// Made this with some trial and error in paint, needs to probably be dug into
let imagePath = """./kcmap.png"""

let translateCooardsToImageLocation (width:int) (height:int) (x:int) (y:int) =
    let scale = double(height) / double(1000)
    let scaledX = int(double(x)*scale)
    let scaledY = int(double(-1*y)*scale)
    (scaledX+width/2, scaledY+height/2)

[<EntryPoint>]
let main argv =

    let serializeRecord (record: MatchRecordBuilder.CompleteMatchRecord) =
        let filePath = "./"+record.MatchIdentifier.ToString()+".json"
        let jsonText = System.Text.Json.JsonSerializer.Serialize(record)
        File.WriteAllText(filePath,jsonText)

    let lines = File.ReadAllLines filePath
    let records = lines
                    |> Seq.map (LogParser.produceMatchEventFromLogLine Option.None)
                    |> Seq.filter Option.isSome
                    |> Seq.map Option.get
                    |> Seq.fold MatchRecordBuilder.stateFolder [||]
                    |> Seq.map (fun n -> match n with MatchRecordBuilder.CompleteMatchRecord m -> Option.Some m | _ -> Option.None)
                    |> Seq.filter Option.isSome
                    |> Seq.map Option.get

    records 
    |> Seq.iter serializeRecord

    let locations = records 
                        |> Seq.head
                        |> (fun n -> n.LocationEvents)

    let startTime = locations
                    |> Seq.sortBy (fun n -> n.TimeStamp)
                    |> Seq.head
                    |> (fun n -> n.TimeStamp)
    let endTime = locations
                    |> Seq.sortByDescending (fun n -> n.TimeStamp)
                    |> Seq.head
                    |> (fun n -> n.TimeStamp)
    let totalTime = endTime - startTime


    let drawPoint (imageIn: Image) size (color: Color) x y =
        let point = new PointF(float32(x), float32(y))
        let ellipse = SixLabors.ImageSharp.Drawing.EllipsePolygon(point, new SizeF(float32(size),float32(size)))
        let options = new ShapeGraphicsOptions()
        options.GraphicsOptions.BlendPercentage <- float32(0.2)
        imageIn.Mutate(fun n -> n.Fill(options,color,ellipse) |> ignore)

    let mapTimestampToColor (timeStamp: DateTime) = 
        let offset = timeStamp - startTime
        let pct = offset.TotalMilliseconds / totalTime.TotalMilliseconds
        let r = byte(255.0*pct)
        let b = byte(255.0*(1.0-pct))
        let color = Color.FromRgb(r,Byte.MinValue,b)

        color
    let image = Image.Load(imagePath)
    let drawimagePoint = drawPoint image 10
    locations
    |> Seq.map(fun n -> (mapTimestampToColor n.TimeStamp, (translateCooardsToImageLocation image.Width image.Height n.X n.Y)))
    |> Seq.iter(fun (c,(x,y)) -> drawimagePoint c x y)

    image.Save("output.jpg")

    
    printfn "Thank you for using me, a toool"
    0 // return an integer exit code
