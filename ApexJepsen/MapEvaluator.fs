module MapEvaluator

    open System

    type MapType = KingsCanyon =0
                    | Olympus =1

    type GetMapFromTime = (DateTime) -> MapType


    let getMapFromTime (time:DateTime) =
        let cyclePattern = [|
            (MapType.KingsCanyon,90);
            (MapType.Olympus,90);
            (MapType.KingsCanyon,60);
            (MapType.Olympus,60);
            (MapType.KingsCanyon,60);
            (MapType.Olympus,60);
            (MapType.KingsCanyon,120);
            (MapType.Olympus,120);
            (MapType.KingsCanyon,90);
            (MapType.Olympus,90);
            (MapType.KingsCanyon,120);
            (MapType.Olympus,120);
            (MapType.KingsCanyon,90);
            (MapType.Olympus,90);
            (MapType.KingsCanyon,120);
            (MapType.Olympus,120);
            (MapType.KingsCanyon,60);
            (MapType.Olympus,60);
            (MapType.KingsCanyon,90);
            (MapType.Olympus,90);
            (MapType.KingsCanyon,90);
            (MapType.Olympus,90);
            (MapType.KingsCanyon,120);
            (MapType.Olympus,120);
        |]
        let fullCycleTime = cyclePattern |> Array.sumBy (fun (_,n) -> n)
        let cycleStartDate = new DateTime(2021,1,5,13,0,0)

        let currentCycleStart = (time - cycleStartDate).TotalMinutes
                                    |> (fun n -> int(n)/fullCycleTime)
                                    |> (fun n -> float(n*fullCycleTime))
                                    |> cycleStartDate.AddMinutes

        let currentCycleOffset = (time - currentCycleStart).TotalMinutes |> int

        let cycleSums = cyclePattern
                        |> Array.scan (fun state (m,n) -> n+state) 0

        let cycleLandPoint = cycleSums
                                |> Array.takeWhile (fun n -> n <= currentCycleOffset)
                                |> Array.length
                                |> (fun n -> n-1)

        let map,_ = cyclePattern.[cycleLandPoint]

        map


