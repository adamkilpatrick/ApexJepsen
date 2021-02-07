
module LogParser
    open System
    open System.Text.RegularExpressions
    open FSharp.Data
    open MatchRecordBuilder
    let linePattern = "(\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d,\d\d\d).*(?:(?:Info Update)|(?:Event Fired))](.*)"

    type EventInfoLocationJson = JsonProvider<"""{"info":{"match_info":{"location":"{\"x\":\"243\",\"y\":\"452\",\"z\":\"234\"}"}},"feature":"location"}""">
    type LocationJson = JsonProvider<"""{"x":"243","y":"452","z":"234"}""">

    type MatchIdJson = JsonProvider<"""{"info":{"match_info":{"pseudo_match_id":"3230b3f0-db33-4103-9160-4198c65eb0a5"}},"feature":"match_info"}""">

    type KillFeedEventJson = JsonProvider<""" {"name":"kill_feed","data":"{\r\n  \"attackerName\": \"MrSlurp\",\r\n  \"victimName\": \"NfTaylong\",\r\n  \"weaponName\": \"Bleed Out\",\r\n  \"action\": \"Bleed Out\"\r\n}"} """>
    // local_player_name seems to be absent in bleed_out kill feed events, not sure why
    type KillFeedJson = JsonProvider<""" {"local_player_name": "CannonDeLuce","attackerName":"jononthemoon","victimName":"makalakimberly16","weaponName":"spitfire","action":"headshot_kill"} """>

    type DamageEventJson = JsonProvider<""" {"name":"damage","data":"{\r\n  \"targetName\": \"DcooperYT\",\r\n  \"damageAmount\": \"17.000000\",\r\n  \"armor\": \"true\",\r\n  \"headshot\": \"false\"\r\n}"} """>
    type DamageJson = JsonProvider<""" {"targetName": "DcooperYT","damageAmount": "17.000000","armor":"true","headshot":"false"} """>

    type WeaponInUseEventJson = JsonProvider<"""{"info":{"me":{"inUse":"{\"inUse\":\"HAVOC\"}"}},"feature":"inventory"}""">
    type WeaponInUseJson = JsonProvider<"""{"inUse":"HAVOC"}""">


    type CategorizedJson = MatchIdJson of MatchIdJson.Root 
                            | EventInfoLocationJson of LocationJson.Root 
                            | KillFeedJson of KillFeedJson.Root
                            | WeaponInUseJson of WeaponInUseJson.Root
                            | DamageJson of DamageJson.Root
                            | MatchEndLine

    let deserializeDamage (damageJsonString:string) =
        let e = DamageEventJson.Parse damageJsonString
        e.Data
        |> DamageJson.Parse

    let deserializeKillFeed (killFeedJsonString:string) =
        let e = KillFeedEventJson.Parse killFeedJsonString
        e.Data
        |> KillFeedJson.Parse
        
    let deserializeLocation (locationJsonString:string) =
        let e = EventInfoLocationJson.Parse locationJsonString
        e.Info.MatchInfo.Location 
        |> LocationJson.Parse

    let deserializeWeaponInUse (weaponInUseJson:string) =
        let e = WeaponInUseEventJson.Parse weaponInUseJson
        e.Info.Me.InUse
        |> WeaponInUseJson.Parse

    let mapKillFeedJsonToRelevantKillFeedJson (killFeedJson: KillFeedJson.Root) =
        let victimIsLocalPlayer = killFeedJson.LocalPlayerName = killFeedJson.VictimName
        let attackerIsLocalPlayer = killFeedJson.LocalPlayerName = killFeedJson.AttackerName
        if victimIsLocalPlayer || attackerIsLocalPlayer then
            Option.Some killFeedJson
        else
            Option.None

    let categorizeJson (jsonString: string) = 
        // These are mega hacky string matches and if someone gets cheeky with a username this will break fast
        try
            match jsonString with
                | j when j.Contains("""pseudo_match_id":null""") -> MatchEndLine |> Option.Some
                | j when j.Contains("pseudo_match_id") -> CategorizedJson.MatchIdJson(MatchIdJson.Parse(j)) |> Option.Some
                | j when j.Contains("location") -> CategorizedJson.EventInfoLocationJson(deserializeLocation j) |> Option.Some
                // Kill feed is special since we don't care about events not tied to local player
                | j when j.Contains("""kill_feed""") && j.Contains("""local_player_name""")-> deserializeKillFeed j
                                                                                                |> mapKillFeedJsonToRelevantKillFeedJson
                                                                                                |> Option.map CategorizedJson.KillFeedJson
                | j when j.Contains("inventory") && j.Contains("inUse")-> CategorizedJson.WeaponInUseJson(deserializeWeaponInUse j) |> Option.Some
                | j when j.Contains("damage") && j.Contains("targetName")->CategorizedJson.DamageJson(deserializeDamage j) |> Option.Some
                | _ -> Option.None
        with
            | _ ->Option.None


    let parseLogLine pattern logLine =
        let matchResult = Regex.Match(logLine,pattern)
        if not(matchResult.Success) || not(matchResult.Groups.Count = 3) 
            then Option.None
        else
            let dateString = (matchResult.Groups.Item 1).Value.ToString().Replace(",",":")
            let parsedDate = DateTime.ParseExact(dateString,"yyyy-MM-dd HH:mm:ss:fff",null, Globalization.DateTimeStyles.None)
            Option.Some(parsedDate, (matchResult.Groups.Item 2).Value.ToString())


    let mapKillFeedJsonToEvent (timeStamp: DateTime) (killFeedJson: KillFeedJson.Root) =
        let victimIsLocalPlayer = killFeedJson.LocalPlayerName = killFeedJson.VictimName
        let foreignPlayerName = if victimIsLocalPlayer then killFeedJson.AttackerName else killFeedJson.VictimName
        {TimeStamp=timeStamp; VictimIsLocalPlayer=victimIsLocalPlayer; ForeignPlayerName=foreignPlayerName; WeaponName=killFeedJson.WeaponName; ActionType=killFeedJson.Action}

    let mapCategorizedJsonToEvent (playerName: string) (timeStamp: DateTime) (json: CategorizedJson) =
        match json with
        | MatchIdJson j -> MatchRecordBuilder.MatchStartEvent {TimeStamp=timeStamp; MatchIdentifier=j.Info.MatchInfo.PseudoMatchId; LocalPlayer=playerName}
        | EventInfoLocationJson j -> MatchRecordBuilder.LocationEvent {TimeStamp=timeStamp; X=j.X; Y=j.Y; Z=j.Z}
        | KillFeedJson j -> MatchRecordBuilder.KillFeedEvent (mapKillFeedJsonToEvent timeStamp j)
        | WeaponInUseJson j -> MatchRecordBuilder.WeaponInUseEvent {TimeStamp=timeStamp; Weapon=j.InUse}
        | DamageJson j -> MatchRecordBuilder.DamageEvent {TimeStamp=timeStamp; Weapon="UNKNOWN"; Victim=j.TargetName; Shield=j.Armor; HeadShot=j.Headshot; Amount=int(j.DamageAmount)}
        | MatchEndLine -> MatchRecordBuilder.MatchEndEvent {TimeStamp=timeStamp;}

    let produceMatchEventFromLogLine (playerName: Option<string>) (logLine: string) =
        let splitLine = parseLogLine linePattern logLine
        let localPlayerName = playerName |> Option.defaultWith (fun () ->"UNKNOWN")
        splitLine
        |> Option.map (fun (t,n) -> (t, categorizeJson n))
        |> Option.filter (fun (t,n) -> n.IsSome)
        |> Option.map (fun (t,n) -> mapCategorizedJsonToEvent localPlayerName t n.Value)
