
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
    type KillFeedJson = JsonProvider<""" {"local_player_name": "CannonDeLuce","attackerName":"jononthemoon","victimName":"makalakimberly16","weaponName":"spitfire","action":"headshot_kill"} """>

    type DamageEventJson = JsonProvider<""" {"name":"damage","data":"{\r\n  \"targetName\": \"DcooperYT\",\r\n  \"damageAmount\": \"17.000000\",\r\n  \"armor\": \"true\",\r\n  \"headshot\": \"false\"\r\n}"} """>
    type DamageJson = JsonProvider<""" {"targetName": "DcooperYT","damageAmount": "17.000000","armor":"true","headshot":"false"} """>

    type WeaponInUseEventJson = JsonProvider<"""{"info":{"me":{"inUse":"{\"inUse\":\"HAVOC\"}"}},"feature":"inventory"}""">
    type WeaponInUseJson = JsonProvider<"""{"inUse":"HAVOC"}""">

    type ScoreBoardEventJson = JsonProvider<"""{"info":{"match_info":{"tabs":"{\"kills\":1,\"spectators\":0,\"teams\":2,\"players\":null,\"damage\":495,\"cash\":35}"}},"feature":"match_info"}""">
    type ScoreBoardJson = JsonProvider<"""[{"kills":1,"spectators":0,"teams":2,"players":null,"damage":495,"cash":35},
                                          {"kills":1,"spectators":0,"teams":2,"players":1,"damage":495,"cash":35}]
                                        """, true>

    type PlayerNameSetJson = JsonProvider<"""{"info":{"me":{"name":"CannonDeLuce"}},"feature":"me"}""">


    type CategorizedJson = MatchIdJson of MatchIdJson.Root 
                            | EventInfoLocationJson of LocationJson.Root 
                            | KillFeedJson of KillFeedJson.Root
                            | WeaponInUseJson of WeaponInUseJson.Root
                            | DamageJson of DamageJson.Root
                            | ScoreBoardJson of ScoreBoardJson.Root
                            | VictoryLine
                            | MatchEndLine

    type LogParseStateHolder = {PlayerName:string;}

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

    let deserializeScoreBoard (scoreBoardEventJson:string) = 
        let e = ScoreBoardEventJson.Parse scoreBoardEventJson
        e.Info.MatchInfo.Tabs
        |> ScoreBoardJson.Parse

    let mapKillFeedJsonToRelevantKillFeedJson (parserState: LogParseStateHolder) (killFeedJson: KillFeedJson.Root) =
        let victimIsLocalPlayer = parserState.PlayerName = killFeedJson.VictimName
        let attackerIsLocalPlayer = parserState.PlayerName = killFeedJson.AttackerName
        if victimIsLocalPlayer || attackerIsLocalPlayer then
            Option.Some killFeedJson
        else
            Option.None

    let categorizeJson (parserState: LogParseStateHolder) (jsonString: string) = 
        // These are mega hacky string matches and if someone gets cheeky with a username this will break fast
        try
            match jsonString with
                | j when j.Contains("""pseudo_match_id":null""") -> MatchEndLine |> Option.Some
                | j when j.Contains("pseudo_match_id") -> CategorizedJson.MatchIdJson(MatchIdJson.Parse(j)) |> Option.Some
                | j when j.Contains("""{"location":""") -> CategorizedJson.EventInfoLocationJson(deserializeLocation j) |> Option.Some
                // Kill feed is special since we don't care about events not tied to local player
                | j when j.Contains("""kill_feed""") -> deserializeKillFeed j
                                                        |> mapKillFeedJsonToRelevantKillFeedJson parserState
                                                        |> Option.map CategorizedJson.KillFeedJson
                | j when j.Contains("inventory") && j.Contains("inUse")-> CategorizedJson.WeaponInUseJson(deserializeWeaponInUse j) |> Option.Some
                | j when j.Contains("damage") && j.Contains("targetName")->CategorizedJson.DamageJson(deserializeDamage j) |> Option.Some
                | j when j.Contains("""\"spectators\":""") -> CategorizedJson.ScoreBoardJson(deserializeScoreBoard j) |> Option.Some
                | j when j.Contains("""{"victory":"true"}""") -> CategorizedJson.VictoryLine |> Option.Some
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


    let mapKillFeedJsonToEvent (parserState: LogParseStateHolder) (timeStamp: DateTime) (killFeedJson: KillFeedJson.Root) =
        let victimIsLocalPlayer = parserState.PlayerName = killFeedJson.VictimName
        let foreignPlayerName = if victimIsLocalPlayer then killFeedJson.AttackerName else killFeedJson.VictimName
        {TimeStamp=timeStamp; VictimIsLocalPlayer=victimIsLocalPlayer; ForeignPlayerName=foreignPlayerName; WeaponName=killFeedJson.WeaponName; ActionType=killFeedJson.Action}

    let mapCategorizedJsonToEvent (parserState:LogParseStateHolder) (timeStamp: DateTime) (json: CategorizedJson) =
        match json with
        | MatchIdJson j -> MatchRecordBuilder.MatchStartEvent {TimeStamp=timeStamp; MatchIdentifier=j.Info.MatchInfo.PseudoMatchId; LocalPlayer=parserState.PlayerName}
        | EventInfoLocationJson j -> MatchRecordBuilder.LocationEvent {TimeStamp=timeStamp; X=j.X; Y=j.Y; Z=j.Z}
        | KillFeedJson j -> MatchRecordBuilder.KillFeedEvent (mapKillFeedJsonToEvent parserState timeStamp j)
        | WeaponInUseJson j -> MatchRecordBuilder.WeaponInUseEvent {TimeStamp=timeStamp; Weapon=j.InUse}
        | DamageJson j -> MatchRecordBuilder.DamageEvent {TimeStamp=timeStamp; Victim=j.TargetName; Shield=j.Armor; HeadShot=j.Headshot; Amount=int(j.DamageAmount)}
        | ScoreBoardJson j -> MatchRecordBuilder.ScoreBoardEvent {TimeStamp=timeStamp; RemainingTeams=j.Teams; RemainingPlayers=j.Players}
        | VictoryLine -> MatchRecordBuilder.VictoryEvent
        | MatchEndLine -> MatchRecordBuilder.MatchEndEvent {TimeStamp=timeStamp;}

    let updateParserState (parserState: LogParseStateHolder) (logLineJson: string) =
        match logLineJson with
        | l when l.Contains("""{"info":{"me":{"name":""") -> {parserState with PlayerName=(PlayerNameSetJson.Parse(l).Info.Me.Name)}
        |_ -> parserState

    let produceMatchEventFromLogLine (parserState:LogParseStateHolder) (logLine: string) =
        let splitLine = parseLogLine linePattern logLine
        let matchEvent = splitLine
                            |> Option.map (fun (t,n) -> (t, (categorizeJson parserState n)))
                            |> Option.filter (fun (_,n) -> n.IsSome)
                            |> Option.map (fun (timeStamp,matchEvent) -> mapCategorizedJsonToEvent parserState timeStamp matchEvent.Value)

        let updatedParserState = splitLine
                                     |> Option.map (fun (_,json) -> updateParserState parserState json)
                                     |> Option.defaultWith (fun () -> parserState)
        (matchEvent,updatedParserState)



    let produceMatchEventSetFromLogLines (logLine: string[]) =
        let initParserState = {PlayerName="UNKNOWN"};
        logLine
        |> Array.mapFold (produceMatchEventFromLogLine) initParserState
        |> (fun (results, state) -> results)
        |> Array.filter (fun n -> n.IsSome)
        |> Array.map (fun n -> n.Value)
