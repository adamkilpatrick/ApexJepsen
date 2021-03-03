module MatchRecordBuilder
    open System

    type MatchStartEvent = {TimeStamp:DateTime; MatchIdentifier:Guid; LocalPlayer:string}
    type KillFeedEvent = {TimeStamp:DateTime; VictimIsLocalPlayer: bool; ForeignPlayerName:string; WeaponName: string; ActionType: string;}
    type MatchEndEvent = {TimeStamp:DateTime}
    type LocationEvent = {TimeStamp:DateTime; X:int; Y:int; Z:int}
    type WeaponInUseEvent = {TimeStamp: DateTime; Weapon: string}
    type DamageEvent = {TimeStamp: DateTime; Victim:string; Amount:int; Shield:bool; HeadShot:bool}
    type ScoreBoardEvent = {TimeStamp: DateTime; RemainingTeams:int; RemainingPlayers: Option<int>}

    type MatchRecordEvent = MatchStartEvent of MatchStartEvent 
                            | MatchEndEvent of MatchEndEvent 
                            | VictoryEvent
                            | KillFeedEvent of KillFeedEvent
                            | WeaponInUseEvent of WeaponInUseEvent
                            | DamageEvent of DamageEvent
                            | ScoreBoardEvent of ScoreBoardEvent
                            | LocationEvent of LocationEvent

    type InProgressMatchRecord = {LocationEvents: LocationEvent[]; 
                                    KillFeedEvents: KillFeedEvent[];
                                    DamageEvents: DamageEvent[];
                                    WeaponInUseEvents:WeaponInUseEvent[];
                                    ScoreBoardEvents: ScoreBoardEvent[];
                                    MatchIdentifier: Guid; 
                                    LocalPlayer: string; 
                                    Victory:bool;
                                    StartTime: DateTime;
                                    MapType:MapEvaluator.MapType}
    type CompleteMatchRecord = {LocationEvents:LocationEvent[]; 
                                KillFeedEvents:KillFeedEvent[];
                                WeaponInUseEvents:WeaponInUseEvent[];
                                DamageEvents: DamageEvent[];
                                ScoreBoardEvents: ScoreBoardEvent[];
                                MatchIdentifier:Guid; 
                                LocalPlayer:string; 
                                Victory:bool;
                                StartTime: DateTime;
                                MapType:MapEvaluator.MapType;
                                EndTime: DateTime}
    type AddMatchEventOutput = InProgressMatchRecord of InProgressMatchRecord 
                                | CompleteMatchRecord of CompleteMatchRecord



    let addMatchRecordEvent (activeMatchRecord: Option<InProgressMatchRecord>) (matchEvent: MatchRecordEvent) =

        let addEventToActiveMatch (inProgressMatchRecord:InProgressMatchRecord) (matchEvent: MatchRecordEvent) : AddMatchEventOutput =
            match matchEvent with
            | MatchEndEvent {TimeStamp=t} -> CompleteMatchRecord {LocationEvents=inProgressMatchRecord.LocationEvents; 
                                KillFeedEvents=inProgressMatchRecord.KillFeedEvents;
                                DamageEvents=inProgressMatchRecord.DamageEvents;
                                MatchIdentifier=inProgressMatchRecord.MatchIdentifier;
                                WeaponInUseEvents=inProgressMatchRecord.WeaponInUseEvents;
                                ScoreBoardEvents=inProgressMatchRecord.ScoreBoardEvents;
                                LocalPlayer=inProgressMatchRecord.LocalPlayer;
                                Victory=inProgressMatchRecord.Victory;
                                StartTime=inProgressMatchRecord.StartTime;
                                MapType=inProgressMatchRecord.MapType;
                                EndTime=t}
            | LocationEvent l -> InProgressMatchRecord { inProgressMatchRecord with LocationEvents= (Array.append inProgressMatchRecord.LocationEvents (Array.singleton l)) }
            | KillFeedEvent k -> InProgressMatchRecord { inProgressMatchRecord with KillFeedEvents=(Array.append inProgressMatchRecord.KillFeedEvents (Array.singleton k))}
            | WeaponInUseEvent w -> InProgressMatchRecord { inProgressMatchRecord with WeaponInUseEvents=(Array.append inProgressMatchRecord.WeaponInUseEvents (Array.singleton w))}
            | ScoreBoardEvent s -> InProgressMatchRecord { inProgressMatchRecord with ScoreBoardEvents=(Array.append inProgressMatchRecord.ScoreBoardEvents (Array.singleton s))}
            | DamageEvent d -> InProgressMatchRecord { inProgressMatchRecord with DamageEvents=(Array.append inProgressMatchRecord.DamageEvents (Array.singleton d))}
            | VictoryEvent -> InProgressMatchRecord { inProgressMatchRecord with Victory=true}
            | MatchStartEvent _ -> raise (new ArgumentException("Invalid event for active match"))

        let createNewEvent (matchEvent: MatchRecordEvent) : AddMatchEventOutput =
            match matchEvent with
            | MatchStartEvent m -> InProgressMatchRecord{LocationEvents=Array.empty;
                                                        KillFeedEvents=Array.empty;
                                                        WeaponInUseEvents=Array.empty;
                                                        DamageEvents=Array.empty;
                                                        ScoreBoardEvents=Array.empty;
                                                        MatchIdentifier=m.MatchIdentifier;
                                                        LocalPlayer=m.LocalPlayer;
                                                        Victory=false;
                                                        StartTime=m.TimeStamp;
                                                        MapType=MapEvaluator.getMapFromTime m.TimeStamp
                                                        }
            | _ -> raise (new ArgumentException("Expected a match start event"))

        match activeMatchRecord with
        | Some record -> addEventToActiveMatch record matchEvent
        | None ->  createNewEvent matchEvent

    let stateFolder (recordState: AddMatchEventOutput []) (newEvent: MatchRecordEvent) =
        let mostRecentRecordState = recordState |> Array.tryLast
        try
            let newState = match mostRecentRecordState with
                                | Option.None -> addMatchRecordEvent Option.None newEvent
                                | Option.Some (AddMatchEventOutput.InProgressMatchRecord n) -> addMatchRecordEvent (Option.Some n) newEvent
                                | Option.Some (AddMatchEventOutput.CompleteMatchRecord n) -> addMatchRecordEvent Option.None newEvent
            Array.append recordState (Array.singleton newState)
        with
        | :? (ArgumentException) as e -> recordState

