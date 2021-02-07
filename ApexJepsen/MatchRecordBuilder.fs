module MatchRecordBuilder
    open System

    type MatchStartEvent = {TimeStamp:DateTime; MatchIdentifier:Guid; LocalPlayer:string}
    type KillFeedEvent = {TimeStamp:DateTime; VictimIsLocalPlayer: bool; ForeignPlayerName:string; WeaponName: string; ActionType: string;}
    type MatchEndEvent = {TimeStamp:DateTime}
    type LocationEvent = {TimeStamp:DateTime; X:int; Y:int; Z:int}
    type MatchRecordEvent = MatchStartEvent of MatchStartEvent 
                            | MatchEndEvent of MatchEndEvent 
                            | KillFeedEvent of KillFeedEvent
                            | LocationEvent of LocationEvent

    type InProgressMatchRecord = {LocationEvents: LocationEvent[]; 
                                    KillFeedEvents: KillFeedEvent[];
                                    MatchIdentifier: Guid; 
                                    LocalPlayer: string; 
                                    StartTime: DateTime}
    type CompleteMatchRecord = {LocationEvents:LocationEvent[]; 
                                KillFeedEvent:KillFeedEvent[];
                                MatchIdentifier:Guid; 
                                LocalPlayer:string; 
                                StartTime: DateTime; 
                                EndTime: DateTime}
    type AddMatchEventOutput = InProgressMatchRecord of InProgressMatchRecord 
                                | CompleteMatchRecord of CompleteMatchRecord



    let addMatchRecordEvent (activeMatchRecord: Option<InProgressMatchRecord>) (matchEvent: MatchRecordEvent) =

        let addEventToActiveMatch (inProgressMatchRecord:InProgressMatchRecord) (matchEvent: MatchRecordEvent) : AddMatchEventOutput =
            match matchEvent with
            | MatchEndEvent {TimeStamp=t} -> CompleteMatchRecord {LocationEvents=inProgressMatchRecord.LocationEvents; 
                                KillFeedEvent=inProgressMatchRecord.KillFeedEvents;
                                MatchIdentifier=inProgressMatchRecord.MatchIdentifier;
                                LocalPlayer=inProgressMatchRecord.LocalPlayer;
                                StartTime=inProgressMatchRecord.StartTime;
                                EndTime=t}
            | LocationEvent l -> InProgressMatchRecord { inProgressMatchRecord with LocationEvents= (Array.append inProgressMatchRecord.LocationEvents (Array.singleton l)) }
            | KillFeedEvent k -> InProgressMatchRecord { inProgressMatchRecord with KillFeedEvents=(Array.append inProgressMatchRecord.KillFeedEvents (Array.singleton k))}
            | MatchStartEvent _ -> raise (new ArgumentException("Invalid event for active match"))

        let createNewEvent (matchEvent: MatchRecordEvent) : AddMatchEventOutput =
            match matchEvent with
            | MatchStartEvent m -> InProgressMatchRecord{LocationEvents=Array.empty;
                                                        KillFeedEvents=Array.empty;
                                                        MatchIdentifier=m.MatchIdentifier;
                                                        LocalPlayer=m.LocalPlayer;
                                                        StartTime=m.TimeStamp
                                                        }
            | _ -> raise (new ArgumentException("Expected a match start event"))

        match activeMatchRecord with
        | Some record -> addEventToActiveMatch record matchEvent
        | None ->  createNewEvent matchEvent

    let stateFolder (recordState: AddMatchEventOutput []) (newEvent: MatchRecordEvent) =
        let mostRecentRecordState = recordState |> Array.tryLast
        let newState = match mostRecentRecordState with
                            | Option.None -> addMatchRecordEvent Option.None newEvent
                            | Option.Some (AddMatchEventOutput.InProgressMatchRecord n) -> addMatchRecordEvent (Option.Some n) newEvent
                            | Option.Some (AddMatchEventOutput.CompleteMatchRecord n) -> addMatchRecordEvent Option.None newEvent
        Array.append recordState (Array.singleton newState)

