#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
PacletDirectoryLoad[Directory[]];
Get["KirillBelov`TelegramBot`TelethonClient`"];


client = TelethonClient[
	SystemCredential["TELEGRAM_CLIENT_API_ID"], 
	SystemCredential["TELEGRAM_CLIENT_API_HASH"], 
	SystemCredential["TELEGRAM_CLIENT_PHONE"], 
	"session2"
]; 


telethonConnect[client]


telethonSignIn[client, 26596]


msgs = telethonGetMessages[client, "dutchpm"]


Length @ msgs


DeleteDuplicates @ Flatten @ Map[Keys] @ msgs


getReactionsCount[___] := 0; 
getReactionsCount[assoc_?AssociationQ] := Total[assoc["results"][[All, "count"]]]


getForwardsCount[___] := 0; 
getForwardsCount[count_Integer] := count


channelStatistics[client_, channel_] := 
channelStatistics[client, channel] = 
Query[All, <|
	"date" -> "date" /* DateString, 
	"views" -> "views", 
	"reactions" -> "reactions" /* getReactionsCount, 
	"forwards" -> "forwards" /* getForwardsCount
|>] @ 
Select[KeyExistsQ[#, "views"]&] @  
telethonGetMessages[client, channel]


DateListPlot[
	Query[All, {"date" /* DateObject, "views"}] @ 
	channelStatistics[client, "WolframMathematicaChannel"]
]


DateListPlot[
	Query[All, {"date" /* DateObject, "views"}] @ 
	channelStatistics[client, "dutchpm"]
]


DateListPlot[
	Query[All, {"date" /* DateObject, "views"}] @ 
	channelStatistics[client, "propromochannel"]
]


propromochannel
