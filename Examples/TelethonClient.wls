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


telethonAuthorizedQ[client]


telethonSignIn[client, 26596]


msgs = telethonGetMessages[client, "propromochannel"]; 


ByteCount[msgs]


Length[msgs]


Length @ Select[#post&] @ msgs


toSchema[assoc_?AssociationQ] := Association[KeyValueMap[#1 -> toSchema[#2]&, assoc]]


toSchema[list_?ListQ] := DeleteDuplicates[Map[toSchema, list]]


toSchema[bool_?BooleanQ] := "Boolean"


toSchema[Null] := "Null"


toSchema[expr_] := ToString[Head[expr]]


Export["file.json", toSchema[msgs[[1]]], "RawJSON"]
