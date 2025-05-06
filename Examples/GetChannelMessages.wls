#!/usr/bin/env wolframscript
(* ::Package:: *)

PacletDirectoryLoad[Directory[]];


Get["KirillBelov`TelegramBot`TelethonClient`"];


client = TelethonClient[SystemCredential["TELEGRAM_CLIENT_API_ID"], SystemCredential["TELEGRAM_CLIENT_API_HASH"], SystemCredential["TELEGRAM_CLIENT_PHONE"], "session4"]


telethonConnect[client];


If[!telethonAuthorizedQ[client], 
    Pause[1];
    telethonSignIn[client, Input["Enter the code:"]]
];


messages = telethonGetMessages[client, "WolframMathematicaChannel"]