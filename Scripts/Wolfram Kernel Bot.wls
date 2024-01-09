#!/usr/bin/env wolframscript
(* ::Package:: *)


PacletInstall["KirillBelov/Objects"]; 
PacletInstall["KirillBelov/TelegramBot"]; 


Get["KirillBelov`TelegramBot`"]; 


messageWithCodeQ[update_] := 
	KeyExistsQ[update, "message"] && 
	KeyExistsQ[update["message"], "text"] && 
	KeyExistsQ[update["message"], "entities"] && 
	Length[Select[update["message", "entities"], 
	KeyExistsQ[#, "type"] && #["type"] == "code"&]] > 0 


messageCode[update_?messageWithCodeQ] := 
	Module[{text, entities, code}, 
		text = update["message", "text"]; 
		entities = update["message", "entities"]; 
		Table[StringTake[text,{e["offset"] + 1,e["offset"] + e["length"]}], {e, entities}]
	]; 


handleUpdate[bot_, update_?messageWithCodeQ] := 
	Module[{code, result}, 
		Print[update]; 
		code = messageCode[update]; 
		result = <|# -> TimeConstrained[ImportString[#, "WL"], 15, $Failed]& /@ code|>; 
		KeyValueMap[
			Print[Switch[#2, 
				_Graphics | _Graphics3D | Legended[Graphics[___], ___] | _Image, UsingFrontEnd[sendPhoto[bot, update[["message", "chat", "id"]], #2, "caption"->#1]], 
				_, sendMessage[bot, update[["message", "chat", "id"]], StringTemplate["<code>`1`</code>\n\n`2`"][#1, #2], "parseMode"->"html"]
			]]&, 
			result
		];
	] 


bot = TelegramBot["1247031088:AAFv0pJEPJWkrvuiy5frmjbgxff7vdpCruE"]; 
task = CreateBotSession[bot, handleUpdate, {1, 60 * 60 * 24 * 365}]; 
UsingFrontEnd[Developer`StartProtectedMode[]]; 
While[task["TaskStatus"] == "Running", Pause[0.1]]; 
