#!/usr/bin/env wolframscript
(* ::Package:: *)

PacletInstall["KirillBelov/Objects"]; 
PacletInstall["KirillBelov/TelegramBot"]; 


<<KirillBelov`TelegramBot`


bot = TelegramBot["6943496663:AAGEOOoGJRIkOGFEKQSO7SUIfk3Ddu_5uoE"]; 


vkWallGet[domain_] := 
ImportString[ExportString[URLRead[HTTPRequest[URLBuild[
	{"https://api.vk.com/method/wall.get"}, 
	{
		"v" -> "5.199", 
		"domain" -> domain, 
		"filter" -> "owner"
	}
	], 
	<|"Headers" -> {
		"Authorization" -> "Bearer ebdaf7d7ebdaf7d7ebdaf7d74febb1850eeebdaebdaf7d7b6c199b357c49ff773694270"
	}|>
]]["Body"], "Text"], "RawJSON"]["response", "items"]


$channel = -1002059073537; 


LocalSymbol["sentPosts"] = If[!ListQ[LocalSymbol["sentPosts"]], {}, LocalSymbol["sentPosts"]]; 


sendPostToTG[post_Association] := 
If[!MemberQ[LocalSymbol["sentPosts"], post["id"]], 
Pause[1]; 
If[sendMessage[bot, $channel, post["text"] <> If[KeyExistsQ[post, "copy_history"], "\n\n" <> post[["copy_history", 1, "text"]], ""], "parseMode" -> "html"]["ok"], 
LocalSymbol["sentPosts"] = Append[LocalSymbol["sentPosts"], post["id"]]; 
]; 
If[
Length[post["attachments"]] > 1, 
If[sendMediaGroup[bot, $channel, Select[post[["attachments"]], #type === "photo"&][[All, "photo", "sizes", -1, "url"]]]["ok"], 
LocalSymbol["sentPosts"] = Append[LocalSymbol["sentPosts"], post["id"]]; 
]
]; 
If[
Length[KeyExistsQ[post, "copy_history"] && post[["copy_history", 1, "attachments"]]] > 1, 
If[sendMediaGroup[bot, $channel, post[["copy_history", 1, "attachments", All, "photo", "sizes", -1, "url"]]]["ok"], 
LocalSymbol["sentPosts"] = Append[LocalSymbol["sentPosts"], post["id"]]; 
]
]; 
]


task = SessionSubmit[ScheduledTask[Table[Pause[1]; sendPostToTG[post], {post, Echo@Reverse@vkWallGet["wolframmathematica"][[ ;; 5]]}], {60, 1000000}]]; 


If[Head@$FrontEnd =!= FrontEndObject, 
	While[task["TaskStatus"] == "Running", Pause[0.1]]; 
]
