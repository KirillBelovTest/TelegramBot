(* ::Package:: *)

(* ::Chapter:: *)
(*Telegram Bot Extensions*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`TelegramBot`Extensions`", {
	"KirillBelov`TelegramBot`Type`", 
	"KirillBelov`TelegramBot`API`"
}]; 


(* ::Section:: *)
(*Clear names*)


ClearAll["`*"]; 


(* ::Section:: *)
(*Names*)


ImportTelegramFile::usage = 
"ImportTelegramFile[bot, fileId] bot imports from telegram file by fileId."; 


HandleBotUpdates::usage = 
"HandleBotUpdates[bot] handle bot updates. 
HandleBotUpdates[bot, updateHandler] handle bot updates using specific updateHandler."; 


DeployBotWebhook::usage = 
"DeployBotWebhook[bot] deploy bot webhook. 
DeployBotWebhook[bot, webhookName] deploy bot webhook with specific webhookName. 
DeployBotWebhook[bot, updateHandler] deploy bot webhook with specific updateHandler. 
DeployBotWebhook[bot, updateHandler, webhookName] deploy bot webhook with specific updateHandler and webhookName."; 


CreateBotSession::usage = 
"CreateBotSession[bot] session bot. 
CreateBotSession[bot, timespec] session bot with specific timespec. 
CreateBotSession[bot, updateHandler] session bot with specific updateHandler. 
CreateBotSession[bot, updateHandler, timespec] session bot with specific updateHandler and timespec."; 


(* ::Section:: *)
(*Private context*)


Begin["`Private`"]; 


(* ::Section:: *)
(*Implementation*)


TelegramBot /: ImportTelegramFile[bot_TelegramBot, fileId_String] := 
Module[{filePath, url}, 
	filePath = getFile[bot, fileId]["result", "file_path"]; 
	url = StringTemplate["https://api.telegram.org/file/bot`1`/`2`"][bot["Token"], filePath]; 
	Import[url]
]; 


TelegramBot /: HandleBotUpdates[bot_TelegramBot, updateHandler_] := 
Module[{updates}, 
	updates = getUpdates[bot]["result"]; 
	If[updates != {}, 
		Do[updateHandler[bot, update], {update, updates}]; 
		getUpdates[bot, "offset" -> updates[[-1, "update_id"]] + 1]
	]
]; 


TelegramBot /: HandleBotUpdates[bot_TelegramBot] /; 
MatchQ[bot["UpdateHandler"], Except[Missing[_]]] := 
HandleBotUpdates[bot, bot["UpdateHandler"]]; 


TelegramBot /: CreateBotSession[bot_TelegramBot, updateHandler: _Symbol | _Function, timespec_List] := (
	deleteWebhook[bot]; 
	With[{task = SessionSubmit[ScheduledTask[HandleBotUpdates[bot, updateHandler], timespec]]}, 
		task
	]
); 


TelegramBot /: CreateBotSession[bot_TelegramBot, updateHandler: _Symbol | _Function] := 
Module[{timespec}, 
	timespec = bot["Timespec"]; 
	CreateBotSession[bot, updateHandler, timespec]
]; 


TelegramBot /: CreateBotSession[bot_TelegramBot, timespec_List] := 
Module[{updateHandler}, 
	updateHandler = bot["UpdateHandler"]; 
	CreateBotSession[bot, updateHandler, timespec]
]; 


TelegramBot /: CreateBotSession[bot_TelegramBot] := 
Module[{updateHandler, timespec}, 
	timespec = bot["Timespec"]; 
	updateHandler = bot["UpdateHandler"]; 
	CreateBotSession[bot, updateHandler, timespec]
]; 


TelegramBot /: DeployBotWebhook[bot_TelegramBot, updateHandler: _Symbol | _Function, webhookName_String] := 
Module[{apiFunction, webhook}, 
	apiFunction = APIFunction[{}, updateHandler[bot, ImportString[HTTPRequestData["Body"], "RawJSON"]]&]; 
	webhook = CloudDeploy[apiFunction, webhookName, Permissions -> "Public"]; 
	setWebhook[bot, webhook[[1]]]; 
	bot["Webhook"] = webhook
]; 


TelegramBot /: DeployBotWebhook[bot_TelegramBot, updateHandler: _Symbol | _Function] := 
Module[{webhook = Hash[bot["Token"], "SHA", "HexString"]}, 
	DeployBotWebhook[bot, updateHandler, webhook]
]; 


TelegramBot /: DeployBotWebhook[bot_TelegramBot, webhookName_String] := 
Module[{updateHandler = bot["UpdateHandler"]}, 
	DeployBotWebhook[bot, updateHandler, webhookName]
]; 


TelegramBot /: DeployBotWebhook[bot_TelegramBot] := 
Module[{updateHandler = bot["UpdateHandler"], webhook = Hash[bot["Token"], "SHA", "HexString"]}, 
	DeployBotWebhook[bot, updateHandler, webhook]
]; 


(* ::Section:: *)
(*End private*)


End[]; 


(* ::Section:: *)
(*End package*)


EndPackage[]; 
