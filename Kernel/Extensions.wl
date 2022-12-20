(* ::Package:: *)

(* ::Chapter:: *)
(*Telegram Bot Extensions*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`TelegramBot`"]


(* ::Section:: *)
(*Public functions*)


ClearAll[ImportTelegramFile]


ImportTelegramFile::usage = 
"ImportTelegramFile[bot, fileId] bot imports from telegram file by fileId"


ClearAll[HandleBotUpdates]


HandleBotUpdates::usage = 
"HandleBotUpdates[bot] handle bot updates
HandleBotUpdates[bot, updateHandler] handle bot updates using specific updateHandler"


ClearAll[DeployBotWebhook]


DeployBotWebhook::usage = 
"DeployBotWebhook[bot] deploy bot webhook
DeployBotWebhook[bot, webhookName] deploy bot webhook with specific webhookName
DeployBotWebhook[bot, updateHandler] deploy bot webhook with specific updateHandler
DeployBotWebhook[bot, updateHandler, webhookName] deploy bot webhook with specific updateHandler and webhookName"


ClearAll[CreateBotSession]


CreateBotSession::usage = 
"CreateBotSession[bot] session bot
CreateBotSession[bot, timespec] session bot with specific timespec
CreateBotSession[bot, updateHandler] session bot with specific updateHandler
CreateBotSession[bot, updateHandler, timespec] session bot with specific updateHandler and timespec"


(* ::Section:: *)
(*Private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Patterns*)


botPattern[] := 
KirillBelov`TelegramBot`TelegramBot[assoc_Symbol?AssociationQ]


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Import files*)


ImportTelegramFile[bot: botPattern[], fileId_String] := 
Module[{filePath, url}, 
	filePath = KirillBelov`TelegramBot`getFile[bot, fileId]["result", "file_path"]; 
	url = StringTemplate["https://api.telegram.org/file/bot`1`/`2`"][bot["Token"], filePath]; 
	Import[url]
]


(* ::Subsection:: *)
(*Handle bot updates*)


HandleBotUpdates[bot: botPattern[], updateHandler_] := 
Module[{updates}, 
	updates = KirillBelov`TelegramBot`getUpdates[bot]["result"]; 
	If[updates != {}, 
		Do[updateHandler[bot, update], {update, updates}]; 
		KirillBelov`TelegramBot`getUpdates[bot, "offset" -> updates[[-1, "update_id"]] + 1]
	]
]


HandleBotUpdates[bot: botPattern[]] /; 
MatchQ[bot["UpdateHandler"], Except[Missing[_]]] := 
HandleBotUpdates[bot, bot["UpdateHandler"]]


(* ::Subsection:: *)
(*Create in Session Bot*)


CreateBotSession[bot: botPattern[], updateHandler: _Symbol | _Function, timespec_List] := 
Module[{task}, 
	KirillBelov`TelegramBot`deleteWebhook[bot];
	Hold[task = SessionSubmit[ScheduledTask[HandleBotUpdates[bot, updateHandler], timespec]]]
]


CreateBotSession[bot: botPattern[], updateHandler: _Symbol | _Function] := 
Module[{timespec}, 
	timespec = bot["Timespec"]; 
	CreateBotSession[bot, updateHandler, timespec]
]


CreateBotSession[bot: botPattern[], timespec_List] := 
Module[{updateHandler}, 
	updateHandler = bot["UpdateHandler"]; 
	CreateBotSession[bot, updateHandler, timespec]
]


CreateBotSession[bot: botPattern[]] := 
Module[{updateHandler, timespec}, 
	timespec = bot["Timespec"]; 
	updateHandler = bot["UpdateHandler"]; 
	CreateBotSession[bot, updateHandler, timespec]
]


(* ::Subsection:: *)
(*Deploy bot webhook*)


DeployBotWebhook[bot: botPattern[], 
	updateHandler: _Symbol | _Function, webhookName_String] := 
Module[{apiFunction, webhook}, 
	apiFunction = APIFunction[{}, updateHandler[bot, ImportString[HTTPRequestData["Body"], "RawJSON"]]&]; 
	webhook = CloudDeploy[apiFunction, webhookName, Permissions -> "Public"]; 
	KirillBelov`TelegramBot`setWebhook[bot, webhook]; 
	bot["Webhook"] = webhook
]


DeployBotWebhook[bot: botPattern[], 
	updateHandler: _Symbol | _Function] := 
Module[{
	webhook = Hash[bot["Token"], "SHA", "HexString"]
}, 
	DeployBotWebhook[bot, updateHandler, webhook]
]


DeployBotWebhook[bot: botPattern[], webhookName_String] := 
Module[{
	updateHandler = bot["UpdateHandler"]
}, 
	DeployBotWebhook[bot, updateHandler, webhookName]
]


DeployBotWebhook[bot: botPattern[]] := 
Module[{
	updateHandler = bot["UpdateHandler"], 
	webhook = Hash[bot["Token"], "SHA", "HexString"]
}, 
	DeployBotWebhook[bot, updateHandler, webhook]
]


(* ::Section:: *)
(*End private*)


End[]


(* ::Section:: *)
(*End package*)


EndPackage[]
