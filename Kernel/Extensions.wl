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
"ImportTelegramFile[bot, fileId] import file"


ClearAll[HandleBotUpdates]


HandleBotUpdates::usage = 
"HandleBotUpdates[bot] handle updates using default updateHandler
HandleBotUpdates[bot, updateHandler] handle updates using specific updateHandler"


ClearAll[DeployWebhook]


DeployWebhook::usage = 
"DeployWebhook[bot] deploy webhook with default update handler and automatic generated webhook url using hash and bot token
DeployWebhook[bot, webhook] deploy webhook with default update handler
DeployWebhook[bot, updateHandler, webhook]  deploy webhook with specific update handler"


ClearAll[CreateLongPollBot]


CreateLongPollBot::usage = 
"CreateLongPollBot[bot]
CreateLongPollBot[]"


(* ::Section:: *)
(*Private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Import files*)


ImportTelegramFile[bot: KirillBelov`TelegramBot`TelegramBot[_Symbol?AssociationQ], fileId_String] := 
Module[{filePath, url}, 
	filePath = KirillBelov`TelegramBot`getFile[bot, fileId]["result", "file_path"]; 
	url = StringTemplate["https://api.telegram.org/file/bot`1`/`2`"][bot["Token"], filePath]; 
	Import[url]
]


(* ::Section:: *)
(*Handle all updates*)


HandleBotUpdates[bot: KirillBelov`TelegramBot`TelegramBot[assoc_Symbol?AssociationQ], updateHandler_] := 
Module[{updates}, 
	updates = KirillBelov`TelegramBot`getUpdates[bot]["result"]; 
	If[updates != {}, 
		Do[updateHandler[bot, update], {update, updates}]; 
		KirillBelov`TelegramBot`getUpdates[bot, "offset" -> updates[[-1, "update_id"]] + 1]
	]
]


HandleBotUpdates[bot: KirillBelov`TelegramBot`TelegramBot[assoc_Symbol?AssociationQ]] /; 
MatchQ[bot["UpdateHandler"], Except[Missing[_]]] := 
HandleBotUpdates[bot, bot["UpdateHandler"]]


(* ::Section:: *)
(*Create in Session Bot*)


CreateLongPollBot[bot_Telegram, updateHandler: _Symbol | _Function, timespec_List] := 
Module[{task}, 
	KirillBelov`TelegramBot`deleteWebhook[bot];
	task = SessionSubmit[HandleBotUpdates[bot, updateHandler], timespec]; 
	bot["Task"] = task
]


CreateLongPollBot[bot_Telegram, updateHandler: _Symbol | _Function] := 
Module[{timespec}, 
	timespec = bot["Timespec"]; 
	CreateLongPollBot[bot, updateHandler, timespec]
]


CreateLongPollBot[bot_Telegram, timespec_List] := 
Module[{updateHandler}, 
	updateHandler = bot["UpdateHandler"]; 
	CreateLongPollBot[bot, updateHandler, timespec]
]


CreateLongPollBot[bot_Telegram] := 
Module[{updateHandler, timespec}, 
	timespec = bot["Timespec"]; 
	updateHandler = bot["UpdateHandler"]; 
	CreateLongPollBot[bot, updateHandler, timespec]
]


(* ::Section:: *)
(*Deploy webhook*)


DeployWebhook[bot: KirillBelov`TelegramBot`TelegramBot[assoc_Symbol?AssociationQ], 
	updateHandler: _Symbol | _Function, webhook_String] := 
Module[{apiFunction, webhookUrl}, 
	apiFunction = APIFunction[{}, updateHandler[bot, ImportString[HTTPRequestData["Body"], "RawJSON"]]&]; 
	webhookUrl = CloudDeploy[apiFunction, webhook, Permissions -> "Public"]; 
	KirillBelov`TelegramBot`setWebhook[bot, webhookUrl]; 
	bot["Webhook"] = webhookUrl
]


DeployWebhook[bot: KirillBelov`TelegramBot`TelegramBot[assoc_Symbol?AssociationQ], 
	updateHandler: _Symbol | _Function] := 
Module[{
	webhook = Hash[bot["Token"], "SHA", "HexString"]
}, 
	DeployWebhook[bot, updateHandler, webhook]
]


DeployWebhook[bot: KirillBelov`TelegramBot`TelegramBot[assoc_Symbol?AssociationQ], webhook_String] := 
Module[{
	updateHandler = bot["UpdateHandler"]
}, 
	DeployWebhook[bot, updateHandler, webhook]
]


DeployWebhook[bot: KirillBelov`TelegramBot`TelegramBot[assoc_Symbol?AssociationQ]] := 
Module[{
	updateHandler = bot["UpdateHandler"], 
	webhook = Hash[bot["Token"], "SHA", "HexString"]
}, 
	DeployWebhook[bot, updateHandler, webhook]
]


(* ::Section:: *)
(*End private*)


End[]


(* ::Section:: *)
(*End package*)


EndPackage[]
