(* ::Package:: *)

(* ::Title:: *)
(*TelegramBot*)


(* ::Section:: *)
(*Info*)


(* :Title: TelegramBot *)
(* :Context: TelegramBot` *)
(* :Version: 0.1.0 *)


(* ::Section:: *)
(*Begin package*)


BeginPackage["TelegramBot`"]


(* ::Section:: *)
(*Clear names*)


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Public names declaration*)


TelegramBot::usage = 
"TelegramBot[token, opts]"


getMe::usage = 
"getMe[bot]"


getUpdates::usage = 
"getUpdates[bot]
getUpdates[bot, options]"


setWebhook::usage = 
"setWebhook[bot, url]
setWebhook[bot, url, options]"


deleteWebhook::usage = 
"deleteWebhook[bot]"


getWebhookInfo::usage = 
"getWebhookInfo[bot]";


sendMessage::usage = 
"sendMessage[bot, chatID, text]
sendMessage[bot, chatID, text, options]"


sendPhoto::usage = 
"sendPhoto[bot, chatID, image]"


(* ::Section:: *)
(*Begin private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Internal methods and variables*)


Options[TelegramBot] = 
	{"Evaluate" -> Evaluate}


$telegramBotAPI = 
	"https://api.telegram.org"


telegramBotExecute[
	TelegramBot[token_String, opts: OptionsPattern[TelegramBot]], 
	method_String, 
	parameters: {(_String -> _)...}: {}, 
	options: {(_String -> _)...}: {}, 
	type: "Query" | "URLEncoded" | "JSON" | "FormData" : "JSON"
] := 
	Module[{
		evaluate = OptionValue[TelegramBot, opts, "Evaluate"], 
		url, httpMethod, contentType, request, requestParameters, requestBody, 
		response, responseBody, responseJSON
	}, 

		requestParameters = DeleteCases[Join[parameters, options], _[_String, Automatic]]; 
		url = If[type === "Query", 
			URLBuild[{$telegramBotAPI, "bot" <> token, method}, requestParameters], 
			URLBuild[{$telegramBotAPI, "bot" <> token, method}]
		]; 
		httpMethod = If[type === "Query", "GET", "POST"];
		requestBody = Switch[type, 
			"Query", "", 
			"URLEncoded", URLBuild[{}, requestParameters], 
			"JSON", ImportString[ExportString[requestParameters, "JSON"], "Text"], 
			"FormData", requestParameters
		];
		contentType = Switch[type, 
			"Query", None, 
			"URLEncoded", "application/x-www-form-urlencoded; charset=utf-8", 
			"JSON", "application/json; charset=utf-8", 
			"FormData", "multipart/form-data; charset=utf-8"
		]; 

		If[type =!= "FormData", 
			request = HTTPRequest[url, <|
				Method -> httpMethod, 
				"Body" -> requestBody, 
				"ContentType" -> contentType
			|>], 
			request = HTTPRequest[url, <|
				Method -> httpMethod, 
				"Body" -> requestBody
			|>]
		];
		
		response = evaluate[URLRead[request]];
		responseBody = response["Body"];
		responseJSON = ImportString[responseBody, "RawJSON"]; 

		Return[responseJSON]
	]


optionValues[function_Symbol, options: OptionsPattern[]] := 
	Table[o -> OptionValue[function, Flatten[{options}], o], {o, Keys[Options[function]]}]


(* ::Section:: *)
(*TelegramBot methods*)


(* ::Subsubsection:: *)
(*getMe*)


SyntaxInformation[getMe] = 
	{"ArgumentsPattern" -> {_}}


TelegramBot /: getMe[bot_TelegramBot] := 
	telegramBotExecute[bot, "getMe"]


(* ::Subsubsection:: *)
(*getUpdates*)


Options[getUpdates] = 
	{
		"offset" -> Automatic, 
		"limit" -> Automatic, 
		"timeout" -> Automatic, 
		"allowed_updates" -> Automatic
	}


SyntaxInformation[getUpdates] = 
	{"ArgumentsPattern" -> {_, OptionsPattern[]}}


TelegramBot /: getUpdates[bot_TelegramBot, options: OptionsPattern[getUpdates]] := 
	telegramBotExecute[bot, "getUpdates", optionValues[getUpdates, options]]


(* ::Subsubsection:: *)
(*setWebhook*)


Options[setWebhook] = 
	{
		"certificate" -> Automatic, 
		"max_connections" -> Automatic, 
		"allowed_updates" -> Automatic
	}


SyntaxInformation[setWebhook] = 
	{"ArgumentsPattern" -> {_, _, OptionsPattern[]}}


TelegramBot /: setWebhook[bot_TelegramBot, url_String, options: OptionsPattern[setWebhook]] := 
telegramBotExecute[bot, "setWebhook", {"url" -> url}, optionValues[setWebhook, options]]


(* ::Subsubsection:: *)
(*deleteWebhook*)


SyntaxInformation[deleteWebhook] = 
	{"ArgumentsPattern" -> {_}}


TelegramBot /: deleteWebhook[bot_TelegramBot] := 
	telegramBotExecute[bot, "deleteWebhook"]


(* ::Subsubsection:: *)
(*getWebhookInfo*)


SyntaxInformation[getWebhookInfo] = 
	{"ArgumentsPattern" -> {_}}


TelegramBot /: getWebhookInfo[bot_TelegramBot] := 
	telegramBotExecute[bot, "getWebhookInfo"]


(* ::Subsubsection:: *)
(*sendMessage*)


Options[sendMessage] = 
	{
		"parse_mode" -> Automatic, 
		"disable_web_page_preview" -> Automatic, 
		"disable_notification" -> Automatic, 
		"reply_to_message_id" -> Automatic,
		"reply_markup" -> Automatic
	}


SyntaxInformation[sendMessage] = 
	{"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}}


TelegramBot /: sendMessage[bot_TelegramBot, chatID: _String | _Integer, text_String, options: OptionsPattern[sendMessage]] := 
	telegramBotExecute[bot, "sendMessage", {"chat_id" -> chatID, "text" -> text}, optionValues[sendMessage, options]]


(* ::Subsubsection:: *)
(*sendPhoto*)


sendPhoto[bot_TelegramBot, chatID: _String | _Integer, file_String?FileExistsQ] := 
	telegramBotExecute[bot, "sendPhoto", {"chat_id" -> chatID, "photo" -> File[file]}, {}, "FormData"]


(* ::Section:: *)
(*End private contex*)


End[]


(* ::Section:: *)
(*From change protection*)


Protect["`*"]


(* ::Section:: *)
(*End package*)


EndPackage[] (*TelegramBot``*)
