(* ::Package:: *)

(* ::Title:: *)
(*TelegramBot API*)


(* ::Section:: *)
(*Info*)


(* :Title     : TelegramBot API  *)
(* :Context   : TelegramBot`API` *)
(* :Version   : 0.1.1            *)
(* :Developer : Kirill Belov     *)


(* ::Section:: *)
(*Begin package*)


BeginPackage["TelegramBot`API`"]


(* ::Section:: *)
(*Clear names*)


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Public names declaration*)


TelegramBot::usage = 
"TelegramBot[token]"


getMe::usage = 
"getMe[bot]"


getUpdates::usage = 
"getUpdates[bot]"


setWebhook::usage = 
"setWebhook[bot, url]"


deleteWebhook::usage = 
"deleteWebhook[bot]"


getWebhookInfo::usage = 
"getWebhookInfo[bot]";


sendMessage::usage = 
"sendMessage[bot, chatId, text]"


(*forwardMessage::usage = 
"forwardMessage[bot, chatId, fromChatId, messageId]"*)


sendPhoto::usage = 
"sendPhoto[bot, chatId, photo]"


(*sendAudio::usage = 
"sendAudio[bot, chatId, audio]"*)


(*sendDocument::usage = 
"sendDocument[bot, chatId, document]"*)


(*sendVideo::usage = 
"sendVideo[bot, chatId, video]"*)


(*sendAnimation::usage = 
"sendAnimation[bot, chatId, animation]"*)


(*sendVoice::usage = 
"sendVoice[bot, chtId, voice]"*)


(*sendVideoNote::usage = 
"sendVideoNote[bot, chatId, videoNote]"*)


(*sendMediaGroup::usage = 
"sendMediaGroup[bot, chatId, {media}]"*)


(*sendLocation::usage = 
"sendLocation[bot, chatId, {latitute, longitude}]"*)


(*sendVenue::usage = 
"sendVenue[bot, chatId, title, address, {latitute, longitude}]"*)


(*sendContact::usage = 
"sendContact[bot, chatId, contact]"*)


(* ::Section:: *)
(*Begin private context*)


Begin["`Private`"]


(* ::Section:: *)
(*TelegramBot object*)


Options[TelegramBot] = 
	{"Evaluate" -> Evaluate, "Logger" -> Identity}


TelegramBot[bot_TelegramBot, opts: OptionsPattern[TelegramBot]] := 
	TelegramBot[token[bot], opts]


(* ::Section:: *)
(*Internal methods and variables*)


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
		log = OptionValue[TelegramBot, opts, "Logger"], 
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
			"FormData", "multipart/form-data"
		]; 

		request = HTTPRequest[url, <|
			Method -> httpMethod,
			"ContentType" -> contentType, 
			"Body" -> requestBody
		|>];
		
		response = evaluate[URLRead[request]];
		responseBody = response["Body"];
		responseJSON = ImportString[responseBody, "RawJSON"]; 
		
		log[requestParameters];
		log[responseJSON];
		
		Return[responseJSON]
	]


optionValues[function_Symbol, options: OptionsPattern[]] := 
	Table[o -> OptionValue[function, Flatten[{options}], o], {o, Keys[Options[function]]}]


optionNames[function_Symbol] := 
	("\"" <> # <> "\""& /@ Keys[Association[Options[function]]])


token[TelegramBot[token_String, ___]] := 
	token


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
	{
		"ArgumentsPattern" -> {_, OptionsPattern[]}, 
		"OptionNames" -> optionNames[getUpdates]
	}


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
	{
		"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
		"OptionNames" -> optionNames[setWebhook]
	}


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
	{
		"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
		"OptionNames" -> optionNames[sendMessage]
	}


TelegramBot /: 
sendMessage[bot_TelegramBot, 
chatID: _String | _Integer, text_String, 
options: OptionsPattern[sendMessage]] := 
	telegramBotExecute[
		bot, 
		"sendMessage", 
		{
			"chat_id" -> chatID, 
			"text" -> text
		}, 
		optionValues[sendMessage, options]
	]


(* ::Subsubsection:: *)
(*sendPhoto*)


Options[sendPhoto] = 
	{
		"caption" -> Automatic, 
		"parse_mode" -> Automatic, 
		"disable_notification" -> Automatic, 
		"reply_to_message_id" -> Automatic,
		"reply_markup" -> Automatic
	}


SyntaxInformation[sendPhoto] = 
	{
		"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
		"OptionNames" -> optionNames[sendPhoto]
	}


TelegramBot /: 
sendPhoto[bot_TelegramBot, chatId: _String | _Integer, 
photo: _String | _Graphics | _Image, options: OptionsPattern[sendPhoto]] := 
	telegramBotExecute[
		bot, 
		"sendPhoto", 
		{
			"chat_id" -> chatId, 
			"photo" -> Switch[photo, 
				_String, photo, 
				_Image | _Graphics, 
					File[Export[FileNameJoin[{$TemporaryDirectory, "telegram-bot-photo.png"}], photo, "PNG"]]
			]
		}, 
		optionValues[sendPhoto, options], 
		"FormData"
	]


(* ::Section:: *)
(*End private contex*)


End[]


(* ::Section:: *)
(*From change protection*)


Protect["`*"]


(* ::Section:: *)
(*End package*)


EndPackage[] (*TelegramBot``*)
