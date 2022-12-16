(* ::Package:: *)

(* ::Chapter:: *)
(*Telegram Bot API*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`TelegramBot`"]


(* ::Section:: *)
(*Endpoints*)


getMe::usage = 
"getMe[bot]
bot@getMe[]"


getUpdates::usage = 
"getUpdates[bot]
bot@getUpdates[]"


setWebhook::usage = 
"setWebhook[bot, url]
bot@setWebhook[url]"


deleteWebhook::usage = 
"deleteWebhook[bot]
bot@deleteWebhook[]"


getWebhookInfo::usage = 
"getWebhookInfo[bot]
bot@getWebhookInfo[]";


sendMessage::usage = 
"sendMessage[bot, chatId, text]
bot@sendMessage[chatId, text]"


getUserProfilePhotos::usage = 
"getUserProfilePhotos[bot, userId]
bot@getUserProfilePhotos[bot, userId]"


getFile::usage = 
"getFile[bot, fileId]
bot@getFile[fileId]"


(* ::Section::Closed:: *)
(*Private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Internal*)


optionNames[symbols: {__Symbol}] := 
Flatten[Function[symbol, Map["\"" <> # <> "\""&, Keys[Options[symbol]]], Listable][symbols]]


deserialize[body_String] := 
ImportString[body, "RawJSON"]


encode[str_String] := 
str


encode[url_URL] := 
url[[1]]


encode[expr_] := 
expr


Options[exec] = {
	"Endpoint" -> "https://api.telegram.org", 
	"Form" -> "JSON", 
	"Encoder" -> encode, 
	"Deserializer" -> deserialize
}


exec[TelegramBot[assoc_Symbol?AssociationQ], {method_String, params_Association}, OptionsPattern[]] := 
Module[{token, evaluate, history, logger, 
	endpoint, encoder, deserializer, form, 
	requestParameters, url, requestBody, contentType, request, httpMethod, 
	response, responseBody, result}, 
	
	token = assoc["Token"]; 
	evaluate = assoc["Evaluate"]; 
	history = assoc["History"]; 
	logger = assoc["Logger"]; 
	endpoint = OptionValue["Endpoint"]; 
	encoder = OptionValue["Encoder"];
	deserializer = OptionValue["Deserializer"]; 
	form = OptionValue["Form"]; 
	
	requestParameters = Map[encoder] @ DeleteCases[params, Automatic]; 
	
	url = Switch[form, 
		"Query", URLBuild[{endpoint, "bot" <> token, method}, requestParameters], 
		_, URLBuild[{endpoint, "bot" <> token, method}]
	]; 
	
	httpMethod = If[form == "Query", "GET", "POST"];
	
	requestBody = Switch[form, 
		"Query", "", 
		"URLEncoded", URLBuild[{}, requestParameters], 
		"JSON", ImportString[ExportString[requestParameters, "JSON"], "Text"], 
		"FormData", requestParameters
	];
	
	contentType = Switch[form, 
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
	
	result = deserializer[responseBody]; 
	
	logger[history, request, response]; 
	
	Return[result]
]


exec[bot_TelegramBot, {method_String, params: OptionsPattern[{}]}, opts: OptionsPattern[{}]] := 
exec[
	bot, 
	{
		method, 
		<|KeyValueMap[
			StringReplace[
				ToString[#1], 
				Map[
					Function[c, c -> "_" <> ToLowerCase[c]], 
					CharacterRange["A", "Z"]
				]
			] -> #2&
		] @ <|FilterRules[Flatten[{params}], Except[Options[exec]]]|>|>
	}, 
	FilterRules[Flatten[{opts}], Options[exec]]
]






(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*getMe*)


SyntaxInformation[getMe] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
getMe[bot_TelegramBot, opts: OptionsPattern[{exec}]] := 
exec[bot, {"getMe"}, opts]


(* ::Subsection:: *)
(*getUpdates*)


Options[getUpdates] = {
	"offset" -> Automatic, 
	"limit" -> Automatic, 
	"timeout" -> Automatic, 
	"allowedUpdates" -> Automatic
}


SyntaxInformation[getUpdates] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec, getUpdates}]
}


TelegramBot /: 
getUpdates[bot_TelegramBot, opts: OptionsPattern[{exec, getUpdates}]] := 
exec[bot, {"getUpdates", opts}, opts]


(* ::Subsection:: *)
(*setWebhook*)


Options[setWebhook] = {
	"certificate" -> Automatic, 
	"maxConnections" -> Automatic, 
	"allowedUpdates" -> Automatic
}


SyntaxInformation[setWebhook] = {
	"ArgumentsPattern" -> {_., _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec, setWebhook}]
}


TelegramBot /: 
setWebhook[bot_TelegramBot, url_String, opts: OptionsPattern[{exec, setWebhook}]] := 
exec[bot, {"setWebhook", "url" -> url, opts}, opts]


(* ::Subsection:: *)
(*deleteWebhook*)


SyntaxInformation[deleteWebhook] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
deleteWebhook[bot_TelegramBot, opts: OptionsPattern[{exec}]] := 
exec[bot, {"deleteWebhook"}, opts]


(* ::Subsection:: *)
(*getWebhookInfo*)


SyntaxInformation[getWebhookInfo] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
getWebhookInfo[bot_TelegramBot, opts: OptionsPattern[{exec}]] := 
exec[bot, {"getWebhookInfo"}, opts]


(* ::Subsection:: *)
(*sendMessage*)


Options[sendMessage] = {
	"parseMode" -> Automatic, 
	"disableWebPagePreview" -> Automatic, 
	"disableNotification" -> Automatic, 
	"replyToMessage_id" -> Automatic,
	"replyMarkup" -> Automatic
}


SyntaxInformation[sendMessage] = 
	{
		"ArgumentsPattern" -> {_., _, _, OptionsPattern[]}, 
		"OptionNames" -> optionNames[{exec, sendMessage}]
	}


TelegramBot /: 
sendMessage[bot_TelegramBot, chatId: _String | _Integer, text_String, opts: OptionsPattern[{exec, sendMessage}]] := 
exec[bot, {"sendMessage", "chatId" -> chatId, "text" -> text, opts}, opts]


(* ::Subsection:: *)
(*getUserProfilePhotos*)


Options[getUserProfilePhotos] = {
	"offset" -> Automatic, 
	"limit" -> Automatic
}


SyntaxInformation[sendMessage] = {
	"ArgumentsPattern" -> {_., _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec, getUserProfilePhotos}]
}


TelegramBot /: 
getUserProfilePhotos[bot_TelegramBot, userId: _String | _Integer, 
	opts: OptionsPattern[{exec, getUserProfilePhotos}]] := 
exec[bot, {"getUserProfilePhotos", "userId" -> userId, opts}, opts]


(* ::Subsection:: *)
(*getFile*)


SyntaxInformation[getFile] = {
	"ArgumentsPattern" -> {_., _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
getFile[bot_TelegramBot, fileId_String, opts: OptionsPattern[{exec}]] := 
exec[bot, {"getFile", "fileId" -> fileId, opts}, opts]


(* ::Section::Closed:: *)
(*End private*)


End[]


(* ::Section::Closed:: *)
(*End package*)


EndPackage[]
