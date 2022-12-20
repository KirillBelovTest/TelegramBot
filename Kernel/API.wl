(* ::Package:: *)

(* ::Chapter:: *)
(*Telegram Bot API*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`TelegramBot`"]


(* ::Section:: *)
(*Endpoints*)


getMe::usage = 
"getMe[bot] base info about the bot
bot@getMe[] another way to call this method"


logOut::usage = 
"logOut[bot] use this method to log out from the cloud Bot API server before launching the bot locally
bot@logOut[] another way to call this method"


close::usage = 
"close[bot] use this method to close the bot instance before moving it from one local server to another
bot@close[] another way to call this method"


getUpdates::usage = 
"getUpdates[bot] get currents updates of the bot
bot@getUpdates[] another way to call this method"


setWebhook::usage = 
"setWebhook[bot, url] all updates senging to url in the body of the post method
bot@setWebhook[url] another way to call this method"


deleteWebhook::usage = 
"deleteWebhook[bot] delete current webhook
bot@deleteWebhook[] another way to call this method"


getWebhookInfo::usage = 
"getWebhookInfo[bot]
bot@getWebhookInfo[] another way to call this method";


sendMessage::usage = 
"sendMessage[bot, chatId, text] send text messages
bot@sendMessage[chatId, text] another way to call this method"


forwardMessage::usage = 
"forwardMessage[bot, chatId, fromChatId, messageId] use this method to forward messages of any kind
bot@forwardMessage[chatId, fromChatId, messageId] another way to call this method"


sendPhoto::usage = 
"sendPhoto[bot, chatId, photo] send photos
bot@sendPhoto[chatId, photo] another way to call this method"


sendAudio::usage = 
"sendAudio[bot, chatId, audio] send audio files
bot@sendAudio[chatId, audio] another way to call this method"


sendDocument::usage = 
"sendDocument[bot, chatId, document] send general files
bot@sendDocument[chatId, document] another way to call this method"


sendVideo::usage = 
"sendVideo[bot, chatId, video] send video files
bot@sendVideo[chatId, video] another way to call this method"


sendAnimation::usage = 
"sendAnimation[bot, chatId, animation] send animation files
bot@sendAnimation[chatId, animation] another way to call this method"


getUserProfilePhotos::usage = 
"getUserProfilePhotos[bot, userId]
bot@getUserProfilePhotos[bot, userId] another way to call this method"


getFile::usage = 
"getFile[bot, fileId]
bot@getFile[fileId] another way to call this method"


(* ::Section:: *)
(*Private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Patterns*)


botPattern[] := 
KirillBelov`TelegramBot`TelegramBot[_Symbol?AssociationQ]


(* ::Section::Closed:: *)
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


exec[bot: botPattern[], {method_String, params_Association}, OptionsPattern[]] := 
Module[{token, evaluate, history, logger, 
	endpoint, encoder, deserializer, form, 
	requestParameters, url, requestBody, contentType, request, httpMethod, 
	response, responseBody, result}, 
	
	token = bot["Token"]; 
	history = bot["History"]; 
	logger = bot["Logger"]; 
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
	
	response = URLRead[request];
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
getMe[bot: botPattern[], opts: OptionsPattern[{exec}]] := 
exec[bot, {"getMe"}, opts]


(* ::Subsection:: *)
(*logOut*)


SyntaxInformation[logOut] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
logOut[bot: botPattern[], opts: OptionsPattern[{exec}]] := 
exec[bot, {"logOut"}, opts]


(* ::Subsection:: *)
(*close*)


SyntaxInformation[close] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
close[bot: botPattern[], opts: OptionsPattern[{exec}]] := 
exec[bot, {"close"}, opts]


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
getUpdates[bot: botPattern[], opts: OptionsPattern[{exec, getUpdates}]] := 
exec[bot, {"getUpdates", opts}, opts]


(* ::Subsection:: *)
(*setWebhook*)


Options[setWebhook] = {
	"certificate" -> Automatic, 
	"ipAddress" -> Automatic, 
	"maxConnections" -> Automatic, 
	"allowedUpdates" -> Automatic, 
	"dropPendingUpdates" -> Automatic, 
	"secretToken" -> Automatic
}


SyntaxInformation[setWebhook] = {
	"ArgumentsPattern" -> {_., _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec, setWebhook}]
}


TelegramBot /: 
setWebhook[bot: botPattern[], url_String, opts: OptionsPattern[{exec, setWebhook}]] := 
exec[bot, {"setWebhook", "url" -> url, opts}, opts]


(* ::Subsection:: *)
(*deleteWebhook*)


Options[deleteWebhook] = {
	"dropPendingUpdates" -> Automatic
}


SyntaxInformation[deleteWebhook] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec, deleteWebhook}]
}


TelegramBot /: 
deleteWebhook[bot: botPattern[], opts: OptionsPattern[{exec, deleteWebhook}]] := 
exec[bot, {"deleteWebhook", opts}, opts]


(* ::Subsection:: *)
(*getWebhookInfo*)


SyntaxInformation[getWebhookInfo] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
getWebhookInfo[bot: botPattern[], opts: OptionsPattern[{exec}]] := 
exec[bot, {"getWebhookInfo"}, opts]


(* ::Subsection:: *)
(*sendMessage*)


Options[sendMessage] = {
	"messageThreadId" -> Automatic, 
	"parseMode" -> Automatic, 
	"entities" -> Automatic, 
	"disableWebPagePreview" -> Automatic, 
	"disableNotification" -> Automatic, 
	"protectContent" -> Automatic, 
	"replyToMessageId" -> Automatic, 
	"allowSendingWithoutReply" -> Automatic, 
	"replyMarkup" -> Automatic
}


SyntaxInformation[sendMessage] = {
	"ArgumentsPattern" -> {_., _, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec, sendMessage}]
}


TelegramBot /: 
sendMessage[bot: botPattern[], chatId: _String | _Integer, text_String, 
	opts: OptionsPattern[{exec, sendMessage}]] := 
exec[bot, {"sendMessage", "chatId" -> chatId, "text" -> text, opts}, opts]


(* ::Subsection:: *)
(*forwardMessage*)


Options[forwardMessage] = {
	"messageThreadId" -> Automatic, 
	"disableNotification" -> Automatic, 
	"protectContent" -> Automatic
}


SyntaxInformation[sendMessage] = {
	"ArgumentsPattern" -> {_., _, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec, forwardMessage}]
}


TelegramBot /: 
forwardMessage[bot: botPattern[], chatId: _String | _Integer, fromChatId: _String | _Integer, 
	messageId_Integer, opts: OptionsPattern[{exec, forwardMessage}]] := 
exec[bot, {"forwardMessage", "chatId" -> chatId, "fromChatId" -> fromChatId, "messageId" -> messageId, opts}, opts]


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
getUserProfilePhotos[bot: botPattern[], userId: _String | _Integer, 
	opts: OptionsPattern[{exec, getUserProfilePhotos}]] := 
exec[bot, {"getUserProfilePhotos", "userId" -> userId, opts}, opts]


(* ::Subsection:: *)
(*getFile*)


SyntaxInformation[getFile] = {
	"ArgumentsPattern" -> {_., _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
getFile[bot: botPattern[], fileId_String, opts: OptionsPattern[{exec}]] := 
exec[bot, {"getFile", "fileId" -> fileId, opts}, opts]


(* ::Section:: *)
(*End private*)


End[]


(* ::Section:: *)
(*End package*)


EndPackage[]