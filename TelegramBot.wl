(* ::Package:: *)

(* ::Title:: *)
(*TelegramBot*)


(* ::Section:: *)
(*Begin*)


BeginPackage["TelegramBot`"]


ClearAll["`*"]


TelegramBot::usage = "TelegramBot[token]";


(* ::Section:: *)
(*API*)


$telegramAPI = "https://api.telegram.org";


telegramExecute[
    TelegramBot[token_String], method_String, 
    parameters: {(_String -> _)...}: {}
] := Block[{
    request, requestURL, requestRules, requestBody, 
    response, responseBody
}, 
    requestURL = URLBuild[{$telegramAPI, "bot" <> token, method}];
    requestRules = DeleteCases[parameters, _[_String, Automatic | Null | None]];
    requestBody = ImportString[ExportString[requestRules, "JSON"], "Text"];

    request = HTTPRequest[requestURL, <|
        Method -> "POST", 
        "ContentType" -> "application/json; charset=utf-8", 
        "Body" -> requestBody
    |>];

    response = URLRead[request];
    responseBody = response["Body"];

    Return[ImportString[responseBody, "RawJSON"]]
]


getUpdates::usage = "getUpdates[bot, opts]";


Options[getUpdates] = {
    "offset" -> Automatic,
    "limit" -> Automatic, 
    "timeout" -> Automatic, 
    "allowed_updates" -> Automatic
};


TelegramBot /: 
getUpdates[bot_TelegramBot, opts: OptionsPattern[getUpdates]] := 
telegramExecute[bot, "getUpdates", Flatten[{opts}]]


setWebhook::usage = "setWebhook[bot, url, opts]";


Options[setWebhook] = {
    "certificate" -> Automatic, 
    "max_connections" -> Automatic, 
    "allowed_updates" -> Automatic
};


TelegramBot /: 
setWebhook[bot_TelegramBot, url_String, opts: OptionsPattern[setWebhook]] := 
telegramExecute[bot, "setWebhook", Join[{"url" -> url}, Flatten[{opts}]]]


deleteWebhook::usage = "deleteWebhook[bot]";


TelegramBot /: 
deleteWebhook[bot_TelegramBot] := 
telegramExecute[bot, "deleteWebhook"]


getWebhookInfo::usage = "getWebhookInfo[bot]";


TelegramBot /: 
getWebhookInfo[bot_TelegramBot] := 
telegramExecute[bot, "getWebhookInfo"]


sendMessage::usage = "sendMessage[bot, chat, text]";


Options[sendMessage] = {
    "parse_mode" -> Automatic, 
    "disable_web_page_preview" -> Automatic, 
    "disable_notification" -> Automatic, 
    "reply_to_message_id" -> Automatic, 
    "reply_markup" -> Automatic
};


TelegramBot /: 
sendMessage[bot_TelegramBot, chat_Integer, text_String, 
    opts: OptionsPattern[sendMessage]] := 
telegramExecute[
    bot, "sendMessage", 
    Join[{"chat_id" -> chat, "text" -> text}, Flatten[{opts}]]
]


sendPhoto::usage = "sendPhoto[bot, chat, photo]";


Options[sendPhoto] = {
	"caption" -> Automatic, 
    "parse_mode" -> Automatic, 
    "disable_web_page_preview" -> Automatic, 
    "disable_notification" -> Automatic, 
    "reply_to_message_id" -> Automatic, 
    "reply_markup" -> Automatic
};


sendPhoto /: 
sendPhoto[bot_TelegramBot, chat_Integer, photo_String, 
    opts: OptionsPattern[sendPhoto]] := 
telegramExecute[
	bot, "sendPhoto", 
	Join[{"chat_id" -> chat, "photo" -> photo}, Flatten[{opts}]]
]


getFile[bot_TelegramBot, fileId_String] := 
	telegramExecute[
		bot, "getFile", 
		{"file_id" -> fileId}
	]


(* ::Section:: *)
(*Deploy*)


deployWebhook[bot_TelegramBot, handler_] := 
    CloudDeploy[APIFunction[{}, handler[HTTPRequestData["Body"]] &],
        "Deploy/TelegramBot/" <> Hash[bot, "SHA", "HexString"] <> "/webhook", 
        Permissions -> "Public"
    ]


(* ::Section:: *)
(*Debug handler*)


handlerReply[bot_TelegramBot][body_String] := 
    Block[{json = ImportString[body, "RawJSON"], chat}, 
        chat = json["message", "chat", "id"];
        sendMessage[bot, chat, body];
    ]


(* ::Section:: *)
(*help*)


$helpText = 
"*\:0421\:041f\:0420\:0410\:0412\:041a\:0410*

/help - \:0441\:043f\:0440\:0430\:0432\:043a\:0430 \:043f\:043e \:043a\:043e\:043c\:0430\:043d\:0434\:0430\:043c
/clear - \:043e\:0447\:0438\:0449\:0430\:0435\:0442 \:0441\:0435\:0441\:0441\:0438\:044e \:043f\:043e\:043b\:044c\:0437\:043e\:0432\:0430\:0442\:0435\:043b\:044f
/example - \:0441\:043f\:0438\:0441\:043e\:043a \:0434\:043e\:0441\:0442\:0443\:043f\:043d\:044b\:0445 \:043f\:0440\:0438\:043c\:0435\:0440\:043e\:0432 \:043a\:043e\:0434\:0430
/example _<func>_ - \:0432\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:043f\:0440\:0438\:043c\:0435\:0440 \:043a\:043e\:0434\:0430
/code _<expr>_ - \:0432\:044b\:043f\:043e\:043b\:044f\:0435\:0442 \:0432\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:0435 _<expr>_
`expr` - \:0432\:0442\:043e\:0440\:043e\:0439 \:0441\:043f\:043e\:0441\:043e\:0431 \:0432\:044b\:043f\:043e\:043b\:043d\:0438\:0442\:044c \:0432\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:0435 - \:044d\:0442\:043e \:043e\:0442\:0444\:043e\:0440\:043c\:0430\:0442\:0438\:0440\:043e\:0432\:0430\:0442\:044c \:0435\:0433\:043e \:043a\:0430\:043a `\:043c\:043e\:043d\:043e\:0448\:0438\:0440\:0438\:043d\:043d\:044b\:0439 \:0442\:0435\:043a\:0441\:0442` (Ctrl + Shift + M). 
\:0412 \:044d\:0442\:043e\:043c \:0441\:043b\:0443\:0447\:0430\:0435 \:043c\:043e\:0436\:043d\:043e \:0441\:043c\:0435\:0448\:0438\:0432\:0430\:0442\:044c \:0432\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:044f \:0438 \:0442\:0435\:043a\:0441\:0442 \:0432 \:043e\:0434\:043d\:043e\:043c \:0441\:043e\:043e\:0431\:0449\:0435\:043d\:0438\:0438

\:0414\:0435\:043c\:043e\:043d\:0441\:0442\:0440\:0430\:0446\:0438\:044f https://youtu.be/nSDiGiG6Ni8
\:0415\:0441\:043b\:0438 \:0443 \:0432\:0430\:0441 \:0435\:0449\:0435 \:0435\:0441\:0442\:044c \:0432\:043e\:043f\:0440\:043e\:0441\:044b - \:043f\:0438\:0448\:0438\:0442\:0435 \:0430\:0432\:0442\:043e\:0440\:0443 @kirillbelovtest"; 


(* ::Section:: *)
(*toText*)


toText[x_?NumberQ] := ToString[DecimalForm[x]]
toText[x_] := ToString[x]


(* ::Section:: *)
(*example*)


example[] := 
"*\:041f\:0440\:0438\:043c\:0435\:0440\:044b*
 
/example query - \:043f\:0440\:0438\:043c\:0435\:0440 \:0440\:0430\:0431\:043e\:0442\:044b \:0441 \:0444\:0443\:043d\:043a\:0446\:0438\:0435\:0439 *Query[]*
/example options - \:0438\:0441\:043f\:043e\:043b\:044c\:0437\:043e\:0432\:0430\:043d\:0438\:0435 \:043e\:043f\:0446\:0438\:0439"


example["code"] := 
"\:0412\:0432\:0435\:0434\:0438\:0442\:0435 \:043f\:043e\:0441\:043b\:0435 \:043a\:043e\:043c\:0430\:043d\:0434\:044b /code \:0432\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:0435. \:041d\:0430\:043f\:0440\:0438\:043c\:0435\:0440: 
/code 2 + 3

\:041b\:0438\:0431\:043e \:043e\:0442\:0444\:043e\:0440\:043c\:0430\:0442\:0438\:0440\:0443\:0439\:0442\:0435 \:0432\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:0435, \:043a\:043e\:0442\:043e\:0440\:043e\:0435 \:043d\:0443\:0436\:043d\:043e \:0432\:044b\:043f\:043e\:043b\:043d\:0438\:0442\:044c `\:043c\:043e\:043d\:043e\:0448\:0438\:0440\:0438\:043d\:043d\:044b\:043c \:0448\:0440\:0438\:0444\:0442\:043e\:043c` \:0431\:0435\:0437 \:0438\:0441\:043f\:043e\:043b\:044c\:0437\:043e\:0432\:0430\:043d\:0438\:044f \:043a\:043e\:043c\:0430\:043d\:0434\:044b /code"


example[text_] /; StringMatchQ[text, "/example query", IgnoreCase -> True] := 
"*Query*

```
(*\:0421\:043e\:0437\:0434\:0430\:0434\:0438\:043c \:0442\:0430\:0431\:043b\:0438\:0446\:0443 \:0441 \:0434\:0430\:043d\:043d\:044b\:043c\:0438*)
data = {
	<|\"name\" -> \"Ivan\",  \"age\" -> 24, \"city\" -> \"Saratov\"|>, 
	<|\"name\" -> \"Masha\", \"age\" -> 21, \"city\" ->  \"Moscow\"|>, 
	<|\"name\" -> \"Oleg\",  \"age\" -> 32, \"city\" ->    \"Tula\"|>
}; 

(*\:0412\:044b\:0431\:0435\:0440\:0435\:043c \:0442\:043e\:043b\:044c\:043a\:043e \:0438\:043c\:0435\:043d\:0430 \:0438 \:0433\:043e\:0440\:043e\:0434\:0430*)
Query[All, {\"name\", \"city\"}] @ data

(*\:0412\:044b\:0431\:0435\:0440\:0435\:043c \:0442\:0435\:0445 \:043a\:0442\:043e \:0441\:0442\:0430\:0440\:0448\:0435 22 \:043b\:0435\:0442*)
Query[Select[#age > 22&]] @ data

(*\:041f\:0440\:0435\:043e\:0431\:0440\:0430\:0437\:0443\:0435\:043c \:043a \:0442\:0430\:0431\:043b\:0438\:0446\:0435 \:0441 \:0433\:043e\:0434\:0430\:043c\:0438 \:0432\:044b\:0445\:043e\:0434\:0430 \:043d\:0430 \:043f\:0435\:043d\:0441\:0438\:044e*)
Query[All, <|
	\"name\" -> \"name\", 
	\"year\" -> Function[2020 + If[#name === \"Masha\", 60, 65] - #age]
|>] @ data
```" 


example[text_] /; StringMatchQ[text, "/example options", IgnoreCase -> True] := 
"*Options*

```
(*\:041f\:0440\:0438\:0441\:0432\:043e\:0438\:043c \:0441\:043f\:0438\:0441\:043e\:043a \:043e\:043f\:0446\:0438\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)
Options[function] = {
	\"a\" -> 1, 
	\"t\" :> Now
}

(*\:0421\:043e\:0437\:0434\:0430\:0434\:0438\:043c \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:044f \:0438\:0441\:043f\:043e\:043b\:044c\:0437\:0443\:044f \:044d\:0442\:0438 \:043e\:043f\:0446\:0438\:0438*)
function[x_, OptionsPattern[]] := 
	\"\:0412\:044b\:0437\:043e\:0432 \:0441\:043e \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430\:043c\:0438:\\n\" <> 
	\"x = \" <> ToString[x] <> \";\\n\" <> 
	\"a = \" <> ToString[OptionValue[\"a\"]] <> \";\\n\" <> 
	\"t = \" <> DateString[OptionValue[\"t\"]]

(*\:0412\:044b\:0431\:0435\:0440\:0435\:043c \:0442\:0435\:0445 \:043a\:0442\:043e \:0441\:0442\:0430\:0440\:0448\:0435 22 \:043b\:0435\:0442*)
Query[Select[#age > 22&]] @ data

(*\:0418 \:0432\:044b\:0437\:043e\:0432\:0435\:043c \:0444\:0443\:043d\:043a\:0446\:0438\:044e*)
function[1]
function[2, a -> 3]
function[4, a -> 5, t -> Yesterday]
```" 


(* ::Section:: *)
(*Evaluate handler*)


handlerEvaluate[bot_TelegramBot][body_String] := 
	Block[{json, username, msgid, chat, text, code, result, fragments, image, boturl, session, temppng}, 
		json = ImportString[body, "RawJSON"]; 
		boturl = "Deploy/TelegramBot/" <> Hash[bot, "SHA", "HexString"] <> "/"; 
		chat = json["message", "chat", "id"]; 
		text = json["message", "text"]; 
		username = json["message", "from", "username"]; 
		session = boturl <> "sessions/" <> username; 
		msgid = json["message", "message_id"]; 
		
		Which[
			text === "/clear" || text === "/clear@WolframKernelBot", 
				DeleteObject[CloudObject[session]], 
		
			text === "/help" || text === "/help@WolframKernelBot", 
				sendMessage[bot, chat, $helpText, "parse_mode" -> "markdown"],  
			
			StringMatchQ[text, "/example"], 
				sendMessage[bot, chat, example[], "parse_mode" -> "markdown"], 
			
			StringMatchQ[text, "/example " ~~ __], 
				sendMessage[bot, chat, example[text], "parse_mode" -> "markdown"],  
			
			StringMatchQ[text, "/semantic " ~~ __], 
				sendMessage[bot, chat, ToString[SemanticInterpretation[StringTrim[text, "/semantic "]]], 
					"parse_mode" -> "markdown", "reply_to_message_id" -> msgid],  
			
			StringMatchQ[text, "/code"], 
				sendMessage[bot, chat, example["code"], "parse_mode" -> "markdown"], 
			
			StringMatchQ[text, "/session" ~~ __], 
				sendMessage[bot, chat, body, "parse_mode" -> "markdown"], 
			
			KeyExistsQ[json["message"], "entities"] || StringMatchQ[text, "/code " ~~ __], 
				Check[CloudGet[boturl <> "sessions/" <> username], Null]; 
				
				fragments = If[StringSplit[text, WhitespaceCharacter][[1]] =!= "/code", 
					Select[json["message", "entities"], #type === "code"&], 
					{<|"offset" -> StringLength["/code"], "length" -> StringLength[text] - StringLength["/code"]|>}
				]; 
				
				Table[
					code = StringTake[text, {fragment["offset"] + 1, fragment["offset"] + fragment["length"]}]; 
					result = Block[{$Context = "Global`"}, Get[StringToStream[code]]]; 
					Which[
						result === Null, 
							Null, 
					
						MatchQ[result, _Graphics | _GeoGraphics | _Graphics3D | _Image], 
							temppng = boturl <> "image" <> ToString[UnixTime[Now]] <> ".png"; 
							image = CloudExport[result, "PNG", temppng, Permissions -> "Public"]; 
							sendPhoto[bot, chat, First[image], "reply_to_message_id" -> msgid]; 
							DeleteObject[image], 
					
						True, 
							sendMessage[bot, chat, toText[result], "reply_to_message_id" -> msgid]
					];, 
					{fragment, fragments}
				]; 
				DeleteObject[CloudObject[session]]; 
				CloudSave["Global`", session]; 
		]; 
	]


(* ::Section:: *)
(*End*)


EndPackage[]


(* ::Section:: *)
(*Run*)


token = "1247031088:AAGW73yzggrm4Tr7bldGCCaIrrwISNsdDzg"; 
bot = TelegramBot`TelegramBot[token]; 
TelegramBot`setWebhook[bot, First @ TelegramBot`deployWebhook[bot, TelegramBot`handlerEvaluate[bot]]]; 
