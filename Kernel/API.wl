(* ::Package:: *)

(* ::Chapter:: *)
(*Telegram Bot API*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`TelegramBot`API`", {
    "KirillBelov`TelegramBot`Type`"
}]


(* ::Section::Closed:: *)
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


getChatMember::usage = 
"getUpdates[bot, chatId, userId] get info about the user on managed chat."; 


banChatMember::usage = 
"banChatMember[bot, chatId, userId] get info about the user on managed chat."


setWebhook::usage = 
"setWebhook[bot, url] all updates senging to url in the body of the post method
bot@setWebhook[url] another way to call this method"


deleteWebhook::usage = 
"deleteWebhook[bot] delete current webhook
bot@deleteWebhook[] another way to call this method"


getWebhookInfo::usage = 
"getWebhookInfo[bot]
bot@getWebhookInfo[] another way to call this method";


exportChatInviteLink::usage =
"exportChatInviteLink[bot, chatId] get invite link for the chatId.";


getFilePath::usage =
"getFilePath[bot, fileId] get file path of the fileId."


sendMessage::usage = 
"sendMessage[bot, chatId, text] send text messages
bot@sendMessage[chatId, text] another way to call this method."


forwardMessage::usage = 
"forwardMessage[bot, chatId, fromChatId, messageId] use this method to forward messages of any kind
bot@forwardMessage[chatId, fromChatId, messageId] another way to call this method."


sendPhoto::usage = 
"sendPhoto[bot, chatId, photo] send photos
bot@sendPhoto[chatId, photo] another way to call this method."


sendAudio::usage = 
"sendAudio[bot, chatId, audio] send audio files
bot@sendAudio[chatId, audio] another way to call this method."


sendDocument::usage = 
"sendDocument[bot, chatId, document] send general files
bot@sendDocument[chatId, document] another way to call this method."


sendVideo::usage = 
"sendVideo[bot, chatId, video] send video files
bot@sendVideo[chatId, video] another way to call this method."


sendAnimation::usage = 
"sendAnimation[bot, chatId, animation] send animation files
bot@sendAnimation[chatId, animation] another way to call this method."


getUserProfilePhotos::usage = 
"getUserProfilePhotos[bot, userId]
bot@getUserProfilePhotos[bot, userId] another way to call this method."


answerCallbackQuery::usage = 
"answerCallbackQuery[bot, callbackQueryId] answer to callback."; 


editMessageText::usage = 
"editMessageText[bot, chatId, messageId, text] edit message."; 


editMessageReplyMarkup::usage = 
"editMessageReplyMarkup[bot, chatId, messageId] edit message reply markup.";


sendMediaGroup::usage = 
"sendMediaGroup[bot, chatId, mediaList] send several media elements."; 


deleteMessage::usage = 
"deleteMessage[bot, chatId, messageId] delete message from chat."; 


(* ::Section::Closed:: *)
(*Private context*)


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Patterns*)


imagePattern[] := 
_Image | _Graphics | _Graphics3D


(* ::Section::Closed:: *)
(*Internal*)


optionNames[symbols: {__Symbol}] := 
Flatten[Function[symbol, Map["\"" <> # <> "\""&, Keys[Options[symbol]]], Listable][symbols]]


deserialize[body_String] := 
ImportString[body, "RawJSON"]


encode[key_String, value_] := key -> encode[value]


encode[str_String] := 
str


encode[url_URL] := 
url[[1]]


encode[key_String, photo: imagePattern[]] := 
key -> <|
    "Content" -> ExportString[photo, "PNG"], 
    "Name" -> key, 
    "MIMEType" -> "image/png"
|>


encode[key_String, audio_Audio] := 
key -> <|
    "Content" -> ExportString[audio, "MP3"], 
    "Name" -> key, 
    "MIMEType" -> "audio/mpeg"
|>


encode[key_String, video_Video] := 
key -> <|
    "Content" -> ExportString[video, "MP4"], 
    "Name" -> key, 
    "MIMEType" -> "video/mp4"
|>


encode[key_String, animation_Manipulate] := 
key -> <|
    "Content" -> ExportString[animation, "MP4"], 
    "Name" -> key, 
    "MIMEType" -> "video/mp4"
|>


encode[expr_] := 
expr


Options[exec] = {
    "Endpoint" -> "https://api.telegram.org", 
    "Form" -> "JSON", 
    "Encoder" -> encode, 
    "Deserializer" -> deserialize
}


exec[bot_TelegramBot, {method_String, params_Association}, OptionsPattern[]] := 
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
    
    requestParameters = KeyValueMap[encoder] @ DeleteCases[params, Automatic]; 
    
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


(* ::Section::Closed:: *)
(*Implementation*)


(* ::Subsection:: *)
(*getMe*)


SyntaxInformation[getMe] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec}]
}


TelegramBot /: getMe[bot_TelegramBot, opts: OptionsPattern[{exec}]] := 
exec[bot, {"getMe"}, opts]


(*getFilePath*)


SyntaxInformation[getFilePath] = {
    "ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec}]
}


TelegramBot /: getFilePath[bot_TelegramBot, fileId_String, opts: OptionsPattern[{exec}]] := 
exec[bot, {"getFile", "file_id" -> fileId, opts}, opts]; 


(* ::Subsection:: *)
(*logOut*)


SyntaxInformation[logOut] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
logOut[bot_TelegramBot, opts: OptionsPattern[{exec}]] := 
exec[bot, {"logOut"}, opts]


(* ::Subsection:: *)
(*close*)


SyntaxInformation[close] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec}]
}


TelegramBot /: 
close[bot_TelegramBot, opts: OptionsPattern[{exec}]] := 
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
getUpdates[bot_TelegramBot, opts: OptionsPattern[{exec, getUpdates}]] := 
exec[bot, {"getUpdates", opts}, opts]


(*getChatMember*)


TelegramBot /: getChatMember[bot_TelegramBot, chatId_, userId_, opts : OptionsPattern[{exec}]] := 
exec[bot, {"getChatMember", "chat_id" -> chatId, "user_id" -> userId, opts}, opts];


(*banChatMember*)


Options[banChatMember] = {
    "untilDate" :> Automatic
};


TelegramBot /: banChatMember[bot_TelegramBot, chatId_, userId_, opts : OptionsPattern[{exec}]] := 
exec[bot, {"banChatMember", "chat_id" -> chatId, "user_id" -> userId, opts}, opts];


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
setWebhook[bot_TelegramBot, url_String, opts: OptionsPattern[{exec, setWebhook}]] := 
exec[bot, {"setWebhook", "url" -> url, opts}, opts]


(* ::Subsection:: *)
(*exportChatInviteLink*)


Options[exportChatInviteLink] = {
    "name" -> Automatic, 
    "expireDate" -> Automatic, 
    "memberLimit" -> Automatic, 
    "createsJoinRequest" -> Automatic
}


SyntaxInformation[exportChatInviteLink] = {
    "ArgumentsPattern" -> {_., _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, setWebhook}]
}


TelegramBot /: 
exportChatInviteLink[bot_TelegramBot, chatId_Integer, opts: OptionsPattern[{exec, exportChatInviteLink}]] := 
exec[bot, {"exportChatInviteLink", "chat_id" -> chatId, opts}, opts]; 


(* ::Subsection:: *)
(*deleteWebhook*)


Options[deleteWebhook] = {
    "dropPendingUpdates" -> Automatic
}


SyntaxInformation[deleteWebhook] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, deleteWebhook}]
}


TelegramBot /: deleteWebhook[bot_TelegramBot, opts: OptionsPattern[{exec, deleteWebhook}]] := 
exec[bot, {"deleteWebhook", opts}, opts]


(* ::Subsection:: *)
(*getWebhookInfo*)


SyntaxInformation[getWebhookInfo] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec}]
}


TelegramBot /: getWebhookInfo[bot_TelegramBot, opts: OptionsPattern[{exec}]] := 
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
}; 


SyntaxInformation[sendMessage] = {
    "ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, sendMessage}]
}; 


TelegramBot /: sendMessage[bot_TelegramBot, chatId: _String | _Integer, text_String, 
    opts: OptionsPattern[{exec, sendMessage}]] := 
exec[bot, {"sendMessage", "chatId" -> chatId, "text" -> text, opts}, opts]; 


(* ::Subsection:: *)
(*deleteMessage*)


Options[deleteMessage] = {
    
}; 


SyntaxInformation[deleteMessage] = {
    "ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, deleteMessage}]
}; 


TelegramBot /: deleteMessage[bot_TelegramBot, chatId: _String | _Integer, messageId_Integer, 
    opts: OptionsPattern[{exec, sendMessage}]] := 
exec[bot, {"deleteMessage", "chatId" -> chatId, "messageId" -> messageId, opts}, opts]; 


(* ::Subsection:: *)
(*sendMediaGroup*)


Options[sendMediaGroup] = {
    "messageThreadId" -> Automatic, 
    "disableNotification" -> Automatic, 
    "protectContent" -> Automatic, 
    "replyParameters" -> Automatic
}


SyntaxInformation[sendMediaGroup] = {
    "ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, sendMediaGroup}]
}


TelegramBot /: sendMediaGroup[bot_TelegramBot, chatId: _String | _Integer, media_List, 
    opts: OptionsPattern[{exec, sendMediaGroup}]] := 
exec[bot, {"sendMediaGroup", "chatId" -> chatId, "media" -> 
    Map[Function[<|
        "type" -> "photo", 
        "media" -> #
    |>], media]
, opts}, opts]


(* ::Subsection:: *)
(*editMessageText*)


Options[editMessageText] = {
    "inlineMessageId" -> Automatic, 
    "parseMode" -> Automatic, 
    "entities" -> Automatic, 
    "disableWebPagePreview" -> Automatic, 
    "replyMarkup" -> Automatic
}


SyntaxInformation[editMessageText] = {
    "ArgumentsPattern" -> {_, _, _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, editMessageText}]
}; 


TelegramBot /: editMessageText[bot_TelegramBot, chatId: _String | _Integer, messageId: _String | _Integer, 
    text_String, opts: OptionsPattern[{exec, editMessageText}]] := 
exec[bot, {"editMessageText", 
    "chatId" -> chatId, "messageId" -> messageId, "text" -> text, opts
}, opts]; 


(* ::Subsection:: *)
(*editMessageReplyMarkup*)


Options[editMessageReplyMarkup] = {
    "inlineMessageId" -> Automatic, 
    "replyMarkup" -> Automatic
}; 


SyntaxInformation[editMessageReplyMarkup] = {
    "ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, editMessageReplyMarkup}]
}; 


TelegramBot /: editMessageReplyMarkup[bot_TelegramBot, chatId: _String | _Integer, messageId: _String | _Integer, opts: OptionsPattern[{exec, editMessageReplyMarkup}]] := 
exec[bot, {"editMessageReplyMarkup", "chatId" -> chatId, "messageId" -> messageId, opts}, opts]; 


(* ::Subsection:: *)
(*forwardMessage*)


Options[forwardMessage] = {
    "messageThreadId" -> Automatic, 
    "disableNotification" -> Automatic, 
    "protectContent" -> Automatic
}


SyntaxInformation[forwardMessage] = {
    "ArgumentsPattern" -> {_., _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, forwardMessage}]
}


TelegramBot /: forwardMessage[bot_TelegramBot, chatId: _String | _Integer, fromChatId: _String | _Integer, 
    messageId_Integer, opts: OptionsPattern[{exec, forwardMessage}]] := 
exec[bot, {"forwardMessage", "chatId" -> chatId, "fromChatId" -> fromChatId, "messageId" -> messageId, opts}, opts]


(* ::Subsection:: *)
(*sendPhoto*)


Options[sendPhoto] = {
    "messageThreadId" -> Automatic, 
    "caption" -> Automatic, 
    "parseMode" -> Automatic, 
    "captionEntities" -> Automatic, 
    "disableNotification" -> Automatic, 
    "protectContent" -> Automatic, 
    "replyToMessageId" -> Automatic, 
    "allowSendingWithoutReply" -> Automatic, 
    "replyMarkup" -> Automatic
}


SyntaxInformation[sendPhoto] = {
    "ArgumentsPattern" -> {_., _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, sendPhoto}]
}


TelegramBot /: sendPhoto[bot_TelegramBot, chatId: _String | _Integer, photo: imagePattern[], 
    opts: OptionsPattern[{exec, sendPhoto}]] := 
exec[bot, {"sendPhoto", "chatId" -> chatId, "photo" -> photo, opts}, opts, "Form" -> "FormData"]


TelegramBot /: sendPhoto[bot_TelegramBot, chatId: _String | _Integer, photo: _String | _URL, 
    opts: OptionsPattern[{exec, sendPhoto}]] := 
exec[bot, {"sendPhoto", "chatId" -> chatId, "photo" -> photo, opts}, opts]


TelegramBot /: sendPhoto[bot_TelegramBot, chatId: _String | _Integer, photo_, 
    opts: OptionsPattern[{exec, sendPhoto}]] := 
sendPhoto[bot, chatId, Rasterize[photo], opts]


(* ::Subsection:: *)
(*sendAudio*)


Options[sendAudio] = {
    "messageThreadId" -> Automatic, 
    "caption" -> Automatic, 
    "parseMode" -> Automatic, 
    "captionEntities" -> Automatic, 
    "duration" -> Automatic, 
    "performer" -> Automatic, 
    "title" -> Automatic, 
    "thumb" -> Automatic, 
    "disableNotification" -> Automatic, 
    "protectContent" -> Automatic, 
    "replyToMessageId" -> Automatic, 
    "allowSendingWithoutReply" -> Automatic, 
    "replyMarkup" -> Automatic
}


SyntaxInformation[sendAudio] = {
    "ArgumentsPattern" -> {_., _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, sendAudio}]
}


TelegramBot /: sendAudio[bot_TelegramBot, chatId: _String | _Integer, audio_Audio, 
    opts: OptionsPattern[{exec, sendAudio}]] := 
exec[bot, {"sendAudio", "chatId" -> chatId, "audio" -> audio, opts}, opts, "Form" -> "FormData"]


TelegramBot /: sendAudio[bot_TelegramBot, chatId: _String | _Integer, audio: _String | _URL, 
    opts: OptionsPattern[{exec, sendAudio}]] := 
exec[bot, {"sendAudio", "chatId" -> chatId, "audio" -> audio, opts}, opts]


(* ::Subsection:: *)
(*sendDocument*)


Options[sendDocument] = {
    "messageThreadId" -> Automatic, 
    "thumb" -> Automatic, 
    "caption" -> Automatic, 
    "parseMode" -> Automatic, 
    "captionEntities" -> Automatic, 
    "title" -> Automatic, 
    "disableContentTypeDetection" -> Automatic, 
    "disableNotification" -> Automatic, 
    "protectContent" -> Automatic, 
    "replyToMessageId" -> Automatic, 
    "allowSendingWithoutReply" -> Automatic, 
    "replyMarkup" -> Automatic
}


SyntaxInformation[sendDocument] = {
    "ArgumentsPattern" -> {_., _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, sendDocument}]
}


TelegramBot /: sendDocument[bot_TelegramBot, chatId: _String | _Integer, document_File, 
    opts: OptionsPattern[{exec, sendDocument}]] := 
exec[bot, {"sendDocument", "chatId" -> chatId, "document" -> document, opts}, opts, "Form" -> "FormData"]


TelegramBot /: sendDocument[bot_TelegramBot, chatId: _String | _Integer, document: _String | _URL, 
    opts: OptionsPattern[{exec, sendDocument}]] := 
exec[bot, {"sendDocument", "chatId" -> chatId, "document" -> document, opts}, opts]


(* ::Subsection:: *)
(*sendVideo*)


Options[sendVideo] = {
    "messageThreadId" -> Automatic, 
    "duration" -> Automatic, 
    "width" -> Automatic, 
    "height" -> Automatic, 
    "thumb" -> Automatic, 
    "caption" -> Automatic, 
    "parseMode" -> Automatic, 
    "captionEntities" -> Automatic, 
    "supports_Streaming" -> Automatic, 
    "disableNotification" -> Automatic, 
    "protectContent" -> Automatic, 
    "replyToMessageId" -> Automatic, 
    "allowSendingWithoutReply" -> Automatic, 
    "replyMarkup" -> Automatic
}


SyntaxInformation[sendVideo] = {
    "ArgumentsPattern" -> {_., _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, sendVideo}]
}


TelegramBot /: sendVideo[bot_TelegramBot, chatId: _String | _Integer, video_Video, 
    opts: OptionsPattern[{exec, sendVideo}]] := 
exec[bot, {"sendVideo", "chatId" -> chatId, "video" -> video, opts}, opts, "Form" -> "FormData"]


TelegramBot /: sendVideo[bot_TelegramBot, chatId: _String | _Integer, video: _String | _URL, 
    opts: OptionsPattern[{exec, sendVideo}]] := 
exec[bot, {"sendVideo", "chatId" -> chatId, "video" -> video, opts}, opts]


(* ::Subsection:: *)
(*sendAnimation*)


Options[sendAnimation] = {
    "messageThreadId" -> Automatic, 
    "duration" -> Automatic, 
    "width" -> Automatic, 
    "height" -> Automatic, 
    "thumb" -> Automatic, 
    "caption" -> Automatic, 
    "parseMode" -> Automatic, 
    "captionEntities" -> Automatic, 
    "disableNotification" -> Automatic, 
    "protectContent" -> Automatic, 
    "replyToMessageId" -> Automatic, 
    "allowSendingWithoutReply" -> Automatic, 
    "replyMarkup" -> Automatic
}


SyntaxInformation[sendAnimation] = {
    "ArgumentsPattern" -> {_., _, _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, sendAnimation}]
}


TelegramBot /: sendAnimation[bot_TelegramBot, chatId: _String | _Integer, animation_Manipulate, 
    opts: OptionsPattern[{exec, sendVideo}]] := 
exec[bot, {"sendAnimation", "chatId" -> chatId, "animation" -> animation, opts}, opts, "Form" -> "FormData"]


TelegramBot /: sendAnimation[bot_TelegramBot, chatId: _String | _Integer, animation: _String | _URL, 
    opts: OptionsPattern[{exec, sendAnimation}]] := 
exec[bot, {"sendAnimation", "chatId" -> chatId, "animation" -> animation, opts}, opts]


(* ::Subsection:: *)
(*getUserProfilePhotos*)


Options[getUserProfilePhotos] = {
    "offset" -> Automatic, 
    "limit" -> Automatic
}


SyntaxInformation[getUserProfilePhotos] = {
    "ArgumentsPattern" -> {_., _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, getUserProfilePhotos}]
}


TelegramBot /: getUserProfilePhotos[bot_TelegramBot, userId: _String | _Integer, 
    opts: OptionsPattern[{exec, getUserProfilePhotos}]] := 
exec[bot, {"getUserProfilePhotos", "userId" -> userId, opts}, opts]


(* ::Subsection:: *)
(*answerCallbackQuery*)


Options[answerCallbackQuery] = {
    "text" -> Automatic, 
    "showAlert" -> Automatic, 
    "url" -> Automatic, 
    "cacheTime" -> Automatic
}


SyntaxInformation[answerCallbackQuery] = {
    "ArgumentsPattern" -> {_., _, OptionsPattern[]}, 
    "OptionNames" -> optionNames[{exec, answerCallbackQuery}]
}


TelegramBot /: answerCallbackQuery[bot_TelegramBot, callbackQueryId: _String | _Integer, 
    opts: OptionsPattern[{exec, answerCallbackQuery}]] := 
exec[bot, {"answerCallbackQuery", "callbackQueryId" -> callbackQueryId, opts}, opts]


(* ::Section::Closed:: *)
(*End private*)


End[]; 


(* ::Section::Closed:: *)
(*End package*)


EndPackage[]; 
