#!/usr/bin/env wolframscript
(* ::Package:: *)

PacletInstall["KirillBelov/GPTLink"]; 
PacletInstall["KirillBelov/Objects"]; 
PacletInstall["KirillBelov/TelegramBot"]; 


<<KirillBelov`TelegramBot`
<<KirillBelov`GPTLink`


<<KirillBelov`GPTLink`


wolframAlpha::usage = 
"wolframAlpha[query] call WolframAlpha using query string. \
WolframAlpha - search engine based on knowledge. \
Works only in English."; 


wolframAlpha[query_String] := 
Echo[ToString[WolframAlpha[Echo[TextTranslation[query, "English"], "Wolfram Alpha Query"], "ShortAnswer"]], "Wolfram Alpha Answer"]


chatGPT[chatId_, text_String, callback_] := 
Module[{userMessage, assistantMessage, chat}, 
	If[Not[KeyExistsQ[$chats, chatId]], 
		$chats[chatId] = GPTChatObject["Tools" -> {wolframAlpha}, 
			"APIToken" :> SystemCredential["TYPINGMIND_API_KEY"], 
			"Endpoint" -> "https://api.typingmind.com/api", 
			"Logger" -> Function[Echo[#Body, #Event]]
		]
	]; 
	chat = $chats[chatId]; 
	GPTChatCompleteAsync[chat, text, callback]
]


handler[bot_TelegramBot, update_Association] := 
Module[{user, chatId, text}, 
	chatId = update["message", "chat", "id"]; 
	text = update["message", "text"]; 
	Echo[update, "Telegram Update"]; 
	chatGPT[chatId, StringTrim[text, "/"], Function[gptChat, 
		Echo[
			sendMessage[bot, chatId, gptChat["Messages"][[-1, "content"]], 
				"parseMode" -> "markdown", "replyToMessageId" -> update["message", "message_id"]
			], 
			"Telegram send Message"
		]
	]]; 
]


$chats = <||>; 
bot = TelegramBot["6155042972:AAHjGFYnzlVT0oG8UxB_5xgAMM4lAKtfcRw"]; 
task = CreateBotSession[bot, handler, {2, 60 * 60 * 24 * 7 * 4 * 12}]; 
If[Head@$FrontEnd =!= FrontEndObject, 
	While[task["TaskStatus"] == "Running", Pause[0.1]]; 
]
