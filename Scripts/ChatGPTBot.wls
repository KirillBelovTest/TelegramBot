#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Installation*)


(* ::Text:: *)
(*If TelegramBot \:0438 OpenAILink packages are not already installed, you can install them like this:*)


PacletInstall["ChristopherWolfram/OpenAILink"]; 
PacletInstall["KirillBelov/Objects"]; 
PacletInstall["KirillBelov/TelegramBot"]; 


(* ::Section:: *)
(*Needs*)


(* ::Text:: *)
(*After installation, you must load into the current session:*)


<<KirillBelov`TelegramBot`
<<ChristopherWolfram`OpenAILink`


(* ::Section:: *)
(*GPT3.5 Simple Interface*)


(* ::Text:: *)
(*Next step. You need to create a wrapper over the functions from the OpenAILink package, such that it can save previous requests and responses in the history. This function should take a chat ID and text as input and return only text:*)


chatGPT[chatId_, text_String] := 
Module[{userMessage, assistantMessage, chat, errorText = ""}, 
	If[Not[KeyExistsQ[$chats, chatId]], $chats[chatId] = {}]; 
	chat = $chats[chatId]; 

	userMessage = OpenAIChatMessageObject["user", text]; 
	AppendTo[chat, userMessage]; 

	assistantMessage = Check[
		OpenAIChatComplete[chat], 
		chat = {userMessage}; 
		errorText = "[HISTORY REMOVED]\n"; 
		OpenAIChatComplete[chat, OpenAITemperature -> 0.25], 
		OpenAIChatComplete::openAIResponseFailureMessage
	]; 

	AppendTo[chat, assistantMessage]; 
	$chats[chatId] = chat; 

	(*Return*)
	errorText <> assistantMessage[[-1, "Text"]]
]


handler[bot_TelegramBot, update_Association] := 
Module[{user, chatId, text, result}, 
	chatId = update["message", "chat", "id"]; 
	text = update["message", "text"]; 
	Print[update]; 
	result = chatGPT[chatId, text]; 
	Print[sendMessage[bot, chatId, result, "parseMode" -> "markdown"]]; 
]


$chats = <||>; 
$OpenAIKey = "<your OpenAI API Key>"; 
bot = TelegramBot["<yuor telegram bot API token>"]; 
task = CreateBotSession[bot, handler, {1, 60 * 60 * 24 * 7 * 4 * 12}]; 
While[task["TaskStatus"] == "Running", Pause[0.1]]; 
