#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Text:: *)
(**)


(* ::Text:: *)
(*In this very short article I want to show how anyone can create their own Telegram bot, *)
(*	which will be an intermediary for communication between humans and the GPT3 .5 model (at the moment) . *)
(*  	Yes, I, like everyone else, was patient for a long time, but finally succumbed to this madness too, though I didn' t want to at first . *)
(*  	At present, there are a lot of different tools and ready - made programs which use OpenAI' s GPT model one way or another . *)
(*   	What is the fundamental difference between what I want to demonstrate to the community today? *)
(*   	And the difference is that firstly, I want to show how to make your own bot, which will be personally yours . *)
(*  	Secondly, all you need to do is to create two simple functions and do the initialization . *)
(*  	And third, by adding this bot to a group chat you will be able to communicate with it all over the place . *)
(*   	And this is not the same as one - to - one communication . When other users see a question from one of the participants, *)
(*	they voluntarily and involuntarily come up with new thoughts and ideas about how to complement the question of the previous user . *)
(*  	And the one who asked the first question, seeing what imagination his friends in the chat have shown, *)
(*	just the same can be even more interesting to develop his original idea . In such a collaboration with GPT, *)
(*	a few people can find much more interesting and useful applications for the neural network than if everyone was doing it alone . Well, let' s do it!*)


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
$OpenAIKey = "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"; 
bot = TelegramBot["6155042972:AAHqO05rHGCc4yHxOjYtDUp6pPtqsZL-qgU"]; 
task = CreateBotSession[bot, handler, {1, 60 * 60 * 24 * 7 * 4 * 12}]; 
While[task["TaskStatus"] == "Running", Pause[0.1]]; 