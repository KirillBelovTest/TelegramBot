#!/usr/bin/env wolframscript
(* ::Package:: *)

PacletDirectoryLoad[FileNameJoin[{$HomeDirectory, "Projects", #}]]& /@ {
    "TelegramBot", 
    "AILink", 
    "Internal"
};


PacletInstall["KirillBelov/AILink"]; 
PacletInstall["KirillBelov/Objects"]; 
PacletInstall["KirillBelov/TelegramBot"]; 


<<KirillBelov`TelegramBot`
<<KirillBelov`AILink`ChatCompletions`


$updatesQueue = CreateDataStructure["Queue"]; 


$sendQueue = CreateDataStructure["Queue"]; 


bot = TelegramBot["6155042972:AAHjGFYnzlVT0oG8UxB_5xgAMM4lAKtfcRw"]; 


task = CreateBotSession[bot, handler, {5, 60 * 60 * 24 * 7 * 4 * 12}]; 


$chatId = -1001838635981; 


$threadId = 2791; 


handler[bot_, update_] := Module[{
    
}, 
    Echo[update, "UPDATE:"]; 
    If[
        KeyExistsQ[update, "message"] && 
        KeyExistsQ[update["message"], "text"] && 
        update["message", "chat", "id"] === $chatId && 
        KeyExistsQ[update["message"], "is_topic_message"] && 
        KeyExistsQ[update["message"], "message_thread_id"] && 
        update["message", "is_topic_message"] && 
        update["message", "message_thread_id"] === $threadId, 
        
        Echo[
            sendMessage[bot, update["message", "chat", "id"], 
                gpt[update["message", "text"]], 
                "messageThreadId" -> update["message", "message_thread_id"]
            ], 
            "SEND:"
        ]
    ]
]; 


chat = AIChatObject[]; 


chat["Messages"] = {<|"role"->"system", 
"content"->"\:0422\:044b \:043f\:043e\:043c\:043e\:0433\:0430\:0435\:0448\:044c \:0441 \:043c\:0430\:0442\:0435\:043c\:0430\:0442\:0438\:0447\:0435\:0441\:043a\:0438\:043c\:0438 \:0437\:0430\:0434\:0430\:0447\:0430\:043c\:0438 \:0438 \:0437\:043d\:0430\:0435\:0448\:044c Wolfram Mathematica."|>}; 


gpt[text_] := Module[{}, 
    If[Length[chat["Messages"]] > 20, 
        chat["Messages"] = chat["Messages"][[{1, -5, -4, -3, -2, -1}]]; 
    ]; 
    chat["Messages"] += text;
    AIChatComplete[chat]; 
    chat["Messages"][[-1, "content"]]
]; 


If[Head@$FrontEnd =!= FrontEndObject, 
    While[task["TaskStatus"] == "Running", Pause[0.1]]; 
]
