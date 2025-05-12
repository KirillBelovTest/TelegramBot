(* ::Package:: *)

(* ::Chapter:: *)
(*TelegramBot Type*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`TelegramBot`Type`", {"KirillBelov`Objects`"}]; 


(* ::Section:: *)
(*Types*)


TelegramBot::usage = 
"TelegramBot[token] telegram bot representation."; 


(* ::Section:: *)
(*Private context*)


Begin["`Private`"]; 


(* ::Section:: *)
(*Implementation*)


CreateType[TelegramBot, init, {
    "Token" :> SystemCredential["TELEGRAM_BOT_TOKEN"], 
    "History", 
    "Logger", 
    "Async" -> True, 
    "UpdateHandler" -> Function["Hello!"]
}]; 


TelegramBot[token_?StringQ] := 
TelegramBot["Token" -> token]; 


(* ::Section:: *)
(*Initializator*)


init[bot_TelegramBot] := 
Module[{info, name}, 
    bot["History"] = CreateDataStructure["RingBuffer", 1024]; 
    bot["Logger"] = Function[{history, request, response}, 
        history["PushBack", <|"Time" -> Now, "Request" -> request, "Response" -> response|>]
    ]; 
    
    info = KirillBelov`TelegramBot`API`getMe[bot]; 
    name = info["result", "username"]; 
    bot["Name"] = name; 
]; 


(* ::Section:: *)
(*End private*)


End[]


(* ::Section:: *)
(*End package*)


EndPackage[]
