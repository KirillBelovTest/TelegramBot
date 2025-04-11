(* ::Package:: *)

(* ::Title:: *)
(*TalkBot*)


(* ::Section:: *)
(*Info*)


(* :Title     : TalkBot              *)
(* :Context   : TelegramBot`TalkBot` *)
(* :Version   : 0.1.1                *)
(* :Developer : Kirill Belov         *)


(* ::Section:: *)
(*Begin package*)


BeginPackage["TelegramBot`TalkBot`", {"TelegramBot`API`", "TelegramBot`PLI`"}]


(* ::Section:: *)
(*Clear names*)


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Public names declaration*)


TalkBot::usage = 
"TalkBot[token]"


(* ::Section:: *)
(*Begin private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Internal functions and variables*)


(* ::Section:: *)
(*Default fields*)


$hash = Function[token, Hash[1, "SHA256", "Base36String"]]


$webhook = Function[token, "talkbot/" <> $hash[token] <> "/webhook"]


$lexis = Function[token, "talkbot/" <> $hash[token] <> "/lexis"]


$grammar = grammarCreate


$tokens = grammarTokens


$dialog = <||>


$task = None


$time = {1, 60 * 60 * 24 * 7}


$loop = Function[object, 
	Block[{bot = object["bot"], handler = object["handler"], updates}, 
		updates = getUpdates[bot]; 
		If[updates["ok"] && updates["result"] =!= {}, 
			Table[handler[object, update], {update, updates}];
			getUpdates[bot, "offset" -> updates[["result", -1, "update_id"]] + 1];
		]
	]
]


$api = Function[object, 
	With[{handler = object["handler"]}, 
		APIFunction[{}, 
			handler[object, ImportString[HTTPRequestData["Body"], "RawJSON"]]&
		]
	]
]


$handler = Function[{object, update}, 
	Block[{
		bot = object["bot"], 
		lexis = object["lexis"], 
		text = update["message", "text"], 
		chatId = update["message", "chat", "id"], 
		reply
	}, 
		reply = GrammarApply[CloudObject[lexis], text];
		sendMessage[bot, chatId, ToString[reply]]
	]
]


(* ::Text:: *)
(*TalkBot constructor*)


SetAttributes[TalkBot, HoldFirst]


TalkBot[token_?StringQ, OptionsPattern[]] := 
	With[{object = Unique["TalkBot`Objects$"]}, 
		Block[{talkBot = TalkBot[object]}, 
			object = <||>;
			
			object["bot"] = TelegramBot[token]; 
			object["webhook"] = $webhook[token]; 
			object["lexis"] = $lexis[token]; 
			object["dialog"] = $dialog; 
			object["tokens"] = $tokens; 
			object["grammar"] = $grammar; 
			object["task"] = $task; 
			object["time"] = $time; 
			object["loop"] = $loop; 
			object["api"] = $api; 
			object["handler"] = $handler; 
			
			Return[talkBot]
		]
	]


TalkBot[object_?AssociationQ][] := 
	object


TalkBot[object_?AssociationQ][key_String] := 
	object[key]


TalkBot[object_?AssociationQ][key_Symbol] := 
	object[SymbolName[key]]


TalkBot /: CloudDeploy[TalkBot[object_?AssociationQ]] := 
	Block[{
		api = object["api"], 
		webhook = object["webhook"], 
		dialog = object["dialog"], 
		lexis = object["lexis"], 
		tokens = object["tokens"], 
		grammar = object["grammar"]
	}, 
		<|
			"lexis" -> CloudDeploy[grammar[dialog, tokens], lexis, Permissions -> "Public"], 
			"webhook" -> CloudDeploy[api[object], webhook, Permissions -> "Public"]
		|>
	]


TalkBot /: Set[TalkBot[object_Symbol?AssociationQ][field_String, keys___], value_] := 
	object[field, keys] = value


TalkBot /: Set[name_Symbol, bot_TalkBot] := 
	Block[{TalkBot}, 
		
		ClearAll[name];
		
		SetAttributes[TalkBot, HoldFirst]; 
		
		name = bot;
		
		name /: Set[name[field_String, keys___], value_] := 
			bot[field, keys] = value; 
		
		name /: Set[name, value_] := 
			(Clear[name]; name = value);
	]


(* ::Section:: *)
(*End private context*)


End[]


(* ::Section:: *)
(*From change protection*)


Protect["`*"]


(* ::Section:: *)
(*End package*)


EndPackage[] (*TelegramBot`TalkBot`*)