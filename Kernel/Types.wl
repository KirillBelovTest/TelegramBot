(* ::Package:: *)

(* ::Chapter:: *)
(*Telegram Bot Types*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`TelegramBot`"]


(* ::Section:: *)
(*Types*)


TelegramBot::usage = 
"TelegramBot[token] telegram bot representation"


(* ::Section:: *)
(*Private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Implementation*)


TelegramBot::mssngprop = 
"Not found property with name \"`1`\""


Options[TelegramBot] = {
	"History" :> CreateDataStructure["RingBuffer", 1024], 
	"Logger" :> Function[{history, request, response}, 
		history["PushBack", <|"Time" -> Now, "Request" -> request, "Response" -> response|>]]
}


SetAttributes[TelegramBot, HoldFirst]


TelegramBot[token_?StringQ, OptionsPattern[]] := 
With[{assoc = Unique["KirillBelov`TelegramBot`Private`Bot$"]}, 
	Module[{bot, info, fileId, name, userPhotos, photo}, 
		assoc = <|
			"Token" -> token, 
			"History" -> OptionValue["History"], 
			"Logger" -> OptionValue["Logger"], 
			"Properties" -> {"Token", "History", "Logger", "Properties"}
		|>; 
		
		bot = TelegramBot[assoc]; 
		
		info = KirillBelov`TelegramBot`getMe[bot]; 
		name = info["result", "username"]; 
		bot["Name"] = name; 
		
		userPhotos = KirillBelov`TelegramBot`getUserProfilePhotos[bot, info["result", "id"]][["result", "photos"]]; 
		fileId = userPhotos[[1, 1, "file_id"]]; 
		photo = KirillBelov`TelegramBot`ImportTelegramFile[bot, fileId];
		bot["Icon"] = photo; 
		
		Return[bot] 
	]
]


TelegramBot[assoc_Symbol?AssociationQ][prop_String] /; 
KeyExistsQ[assoc, prop] := 
assoc[prop]


TelegramBot[assoc_Symbol?AssociationQ][prop_String] /; 
Not[KeyExistsQ[assoc, prop]] := 
(Message[TelegramBot::mssngprop, prop]; Missing[StringTemplate[TelegramBot::mssngprop][prop]])


TelegramBot /: 
Set[TelegramBot[assoc_Symbol?AssociationQ][prop_], value_] := (
	assoc["Properties"] = DeleteDuplicates[Append[assoc["Properties"], prop]]; 
	assoc[prop] = value
)


(bot_TelegramBot)[method_Symbol[args___]] := 
method[bot, args]


TelegramBot /: 
Set[name_Symbol, bot: TelegramBot[assoc_Symbol?AssociationQ]] := Module[{nameString}, 
	ClearAll[name]; 
	nameString = ToString[name]; 
	Block[{TelegramBot}, 
		SetAttributes[TelegramBot, HoldFirst]; name = bot]; 
	name /: Set[name[keys__], value_] := With[{$name = name}, 
		$name[keys] = value; 
		ResourceFunction["AddCodeCompletion"][nameString][bot["Properties"]]; 
		value
	];  
	ResourceFunction["AddCodeCompletion"][nameString][bot["Properties"]]; 
	name
]


TelegramBot /: MakeBoxes[bot_TelegramBot, form: StandardForm | TraditionalForm] := 
BoxForm`ArrangeSummaryBox[
	TelegramBot, 
	bot, 
	bot["Icon"], 
	{{BoxForm`SummaryItem[{"Name: ", bot["Name"]}], SpanFromLeft}}, 
	KeyValueMap[{BoxForm`SummaryItem[{#1 <> ": ", #2}], SpanFromLeft}&, KeyDrop[bot[[1]], {"Name", "Icon"}]], 
	form, 
	"Interpretable" -> Automatic
]


(* ::Section:: *)
(*End private*)


End[]


(* ::Section:: *)
(*End package*)


EndPackage[]
