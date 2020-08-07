(* Wolfram Language Package *)

BeginPackage["TelegramBot`PLI`", { "TelegramBot`API`"}]
(* Exported symbols added here with SymbolName::usage *)  


grammarTokens::usage = 
"grammarTokens - default tokens"


grammarCreate::usage = 
"grammarCreate[dialog, tokens]"


grammarRuleCreate


Begin["`Private`"] (* Begin Private Context *) 


grammarTokens = 
	{
		"value" -> "price" | "cost" :> "price", 
		"currency" -> "bitcoin" | "btc" :> "BTC", 
		"currency" -> "litecoin" | "ltc" :> "LTC", 
		"currency" -> "etherium" | "eth" :> "ETH"
	}


grammarRuleCreate[phrase_String, replyfunc_] := 
	Block[{value, currency, 
		names, template
	}, 
		names = Association@StringCases[phrase, {
			"`" ~~ variable: LetterCharacter.. ~~ "`" :> 
			(variable -> Symbol[variable])
		}];
		
		template = StringCases[phrase, {
			"[" ~~ optional: {"'", ",", ".", "?", LetterCharacter}.. ~~ "]" :> 
				OptionalElement[optional], 
	
			"`" ~~ variable: LetterCharacter.. ~~ "`" :> 
				Pattern @@ {Symbol[variable], GrammarToken[variable]}, 
	
				WhitespaceCharacter... ~~ other: {"'", LetterCharacter}.. ~~ WhitespaceCharacter... :> 
			other
		}]; 
		
		With[{s = names}, FixedOrder @@ template :> replyfunc[s]]
	]


grammarCreate[dialog_, tokens_] := 
	GrammarRules[
		KeyValueMap[grammarRuleCreate, dialog], 
		tokens
	]


End[] (*`Private`*)


EndPackage[] (*`PLI`*)