(* :Package: *)

BeginPackage["KirillBelov`TelegramBot`TelethonClient`"];


TelethonClient::usage = 
"TelethonClient[apiId, apiHash, phoneNumber, sessionName] creates a new Telethon client with the given parameters.";


telethonConnectedQ::usage =
"telethonConnectedQ[client] check connection.";


telethonAuthorizedQ::usage =
"telethonAuthorizedQ[client] check if the client is authorized.";


telethonConnect::usage =
"telethonConnect[client] connects the Telethon client to the Telegram servers.";


telethonDisconnect::usage =
"telethonDisconnect[client] disconnects from the Telegram servers.";


telethonSendCodeRequest::usage =
"telethonSendCodeRequest[client] request the code for signing in.";


telethonSignIn::usage =
"telethonSignIn[client, code] sign in using code.";


telethonGetMessages::usage =
"telethonGetMessages[client, channel] get messages from the specified channel.";


telethonGetStatistics::usage = 
"telethonGetStatistics[client, channel, messageId] get statistics for th message.";


Begin["`Private`"];


If[!AssociationQ[$clients], 
    $clients = <||>
];


TelethonClient[apiId_, apiHash_, phoneNumber_, sessionName_] := 
With[{id = "telethon_client_" <> 
    ToString[Hash[{apiId, apiHash, phoneNumber, sessionName}]]
}, 
    If[!KeyExistsQ[$clients, id], 
        $clients[id] = TelethonClient[<|
            "id" -> id, 
            "api_id" -> apiId, 
            "api_hash" -> apiHash, 
            "phone_number" -> phoneNumber, 
            "session_name" -> sessionName
        |>]; 

        py @ StringTemplate[
"
from telethon_client import TelethonClient
`id` = TelethonClient(`api_id`, '`api_hash`', '`phone_number`', '`session_name`')
"
        ][$clients[id][[1]]]
    ]; 

    $clients[id]
];


toArg[s_String] := "'" <> s <> "'"; 


toArg[s_Symbol] := ToString[s]; 


toArg[s_?NumberQ] := ToString[s]; 


TelethonClient[assoc_?AssociationQ][(type_Symbol)[args_?AssociationQ, serializer_: None]] := 
py @ 
StringTemplate["import asyncio
import telethon.tl.functions.stats as stats
import telethon.tl.functions.messages as messages
import telethon.tl.functions.channels as channels
asyncio.get_event_loop().run_until_complete(`id`.client(`Type`(`args`)))`serializer`"] @ 
Join[assoc, <|
    "Type" -> StringTrim[Context[type], "`"] <> "." <> SymbolName[type], 
    "args" -> StringRiffle[KeyValueMap[#1 <> "=" <> toArg[#2]&, args], ", "], 
    "serializer" -> If[serializer === None, "", "." <> serializer <> "()"]
|>];


telethonConnectedQ[TelethonClient[assoc_?AssociationQ]] := 
py @ StringTemplate["`id`.is_connected()"] @ assoc;


telethonAuthorizedQ[TelethonClient[assoc_?AssociationQ]] := 
py @ StringTemplate["`id`.is_authorized()"] @ assoc;


telethonConnect[client: TelethonClient[assoc_?AssociationQ]] := 
py @ StringTemplate["`id`.connect()"] @ assoc;


telethonDisconnect[TelethonClient[assoc_?AssociationQ]] := 
py @ StringTemplate["`id`.disconnect()"] @ assoc;


telethonSendCodeRequest[TelethonClient[assoc_?AssociationQ]] := 
py @ StringTemplate["`id`.send_code_request()"] @ assoc;


telethonSignIn[TelethonClient[assoc_?AssociationQ], code: _String | _Integer] := 
py @ StringTemplate["`id`.sign_in(`code`)"] @ Append[assoc, "code" -> code];


telethonGetMessages[TelethonClient[assoc_?AssociationQ], username_String] := 
py @ StringTemplate["`id`.get_messages('`username`')"] @ Append[assoc, "username" -> username];


telethonGetStatistics[TelethonClient[assoc_?AssociationQ], username_String, messageId: _String | _Integer] := 
py @ StringTemplate["`id`.get_statistics('`username`', `message_id`)"] @ 
Join[assoc, <|"username" -> username, "message_id" -> messageId|>];


With[{directory = DirectoryName[$InputFileName, 2]}, 
    If[!ValueQ[$python], 
        $python := $python = 
        StartExternalSession[{"Python", 
            "Evaluator" -> FileNameJoin[{directory, ".venv", "Scripts", "python.exe"}]
        }];
    ]
];


py[code_String] := 
ExternalEvaluate[$python, code];


End[];


EndPackage[];