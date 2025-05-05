(* :Package: *)

BeginPackage["KirillBelov`TelegramBot`TelethonClient`"];


TelethonClient::usage = 
"TelethonClient[apiId, apiHash, phoneNumber, sessionName] creates a new Telethon client with the given parameters.";


telethonConnect::usage =
"telethonConnect[client] connects the Telethon client to the Telegram servers.";


telethonDisconnect::usage =
"telethonDisconnect[client] disconnects from the Telegram servers.";


telethonIsConnected::usage =
"telethonIsConnected[client] check connection.";


telethonIsUserAuthorized::usage =
"telethonIsUserAuthorized[client] check authorization.";


telethonSignIn::usage =
"telethonSignIn[client, code] sign in using code.";


telethonSendCodeRequest::usage =
"telethonSendCodeRequest[client] request the code for signing in.";


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
            "phoneNumber" -> phoneNumber, 
            "sessionName" -> sessionName
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


telethonConnect[TelethonClient[assoc_?AssociationQ]] := 
py @ StringTemplate["`id`.connect()"] @ assoc;


telethonDisconnect[TelethonClient[assoc_?AssociationQ]] := 
py @ StringTemplate["`id`.disconnect()"] @ assoc;


telethonSignIn[TelethonClient[assoc_?AssociationQ], code_String] := 
py @ StringTemplate["`id`.sign_in(`code`)"] @ Append[assoc, "code" -> code];


telethonGetMessages[TelethonClient[assoc_?AssociationQ], username_String] := 
py @ StringTemplate["`id`.get_messages(`username`)"] @ Append[assoc, "username" -> username];


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