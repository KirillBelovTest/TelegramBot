(* :Package: *)

BeginPackage["KirillBelov`TelegramBot`TelethonClient`"];


TelethonClient::usage = 
"TelethonClient[apiId, apiHash, phoneNumber, sessionName, clientName] creates a new Telethon client with the given parameters.";


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
"telethonSendCodeRequest[client, code] request the code for signing in.";


Begin["`Private`"];


If[!ValueQ[$python], 
    $python := $python = 
    StartExternalSession[{"Python", "Evaluator" -> First[FindExternalEvaluators["Python"]]["Evaluator"]}];
];



End[];


EndPackage[];