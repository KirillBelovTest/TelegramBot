(* :Package: *)

BeginPackage["KirillBelov`TelegramBot`PythonVenv`"];


Begin["`Private`"];


(*
    python -m venv .venv
    .\.venv\Scripts\Activate.ps1
    pip install -r requirements.txt
    pip install -e .
*)


Module[{
    directory, venv, 
    shellSpec, cmds, 
    sep, script
},
    directory = DirectoryName[$InputFileName, 2]; 
    venv = FileNameJoin[{directory, ".venv"}];

    If[!FileExistsQ[venv],

        If[$OperatingSystem === "Windows",
            shellSpec = {"powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-Command"},   
        (* Unix / macOS: bash *)
            shellSpec = {"bash", "-c"}
        ];

        If[$OperatingSystem === "Windows",
            cmds = {
                "python -m venv .venv",
                ".\\.venv\\Scripts\\Activate.ps1",
                "pip install -r requirements.txt",
                "pip install -e ."
            },
        (* Unix / macOS *)
            cmds = {
                "python -m venv .venv",
                "source .venv/bin/activate",
                "pip install -r requirements.txt",
                "pip install -e ."
            }
        ];

        sep = "; ";
        script = StringRiffle[cmds, sep];

        RunProcess[
            Join[shellSpec, {script}],
            All, 
            ProcessDirectory -> directory
        ]
    ]
];


End[];


EndPackage[];