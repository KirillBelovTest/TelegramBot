(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "KirillBelov/TelegramBot",
    "Description" -> "Telegram Bot API client for the Wolfram Language",
    "Creator" -> "Kirill Belov <kirillbelovtest@gmail.com>",
    "URL" -> "https://resources.wolframcloud.com/PacletRepository/resources/KirillBelov/TelegramBot",
    "SourceControlURL" -> "https://github.com/KirillBelovTest/TelegramBot",
    "License" -> "MIT",
    "PublisherID" -> "KirillBelov",
    "Version" -> "1.0.16",
    "WolframVersion" -> "13+",
    "Dependencies" -> {},
    "PrimaryContext" -> "KirillBelov`TelegramBot`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {
			"KirillBelov`TelegramBot`", 
            "TelegramBot.wl"
          },
          {
            "KirillBelov`TelegramBot`API`",
            "API.wl"
          },
          {
            "KirillBelov`TelegramBot`Type`",
            "Type.wl"
          },
          {
            "KirillBelov`TelegramBot`Extensions`",
            "Extensions.wl"
          }
        },
        "Symbols" -> {
          "KirillBelov`TelegramBot`CreateBotSession",
          "KirillBelov`TelegramBot`DeployBotWebhook",
          "KirillBelov`TelegramBot`getMe",
          "KirillBelov`TelegramBot`getUpdates",
          "KirillBelov`TelegramBot`HandleBotUpdates",
          "KirillBelov`TelegramBot`ImportTelegramFile",
          "KirillBelov`TelegramBot`sendMessage",
          "KirillBelov`TelegramBot`sendPhoto",
          "KirillBelov`TelegramBot`TelegramBot"
        }
      },
      {
        "Documentation",
        "Root" -> "Documentation",
        "Language" -> "English"
      }
    }
  |>
]
