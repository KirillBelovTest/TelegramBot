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
    "Version" -> "1.0.4",
    "WolframVersion" -> "13+",
    "Dependencies" -> {},
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {"KirillBelov`TelegramBot`", "Init.wl"},
          {
            "KirillBelov`TelegramBot`API`",
            "API.wl"
          },
          {
            "KirillBelov`TelegramBot`Types`",
            "Types.wl"
          },
          {
            "KirillBelov`TelegramBot`Extensions`",
            "Extensions.wl"
          }
        },
        "Symbols" -> {
          "KirillBelov`TelegramBot`CreateLongPollBot",
          "KirillBelov`TelegramBot`deleteWebhook",
          "KirillBelov`TelegramBot`DeployWebhook",
          "KirillBelov`TelegramBot`getFile",
          "KirillBelov`TelegramBot`getMe",
          "KirillBelov`TelegramBot`getUpdates",
          "KirillBelov`TelegramBot`getUserProfilePhotos",
          "KirillBelov`TelegramBot`getWebhookInfo",
          "KirillBelov`TelegramBot`HandleBotUpdates",
          "KirillBelov`TelegramBot`ImportTelegramFile",
          "KirillBelov`TelegramBot`sendMessage",
          "KirillBelov`TelegramBot`setWebhook",
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
