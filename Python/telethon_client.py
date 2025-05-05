import asyncio
from telethon import TelegramClient
from telethon.errors import FloodWaitError
from telethon.tl.functions.messages import GetHistoryRequest

class TelethonClient:
    def __init__(self, session_name, api_id, api_hash, phone_number):
        self.session_name = session_name
        self.api_id = api_id
        self.api_hash = api_hash
        self.phone_number = phone_number

    def connect(self):
        loop = asyncio.get_event_loop()
        with TelegramClient(self.session_name, self.api_id, self.api_hash) as client:
            client.connect()
            if not client.is_connected():
                print("[Telethon] Connect to Telegram")
                loop.run_until_complete(client.connect())

            if not client.is_user_authorized():
                print("[Telethon] Send code request to", self.phone_number)
                loop.run_until_complete(client.send_code_request(phone=self.phone_number))

    def disconnect(self):
        loop = asyncio.get_event_loop()
        with TelegramClient(self.session_name, self.api_id, self.api_hash) as client:
            client.connect()
            loop.run_until_complete(client.disconnect())

    def sign_in(self, code):
        loop = asyncio.get_event_loop()
        self.connect()
        with TelegramClient(self.session_name, self.api_id, self.api_hash) as client:
            client.connect()
            loop.run_until_complete(client.sign_in(phone=self.phone_number, code=code))

    def get_messages(self, channel):
        loop = asyncio.get_event_loop()
        self.connect()

        all_messages = []
        offset_id = 0
        limit = 100

        with TelegramClient(self.session_name, self.api_id, self.api_hash) as client:
            client.connect()
            while True:
                try:
                    history = loop.run_until_complete(client(GetHistoryRequest(
                        peer=channel,
                        offset_id=offset_id,
                        offset_date=None,
                        add_offset=0,
                        limit=limit,
                        max_id=0,
                        min_id=0,
                        hash=0
                    )))

                    if not history.messages:
                        break

                    all_messages.extend(history.messages)
                    offset_id = history.messages[-1].id

                    loop.sleep(3)  # Sleep to avoid hitting the rate limit

                except FloodWaitError as e:
                    print(f"[Telethon] Flood wait triggered, waiting {e.seconds} seconds.")
                    loop.sleep(e.seconds + 5)

                except Exception as e:
                    print(f"[Telethon] Unexpected error: {e}")
                    break

        return [message.to_dict() for message in all_messages]
