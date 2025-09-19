import asyncio
from telethon import TelegramClient
from telethon.errors import FloodWaitError
from telethon.tl.functions.messages import GetHistoryRequest
from telethon.tl.functions.stats import *

class TelethonClient:
    def __init__(self, api_id, api_hash, phone_number, session_name):
        """Initialize the Telethon client with API ID, API hash, phone number, and session name."""
        print("[TelethonClient] Initialized with session name: " + session_name)
        self.api_id = api_id
        self.api_hash = api_hash
        self.phone_number = phone_number
        self.session_name = session_name
        self.client = TelegramClient(session_name, api_id, api_hash)

    def is_connected(self) -> bool:
        """Check if the client is connected to Telegram"""
        loop = asyncio.get_event_loop()
        result = self.client.is_connected()
        print("[TelethonClient] Check if connection is established: " + str(result))
        return result

    def is_authorized(self) -> bool:
        """Check if the user is authorized"""
        loop = asyncio.get_event_loop()
        result = loop.run_until_complete(self.client.is_user_authorized())
        print("[TelethonClient] Check user authorization: " + str(result))
        return result

    def connect(self) -> None:
        """Connect to Telegram"""
        loop = asyncio.get_event_loop()
        print("[TelethonClient] Connect to Telegram")
        loop.run_until_complete(self.client.connect())

    def disconnect(self) -> None:
        """Disconnect from Telegram"""
        loop = asyncio.get_event_loop()
        print("[TelethonClient] Disconnect from Telegram")
        loop.run_until_complete(self.client.disconnect())

    def send_code_request(self) -> None:
        """Send a code request to the phone number"""
        loop = asyncio.get_event_loop()
        print("[TelethonClient] Send code request")
        loop.run_until_complete(self.client.send_code_request(self.phone_number))

    def sign_in(self, code) -> None:
        """Sign in with the code received"""
        loop = asyncio.get_event_loop()
        print("[TelethonClient] SignIn with code: " + str(code))
        loop.run_until_complete(self.client.sign_in(phone=self.phone_number, code=code))

    def get_messages(self, channel, limit=100) -> list:
        """Get messages from a channel"""
        loop = asyncio.get_event_loop()
        all_messages = []
        offset_id = 0

        print(f"[TelethonClient] Fetching messages from channel: {channel}")

        while True:
            try:
                history = loop.run_until_complete(self.client(GetHistoryRequest(
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

                loop.run_until_complete(asyncio.sleep(3)) # Sleep to avoid hitting the rate limit

            except FloodWaitError as e:
                print(f"[TelethonClient] Flood wait triggered, waiting {e.seconds} seconds.")
                loop.run_until_complete(asyncio.sleep(e.seconds + 5))

            except Exception as e:
                print(f"[TelethonClient] Unexpected error: {e}")
                break

        return [message.to_dict() for message in all_messages]

    def get_statistics(self, channel, message_id) -> list:
        """Get channel statistics for the specified message id."""
        print(f"[TelethonClient] Fetching statistics from channel: {channel}")

        loop = asyncio.get_event_loop()

        channel_entity = loop.run_until_complete(self.client.get_entity(channel))

        return loop.run_until_complete(self.client.get_stats(channel_entity)).to_dict()

    def get_user_info(self, user_id) -> dict:
        """Get user information"""
        loop = asyncio.get_event_loop()
        user_entity = loop.run_until_complete(self.client.get_entity(user_id))
        return user_entity.to_dict()

    def send_file(self, channel, file_path) -> None:
        """Send a file to a channel"""
        loop = asyncio.get_event_loop()
        loop.run_until_complete(self.client.send_file(channel, file_path))

    def send_audio(self, channel, audio_path) -> None:
        """Send an audio file to a channel"""
        loop = asyncio.get_event_loop()
        loop.run_until_complete(self.client.send_file(channel, audio_path))

    def send_video(self, channel, video_path) -> None:
        """Send a video file to a channel"""
        loop = asyncio.get_event_loop()
        loop.run_until_complete(self.client.send_file(channel, video_path))

    def send_photo(self, channel, photo_path) -> None:
        """Send a photo file to a channel"""
        loop = asyncio.get_event_loop()
        loop.run_until_complete(self.client.send_file(channel, photo_path))

    def send_document(self, channel, document_path) -> None:
        """Send a document file to a channel"""
        loop = asyncio.get_event_loop()
        loop.run_until_complete(self.client.send_file(channel, document_path))

    def send_message(self, channel, message) -> None:
        """Send a message to a channel"""
        loop = asyncio.get_event_loop()
        loop.run_until_complete(self.client.send_message(channel, message))

    def get_channel_info(self, channel) -> dict:
        """Get channel information"""
        loop = asyncio.get_event_loop()
        channel_entity = loop.run_until_complete(self.client.get_entity(channel))

    def find_channel_by_username(self, username) -> dict:
        """Find a channel by username"""
        loop = asyncio.get_event_loop()
        channel_entity = loop.run_until_complete(self.client.get_entity(username))
        return channel_entity.to_dict()

    def find_message_by_text(self, channel, text) -> dict:
        """Find a message by text"""
        loop = asyncio.get_event_loop()
        messages = loop.run_until_complete(self.client.get_messages(channel, search=text))