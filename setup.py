from setuptools import setup, find_packages

setup(
    name="telethon-client",
    version="0.1.0",
    packages=find_packages(where="Python"),
    package_dir={"": "Python"},
    install_requires=[
        "telethon>=1.33.1"
    ],
    author="Kirill Belov",
    author_email="kirillbelovtest@gmail.com",
    description="Python telethon client package for Wolfram Language TelegramBot integration",
    url="https://github.com/kirillbelovtest/telegrambot",
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: MIT",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.7',
)