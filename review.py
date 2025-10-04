# REQUIRES: pip install google-genai dotenv
#
# This script will pipe all my essays through an LLM to check punctuation and grammar mistakes

import os
import json
from google import genai
from google.genai import types
import sys
import subprocess
from dotenv import load_dotenv

load_dotenv()


if "__PROXIED__" not in sys.argv:
    print("Starting proxy...")
    subprocess.run(["proxychains", "-q", "python", "review.py"] + ["__PROXIED__"])
    sys.exit()


posts_dir = "./posts"
client = genai.Client()

system_instruction = (
    "You are a university literature teacher. "
    "Analyze the essay and return a strict JSON array of problems, "
    "or an empty array if the essay is flawless. "
    "The essay style is informal internet writing. "
)

schema = types.Schema(
    type=types.Type.ARRAY,
    items=types.Schema(
        type=types.Type.OBJECT,
        properties={
            "sentence": types.Schema(type=types.Type.STRING),
            "explanation": types.Schema(type=types.Type.STRING),
            "improved_sentence": types.Schema(type=types.Type.STRING),
            "type": types.Schema(
                type=types.Type.STRING,
                enum=["grammar", "punctuation", "spelling", "word choice", "clarity"],
            ),
        },
        required=["type", "sentence", "explanation", "improved_sentence"],
    ),
)

for filename in os.listdir(posts_dir):
    if not filename.endswith(".md"):
        continue

    filepath = os.path.join(posts_dir, filename)

    try:
        with open(filepath, "r", encoding="utf-8") as f:
            essay = f.read()
    except Exception as e:
        print(f"Error reading {filename}: {e}")
        continue

    prompt = f"Critically review the following student essay titled '{filename}'.\n\nEssay:\n{essay}"

    print(f"\n--- Reviewing {filename} ---")

    try:
        response = client.models.generate_content(
            model="gemini-2.5-flash",
            contents=prompt,
            config=types.GenerateContentConfig(
                system_instruction=system_instruction,
                response_mime_type="application/json",
                response_schema=schema,
            ),
        )

        review_data = json.loads(response.text)

        print(f"{len(review_data)} mistakes found:")
        for i, item in enumerate(review_data):
            print(f"\n--- Mistake {i + 1} ---")
            print(item["sentence"])
            print(item["improved_sentence"])
            print("Explanation:", item["explanation"])

    except Exception as e:
        print(f"Failed to review {filename}. JSON parsing or API error: {e}")
