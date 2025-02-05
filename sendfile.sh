#!/bin/bash


FILENAME=$(basename "$FILE_PATH")

# Get the file size using macOS compatible stat command. use stat -c%s "$FILE_PATH" otherwise
FILE_SIZE=$(stat -c%s "$FILE_PATH")

# Stage 1: Get an upload URL
UPLOAD_URL_RESPONSE=$(curl -s -F files=@"$FILENAME" -F filename="$FILENAME" -F token=$TOKEN -F length=$FILE_SIZE https://slack.com/api/files.getUploadURLExternal)

UPLOAD_URL=$(echo "$UPLOAD_URL_RESPONSE" | jq -r '.upload_url')
FILE_ID=$(echo "$UPLOAD_URL_RESPONSE" | jq -r '.file_id')

if [ "$UPLOAD_URL" == "null" ]; then
  echo "Error getting upload URL: $UPLOAD_URL_RESPONSE"
  exit 1
fi

export UPLOAD_URL="central-sadly-hawk.ngrok-free.app/upload"

# Stage 2: Upload the file to the provided URL
UPLOAD_RESPONSE=$(curl -s -v -L -X POST \
  -T "$FILE_PATH" \
  -H "Content-Type: application/octet-stream" \
  "$UPLOAD_URL")

if [ $? -ne 0 ]; then
  echo "Error uploading file: $UPLOAD_RESPONSE"
  exit 1
fi

echo "$UPLOAD_RESPONSE"

# Stage 3: Complete the upload, and post the message and the file
COMPLETE_RESPONSE=$(curl -s -X POST \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json;charset=utf-8" \
  -d '{
        "files": [
          {
            "id": "'"$FILE_ID"'"
          }
        ],
        "channel_id": "'"$CHANNEL_ID"'",
        "initial_comment": "Hello file 001"
      }' \
  https://slack.com/api/files.completeUploadExternal)

if [ "$(echo "$COMPLETE_RESPONSE" | jq -r '.ok')" != "true" ]; then
  echo "Error completing upload: $COMPLETE_RESPONSE"
  exit 1
fi

# OPTIONAL Stage 4: Share the uploaded file in a channel, only if it was not performed in the previous stage
# SHARE_RESPONSE=$(curl -s -X POST \
#  -H "Authorization: Bearer $TOKEN" \
#  -H "Content-Type: application/json;charset=utf-8" \
#  -d '{
#        "channel": "'"$CHANNEL_ID"'",
#        "file": "'"$FILE_ID"'",
#        "initial_comment": "Hello file 002"
#      }' \
#  https://slack.com/api/files.sharedPublicURL)
#
#if [ "$(echo "$SHARE_RESPONSE" | jq -r '.ok')" != "true" ]; then
#  echo "Error sharing file: $SHARE_RESPONSE"
#  exit 1
#fi

echo "File successfully uploaded. Is not then check your token scopes"
