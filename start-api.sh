#!/bin/bash


stack exec hamocrm-exe -- -p "$PORT" -u "$USER" -i "$CLIENT_ID" -s "$CLIENT_SECRET" -r "$REDIRECT_URL" &

