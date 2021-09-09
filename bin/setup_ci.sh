#!/bin/bash

set -e

mkdir -p ~/.budget/.secrets
cd ~/.budget/.secrets
wget $GOOGLE_CREDENTIALS_FILE_URL
ls -lah

