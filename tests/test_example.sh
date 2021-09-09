#!/bin/bash

set -e

FILE=$1

MIME=$(file -b --mime-type "$FILE")

if [ "$MIME" != 'application/pdf' ]; then
    echo "MIME: $MIME"
    printf '%s\n' "MIME_CHECK_FAILED" >&2
    exit 1
fi

SIZE=$(stat -c%s $FILE)

if ((("$SIZE" < 1000000)) || (("$SIZE" > 5000000)) ); then
  echo "SIZE: $SIZE"
  printf '%s\n' "SIZE_CHECK_FAILED" >&2
  exit 1
fi