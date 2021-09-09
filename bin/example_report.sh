#!/bin/bash

set -e

docker run \
   -v ~/.budget/.secrets:/app/.secrets \
   -v ~/.budget/files:/app/files \
   -v ~/.budget/plots:/app/plots \
   --env SPREADSHEET_URL="$SPREADSHEET_URL" \
   --env SPREADSHEET_EMAIL="$SPREADSHEET_EMAIL" \
    $IMAGE_NAME Rscript -e "rmarkdown::render('/app/report.Rmd',params=list(args = myarg), output_file = 'files/$OUT_FILE')"

ls -lah ~/.budget/files