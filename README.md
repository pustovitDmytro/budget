
![Logo](.docs/logo.png "Logo")

## 🇺🇦 Help Ukraine
I woke up on my 26th birthday at 5 am from the blows of russian missiles. They attacked the city of Kyiv, where I live, as well as the cities in which my family and friends live. Now my country is a war zone. 

We fight for democratic values, freedom, for our future! Once again Ukrainians have to stand against evil, terror, against genocide. The outcome of this war will determine what path human history is taking from now on.

💛💙  Help Ukraine! We need your support! There are [dozen ways][ukr-link] to help us, just do it!

## Usage

Run without R studio:

```bash
Rscript -e "rmarkdown::render('src/report.Rmd',params=list(args = myarg))"
```

## Environment
```
SPREADSHEET_URL="https://docs.google.com/spreadsheets/d/XXX/edit?usp=sharing"

SPREADSHEET_EMAIL="<email>@gmail.com"
```

### Show installed packages:
```
Rscript -e 'x <- installed.packages(); x[ is.na(x[,"Priority"]), c("Package", "Version")]'
```

# Docker

1. Authentificate email
```
docker run -it --network=host \
    -v ~/.budget/.secrets:/app/.secrets \
    --env SPREADSHEET_URL="https://docs.google.com/spreadsheets/d/XXX/edit?usp=sharing" \
    --env SPREADSHEET_EMAIL="<email>@gmail.com" \
    pustovitdmytro/budget R -e "source('/app/load_data.R')"
```

2. Generate Report:
```
docker run -it \
    -v ~/.budget/.secrets:/app/.secrets \
    -v ~/.budget/files:/app/files \
    --env SPREADSHEET_URL="https://docs.google.com/spreadsheets/d/XXX/edit?usp=sharing" \
    --env SPREADSHEET_EMAIL="<email>@gmail.com" \
    pustovitdmytro/budget Rscript -e "rmarkdown::render('/app/report.Rmd',params=list(args = myarg), output_file = 'files/report.pdf')"

```

check `~/budget/files/report.pdf`


# Examples
See spreadsheet sample:
https://docs.google.com/spreadsheets/d/16TUXk_thxH8JkbdlAdFe9q8s3f6PDZiq1zXvecUp-eM/edit?usp=sharing

Sample spreadsheet will result in next [report](./tests/example.pdf)

[ukr-badge]: https://img.shields.io/badge/made_in-ukraine-ffd700.svg?labelColor=0057b7
[ukr-link]: https://war.ukraine.ua
