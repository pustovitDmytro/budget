FROM rocker/r-ver:3.6.3

# apt-get update
# apt-get install -y --no-install-recommends wget pandoc

RUN mkdir /app 

WORKDIR /app

COPY . /app

RUN R -e "source('/app/install.R')"

