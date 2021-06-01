FROM rocker/r-ver:3.6.3

RUN mkdir /app 

WORKDIR /app

COPY . /app

RUN R -e "source('/app/install.R')"
