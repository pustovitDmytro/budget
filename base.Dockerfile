FROM rocker/r-ver:3.6.3

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    wget \
    pandoc \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libjpeg-dev \
    libz-dev \
    zlib1g-dev \
    libgit2-dev \
    libcairo2-dev \
    libudunits2-dev \
    libmagick++-dev \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    lmodern

WORKDIR /bin

COPY src/install.R /bin/install.R

RUN R -e "source('/bin/install.R')"
