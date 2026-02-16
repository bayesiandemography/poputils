# syntax=docker/dockerfile:1
#
# Build + check an R package under R 4.2.0 using remotes (not pak)
#
# Usage (from package root):
#   docker build -t poputils-r420 .
#   docker run --rm poputils-r420
#
FROM rocker/r-ver:4.2.0

ENV DEBIAN_FRONTEND=noninteractive
ENV _R_CHECK_CRAN_INCOMING_=false

# System requirements for common R packages and compiling cpp11 code
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    make \
    gfortran \
    git \
    pkg-config \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    pandoc \
    pandoc-citeproc \
  && rm -rf /var/lib/apt/lists/*

# remotes for dependency installation
RUN R -q -e "install.packages('remotes', repos='https://cloud.r-project.org')"

WORKDIR /pkg
COPY . /pkg

RUN command -v pandoc >/dev/null && pandoc --version | head -n 2 && \
    command -v pandoc-citeproc >/dev/null && pandoc-citeproc --version

# Install deps from DESCRIPTION (Imports/Depends/LinkingTo/Suggests)
RUN R -q -e " \
  options(repos = c(CRAN='https://cloud.r-project.org')); \
  remotes::install_deps(dependencies = TRUE, upgrade = 'never') \
"

# Build + check
CMD ["bash","-lc","set -euo pipefail; \
  R -q -e \"cat('R version: ', R.version.string, '\\n')\"; \
  R CMD build .; \
  TARBALL=$(ls -1t *.tar.gz | head -n 1); \
  _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran \"$TARBALL\""]