#!/usr/bin/env bash
set -euo pipefail

# Bootstrap a fresh Ubuntu 24.04 ECS for the one-time core-artifact build.
# Installs exact R 4.5.0 from the official CRAN source tarball and the system
# libraries needed by this repository's R packages. Safe to re-run.

R_VERSION="4.5.0"
R_PREFIX="/opt/R/${R_VERSION}"
R_TARBALL="R-${R_VERSION}.tar.gz"
R_URL="https://cran.r-project.org/src/base/R-4/${R_TARBALL}"
BUILD_JOBS="${R_BUILD_JOBS:-32}"

sudo apt-get update
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
  build-essential gfortran git curl wget ca-certificates unzip zip tmux htop \
  pkg-config make gcc g++ \
  libreadline-dev libbz2-dev liblzma-dev libpcre2-dev zlib1g-dev \
  libcurl4-openssl-dev libssl-dev libxml2-dev libicu-dev \
  libblas-dev liblapack-dev \
  libcairo2-dev libfontconfig1-dev libfreetype6-dev \
  libharfbuzz-dev libfribidi-dev libpng-dev libjpeg-dev libtiff-dev \
  libgit2-dev libssh2-1-dev

if [[ ! -x "${R_PREFIX}/bin/R" ]]; then
  work="$(mktemp -d)"
  trap 'rm -rf "$work"' EXIT
  cd "$work"
  curl -fL --retry 5 --retry-delay 2 -o "$R_TARBALL" "$R_URL"
  tar -xf "$R_TARBALL"
  cd "R-${R_VERSION}"
  ./configure \
    --prefix="$R_PREFIX" \
    --enable-R-shlib \
    --with-x=no \
    --with-blas \
    --with-lapack
  make -j"$BUILD_JOBS" -O
  sudo make install
fi

sudo ln -sfn "${R_PREFIX}/bin/R" /usr/local/bin/R
sudo ln -sfn "${R_PREFIX}/bin/Rscript" /usr/local/bin/Rscript

printf 'R: '
Rscript -e 'cat(as.character(getRversion()), "\n")'
if [[ "$(Rscript -e 'cat(as.character(getRversion()))')" != "$R_VERSION" ]]; then
  echo "ERROR: expected R ${R_VERSION}" >&2
  exit 1
fi

echo "Bootstrap complete. Next: clone/pull the repo, upload data/raw, then run scripts/run_core_artifacts.sh"
