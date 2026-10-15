#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
exec Rscript --vanilla scripturi/actualizare_programata.R
