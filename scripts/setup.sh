#!/usr/bin/env bash
# setup.sh -- one-command setup for a Linux VM. Installs R + all packages + OCR,
# creates the working folders, runs the tests, and tells you the URL to share.
#
#   bash scripts/setup.sh            # install + verify
#   bash scripts/setup.sh --start    # install + verify + start the app now
#
# Re-runnable and safe: skips what's already there.
set -euo pipefail
cd "$(dirname "$0")/.."
PORT="${BSO_PORT:-8100}"

echo "==> Bank Statement OCR — setup"

# 1. R + packages + OCR tools
if ! command -v Rscript >/dev/null 2>&1; then
  echo "==> Installing R and packages (apt)…"
  sudo apt-get update
  sudo apt-get install -y r-base r-cran-shiny r-cran-dt r-cran-yaml \
    r-cran-jsonlite r-cran-openxlsx r-cran-readxl r-cran-pdftools r-cran-magick \
    r-cran-testthat tesseract-ocr poppler-utils
else
  echo "==> R present. Ensuring packages…"
  Rscript -e 'pkgs<-c("shiny","DT","yaml","jsonlite","openxlsx","readxl","pdftools","magick","testthat");
    miss<-pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)];
    if(length(miss)){cat("installing:",paste(miss,collapse=", "),"\n");
      try(install.packages(miss,repos="https://cloud.r-project.org"))}else cat("all packages present\n")'
  command -v tesseract >/dev/null 2>&1 || echo "   (note: tesseract not found — scanned-PDF OCR disabled; text PDF/CSV/Excel still work)"
fi

# 2. Working folders
echo "==> Creating working folders…"
mkdir -p logs out inbox outbox processed failed templates_user

# 3. Verify
echo "==> Running the test suite…"
if Rscript tests/run_tests.R >/tmp/bso_tests.log 2>&1; then
  tail -6 /tmp/bso_tests.log
  echo "==> OK — all tests passed."
else
  tail -20 /tmp/bso_tests.log
  echo "!! Tests reported problems — see above."; exit 1
fi

IP="$(hostname -I 2>/dev/null | awk '{print $1}')"; IP="${IP:-<this-vm>}"
echo
echo "======================================================================"
echo " Ready. Start it with:   bash scripts/start.sh"
echo " Then share this URL:    http://${IP}:${PORT}"
echo " Folder option instead:  schedule 'Rscript scripts/serve_inbox.R'"
echo " Full guide:             docs/operational/README.md"
echo "======================================================================"

if [[ "${1:-}" == "--start" ]]; then
  echo "==> Starting the app…"
  exec Rscript scripts/run_app.R
fi
