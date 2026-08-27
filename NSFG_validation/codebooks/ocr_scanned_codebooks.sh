#!/usr/bin/env bash
# Re-OCR the scanned NSFG codebooks (1973, 1976, 1982) -> <base>_ocr.txt
# next to each PDF. The other codebooks are already digital text; skip them.
#
# macOS one-time setup:   brew install tesseract poppler
# Run (from this folder):  bash ocr_scanned_codebooks.sh
set -uo pipefail
cd "$(dirname "$0")"

# --- preflight: fail loudly if a tool is missing (instead of empty output) ---
missing=""
for c in tesseract pdftoppm pdfinfo; do command -v "$c" >/dev/null 2>&1 || missing="$missing $c"; done
if [ -n "$missing" ]; then
  echo "ERROR: missing command(s):$missing" >&2
  echo "Install with:  brew install tesseract poppler" >&2
  exit 1
fi
if ! tesseract --list-langs 2>/dev/null | grep -qx eng; then
  echo "ERROR: tesseract has no 'eng' language data. Try: brew reinstall tesseract" >&2
  exit 1
fi

DPI=300
JOBS=$(sysctl -n hw.logicalcpu 2>/dev/null || nproc)
export OMP_THREAD_LIMIT=1

for pdf in 1973_codebook.pdf 1976_codebook.pdf 1982_codebook.pdf; do
  [ -f "$pdf" ] || { echo "skip (not found): $pdf"; continue; }
  base="${pdf%.pdf}"; wd="$(mktemp -d)"
  n=$(pdfinfo "$pdf" | awk '/^Pages:/{print $2}')
  echo "OCR $pdf : $n pages on $JOBS cores..."
  seq 1 "$n" | xargs -P"$JOBS" -I{} bash -c '
    p="$1"; pdf="$2"; wd="$3"; dpi="$4"; pp=$(printf "%04d" "$p")
    pdftoppm -f "$p" -l "$p" -r "$dpi" -png "$pdf" "$wd/i$p"
    img=$(ls "$wd/i$p"-*.png 2>/dev/null | head -1)
    [ -n "$img" ] && tesseract "$img" "$wd/page-$pp"
    rm -f "$wd/i$p"-*.png' _ {} "$pdf" "$wd" "$DPI" 2>"$wd/errors.log"
  if ! ls "$wd"/page-*.txt >/dev/null 2>&1; then
    echo "ERROR: no pages were OCR'd for $pdf. First errors:" >&2
    head -8 "$wd/errors.log" >&2
    rm -rf "$wd"; continue
  fi
  cat "$wd"/page-*.txt > "${base}_ocr.txt"
  echo "  -> ${base}_ocr.txt ($(wc -l < "${base}_ocr.txt") lines)"
  rm -rf "$wd"
done
echo "Done."
