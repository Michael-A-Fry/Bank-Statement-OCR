# Offline install — for a locked-down PC with no internet

You download everything once on an internet-connected **laptop**, carry it over on
a USB stick / share, and install on the **offline Windows PC**. No compiling, no
Rtools — you are just moving prebuilt Windows binaries.

## The one rule that makes it work
Windows R packages are built **per R minor version**, so the laptop that
*downloads* and the PC that *installs* must run the **same R x.y** (e.g. both
**4.6**) on **Windows x86_64**. Do this and everything below just works; skip it
and the packages won't load on the PC.

So: pick R **4.6** for both. You'll install R 4.6 on the PC, and you run the
bundling step on the laptop's R 4.6.

---

## What to carry over (4 things)
1. **R 4.6 installer** for Windows — from CRAN's "R for Windows" download page.
2. **The R packages bundle** — produced by the script in Step 2 below.
3. **The app folder** — this repo as a ZIP (`R/`, `templates/`, `app.R`, `scripts/`).
4. **(Only for scanned-PDF OCR)** Tesseract + Poppler for Windows — see Step 5.
   Text PDFs, CSV and Excel need nothing here.

---

## On the laptop (has internet)

### Step 1 — get R 4.6
Install R 4.6 on the laptop if it isn't already, and also **save the R 4.6
Windows installer file** to carry to the PC.

### Step 2 — bundle the R packages (one command)
From the repo folder, under **R 4.6**:
```sh
Rscript scripts/bundle-offline.R
```
This resolves all ~40 dependencies and downloads the Windows binaries into a new
**`bso-offline/`** folder (a self-contained local package repo). Copy that whole
folder to the PC.

### Step 3 — grab the app + (optional) OCR tools
- ZIP this repo (or `git archive`) to carry the app folder.
- If you need **scanned-PDF OCR**, also download for Windows:
  - **Tesseract OCR** — the UB Mannheim Windows installer
    (`github.com/UB-Mannheim/tesseract/wiki`).
  - **Poppler for Windows** — a binaries zip
    (e.g. `github.com/oschwartz10612/poppler-windows/releases`); you'll add its
    `bin\` to PATH on the PC.

---

## On the offline PC (no internet)

### Step 4 — R + packages + app
1. Run the **R 4.6 installer** you carried over.
2. Unzip the **app folder** somewhere stable, e.g. `C:\BankStatements\`.
3. Copy the **`bso-offline`** folder in beside it (so `C:\BankStatements\bso-offline\`).
4. Install the packages from the local bundle (one command, no internet):
   ```sh
   cd C:\BankStatements
   Rscript scripts\install-offline.R
   ```
   It prints `Installed 9/9 packages` and, if anything is missing, tells you the
   likely cause (usually an R-version mismatch — rebuild the bundle under the R
   version the PC actually has).
5. Verify:
   ```sh
   Rscript tests\run_tests.R          # expect: failed: 0
   ```

### Step 5 — Tesseract + Poppler (only for scanned PDFs)
1. Run the **Tesseract** installer.
2. Extract **Poppler** and add its `bin\` folder to the system **PATH**.
3. Confirm in a new terminal:
   ```sh
   tesseract --version
   pdftoppm -h
   ```
   Both found → scanned-PDF OCR is enabled. (The app auto-detects them; without
   them, text PDFs / CSV / Excel still work.)

That's it. From here, `docs/SETUP-AND-DEPLOYMENT.md` covers running it for the
team and `scripts/install-service.ps1` makes it auto-start on boot.

---

## If the PC must run a different R version
If the PC can't run 4.6 (corporate standard is, say, 4.4), install **that same
version on the laptop**, run `scripts/bundle-offline.R` under it, and carry the
bundle over. The rule is only ever: *bundle under the same R x.y the PC runs.*
`install-offline.R` reports a mismatch clearly if you get it wrong.
