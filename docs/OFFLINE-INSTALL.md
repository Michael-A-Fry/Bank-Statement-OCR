# Offline install — for a locked-down PC with no internet

Two files do it. You run one on an internet **laptop**, drag one folder across,
and run the other on the offline **Windows PC**. No compiling, no Rtools — you're
moving prebuilt Windows binaries.

## The one rule that makes it work
Windows R packages are built **per R minor version**, so the laptop that
*downloads* and the PC that *installs* must run the **same R x.y** (e.g. both
**4.6**), on **Windows x86_64**. Since you're installing R fresh on the PC, use
**4.6 on both** and it's foolproof. `install-on-pc.R` says so plainly if they ever
don't match.

---

## On the laptop (has internet)
From the repo folder, under **R 4.6**, run one command:
```sh
Rscript scripts/bundle-offline.R
```
It builds a single **`bso-offline/`** folder containing:
- `repo/` — every R package + all dependencies (Windows binaries)
- `prereqs/` — the R 4.6 installer, plus the Tesseract and Poppler installers
  (best effort; if a download is blocked it prints the URL to grab by hand)
- `install-on-pc.R` — the PC-side script
- `packages.txt` — the list, for reference

Then **drag the whole `bso-offline` folder to the PC** (USB / share), along with
the **app folder** (this repo as a ZIP).

---

## On the offline PC (no internet)
1. If the PC has no R yet, run the **R 4.6 installer** from `bso-offline\prereqs\`.
2. Open a terminal in the `bso-offline` folder and run:
   ```sh
   Rscript install-on-pc.R
   ```
   It installs all R packages from `repo/` (no internet), unzips **Poppler** and
   adds it to your user PATH, and points you at the **Tesseract** installer.
3. For **scanned-PDF OCR**, run the Tesseract installer it named (tick *Add to
   PATH*). Open a new terminal and confirm:
   ```sh
   tesseract --version
   pdftoppm -h
   ```
   Text PDFs, CSV and Excel work without these two.
4. Verify, from the app folder:
   ```sh
   Rscript tests\run_tests.R          # expect: failed: 0
   ```

From here, `docs/SETUP-AND-DEPLOYMENT.md` covers running it for the team and
`scripts/install-service.ps1` makes it auto-start on boot.

---

## Notes
- **Different R version on the PC?** Install that same version on the laptop, run
  `scripts/bundle-offline.R` under it, and carry the bundle over. The rule is only
  ever: *bundle under the same R x.y the PC runs.*
- **Blocked downloads on the laptop?** The package `repo/` is the essential part
  and is fully automated. Only the three system installers are best-effort; if any
  is skipped, the script prints exactly where to download it, and you drop it into
  `bso-offline\prereqs\` yourself.
