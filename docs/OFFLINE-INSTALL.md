# Offline install - for a locked-down server with no internet

**Two double-clicks.** You build a package on an internet PC, copy one folder to
the air-gapped server, and double-click one file. No compiling, no Rtools, no
typing - you're moving prebuilt Windows binaries.

## The one rule that makes it work
Windows R packages are built **per R minor version**, so the PC that *builds* the
package and the server that *runs* it must use the **same R x.y** (e.g. both
**4.6**), on **Windows x86_64**. Since R is installed fresh on the server from the
package itself, just use **the same R x.y on the build PC** and it's foolproof.
`RUN-ME.bat` reports plainly if they ever don't match.

---

## Step 1 - Build the package (on a PC that HAS internet)
Put this repo on a normal Windows PC with internet and R installed (same R x.y the
server will run). **Double-click `make-bundle.bat`.**

It assembles a single self-contained folder, **`StatementStudio-offline`**, that
holds the whole app plus everything it needs to install itself offline:
- `RUN-ME.bat` - the only thing you run on the server
- the app (`app.R`, `R/`, `templates/`, `config/`, ...)
- `offline/repo/` - every R package + all dependencies (Windows binaries)
- `offline/prereqs/` - the R installer, plus the Tesseract and Poppler installers
  (best effort; if a download is blocked it prints the URL to grab by hand)
- `offline/install-on-pc.R`, `offline/packages.txt` - the install step + the list

(Prefer the command line? `Rscript scripts\bundle-offline.R` does the same thing.)

---

## Step 2 - Copy it across
Copy the whole **`StatementStudio-offline`** folder to the server (USB / share).
That one folder is everything - there is nothing else to carry.

---

## Step 3 - Run it (on the offline server)
Open the folder and **double-click `RUN-ME.bat`.** On the **first** run it, with no
internet:
1. installs **R** silently from `offline\prereqs\` if the server has none,
2. installs every **R package** from `offline\repo\`,
3. sets up **Poppler** and silent-installs **Tesseract** for scanned-PDF OCR
   (both added to PATH),
4. creates `config\config.yaml` from the example,
5. **starts the app** and prints the `http://...:8100` URL to share.

Every run after that just starts the app. Leave the window open; Ctrl-C stops it.
(Text PDFs, CSV and Excel work even without Poppler/Tesseract - those two are only
for scanned-image PDFs.)

To keep it always-up across reboots, register it as a service - see
`docs/SETUP-AND-DEPLOYMENT.md` (Task Scheduler "at startup" running
`scripts\run_app.R`).

---

## Notes
- **Different R version on the server?** Install that same version on the build PC,
  double-click `make-bundle.bat` under it, and carry the folder over. The rule is
  only ever: *build under the same R x.y the server runs.*
- **Blocked downloads on the build PC?** The package `offline\repo\` is the
  essential part and is fully automated. Only the three system installers are
  best-effort; if any is skipped, the build prints exactly where to download it,
  and you drop it into `StatementStudio-offline\offline\prereqs\` yourself before
  copying the folder across.
- **No installer in `prereqs\`?** If the R download was blocked and the server has
  no R, `RUN-ME.bat` says so and points you back here. Add `R-x.y-win.exe` to
  `offline\prereqs\` and run it again.
