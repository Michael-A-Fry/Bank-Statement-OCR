# Offline install - for a locked-down server with no internet

**Two double-clicks.** You build a package on an internet PC, copy one folder to
the air-gapped server, and double-click one file. No compiling, no Rtools, no
typing - you're moving prebuilt Windows binaries.

## Isolated by design - the server's existing R is left alone
`RUN-ME.bat` installs its **own private copy of R inside the folder**
(`R-runtime\`) and uses **only** that, with an app-local package library
(`R-lib\`). It is installed non-invasively (it does **not** register as the
machine's R and does **not** grab `.RData` file types), so **any R or RStudio
already on the server - even an old version - is ignored and left exactly as it
is.** Nothing is upgraded, replaced, or removed. To uninstall Statement Studio
later, just delete the folder.

Because the server uses the R that ships **in the bundle**, there is **no version
matching to get right**: the package binaries here were built for that same R.
(Windows R packages are built per R minor version - this is why we ship the R too,
rather than trusting whatever is on the server.)

---

## Step 1 - Build the package (on a PC that HAS internet)
Put this repo on a normal Windows PC with internet and **any recent R** installed.
(The server will use whatever R version this PC has - it ships inside the bundle -
so there's nothing to match.) **Double-click `make-bundle.bat`.**

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
1. installs a **private copy of R** into `R-runtime\` (silent, isolated - your
   existing R/RStudio is untouched),
2. installs every **R package** into the app-local `R-lib\` from `offline\repo\`,
3. sets up **Poppler** and silent-installs **Tesseract** for scanned-PDF OCR,
4. creates `config\config.yaml` from the example,
5. **starts the app** and prints the `http://...:8100` URL to share.

Every run after that just starts the app. Leave the window open; Ctrl-C stops it.
(Text PDFs, CSV and Excel work even without Poppler/Tesseract - those two are only
for scanned-image PDFs.) If a Windows permission prompt appears during the one-time
R install, accept it.

To keep it always-up across reboots, register it as a service - see
`docs/SETUP-AND-DEPLOYMENT.md` (Task Scheduler "at startup" running
`scripts\run_app.R`).

---

## Updating to a new version later
Build a fresh `StatementStudio-offline` on the internet PC (Step 1) and copy it onto
the server **over the existing folder, choosing "replace files in the destination".**
That refreshes the app code and leaves your server-only files in place -
`config\config.yaml`, the installed `R-runtime\` and `R-lib\`, and your `logs\` /
`feed\` / `uploads\`. Your config is safe either way: the bundle never carries a
`config.yaml`, and `RUN-ME.bat` also keeps a backup under
`%LOCALAPPDATA%\StatementStudio\` and restores it automatically if the folder's copy
is ever missing (e.g. if you delete-and-replace the whole folder instead).

## Notes
- **The server already has R (an old version)?** Fine - it's ignored. `RUN-ME.bat`
  installs and uses its own private R in `R-runtime\`; the existing R and RStudio
  are not touched.
- **Blocked downloads on the build PC?** The package `offline\repo\` is the
  essential part and is fully automated. Only the three system installers are
  best-effort; if any is skipped, the build prints exactly where to download it,
  and you drop it into `StatementStudio-offline\offline\prereqs\` yourself before
  copying the folder across.
- **No installer in `prereqs\`?** If the R download was blocked and the server has
  no R, `RUN-ME.bat` says so and points you back here. Add `R-x.y-win.exe` to
  `offline\prereqs\` and run it again.
