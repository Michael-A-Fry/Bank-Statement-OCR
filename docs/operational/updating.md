# Updating to a new version

You get a new version the same way you first built it — build a fresh package on
the internet PC, then drop it on the server. Your settings, templates, logs and the
installed R are all kept automatically.

---

## Steps
1. **On the internet PC:** get the new app folder, then **double-click
   `make-bundle.bat`** to build a fresh `StatementStudio-offline` folder (same as
   first-time setup, Step 1).
2. **Copy it to the server, over the existing folder**, and when Windows asks,
   choose **"Replace the files in the destination"**.
3. **Double-click `RUN-ME.bat`.** It starts on the new version.

That's it. The update refreshes the app; it does **not** re-install R or the
packages (those are already there), so it's quick.

---

## What is kept automatically
Because the fresh package doesn't carry these, replacing the folder leaves them in
place:

- **`config\config.yaml`** — your settings (admin password, Qlik URL, feed folder).
- **`R-runtime\` and `R-lib\`** — the installed private R and its packages.
- **`logs\`, `feed\`, `uploads\`** — run history, the Qlik feed, uploads.
- Any **templates you created** in the app.

Your settings are safe even if you delete the whole folder and paste a fresh one:
`RUN-ME.bat` also keeps a backup of `config\config.yaml` outside the folder (under
`%LOCALAPPDATA%\StatementStudio`) and restores it automatically if the folder's copy
is missing.

---

## Notes
- **To also refresh the R packages** (only needed if a new version changes them):
  delete the file `offline\.installed` before running `RUN-ME.bat`, and it will
  reinstall packages on the next start.
- **To roll back:** keep the previous `StatementStudio-offline` folder; to go back,
  restore it and run its `RUN-ME.bat`. Your `config`, `logs` and `feed` are
  unaffected.
