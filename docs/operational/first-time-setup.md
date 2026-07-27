# First-time setup (air-gapped server)

Two double-clicks: build a package on a PC with internet, copy one folder to the
server, run it. Nothing needs to be pre-installed on the server — not even R.

You need a Windows PC **with internet and R installed** (any recent version, from
`cran.r-project.org`) to build the package, the app folder on that PC, the
air-gapped Windows server, and an approved way to copy a folder between them.

Whatever R or RStudio the server already has is ignored and left untouched — the
package brings its own and installs it inside its own folder.

---

## 1 — Build the package (on the internet PC)

In the app folder, double-click **`make-bundle.bat`**.

A few minutes later you have one folder, **`StatementStudio-offline`**, holding
the app plus every R package, the R installer, and the Poppler/Tesseract
installers the server needs.

Read the last lines before you walk away. If it reports anything **MISSING**,
the PC's internet was blocked or proxied — fix that and run it again. A bundle
missing Poppler/Tesseract installs and runs fine and then cannot read a single
scanned PDF, which is the failure you least want to discover on the server.

## 2 — Copy it to the server

Copy the whole `StatementStudio-offline` folder across, e.g. to
`D:\StatementStudio-offline`. That folder is the entire product.

## 3 — Run it (on the server)

Double-click **`RUN-ME.bat`**.

The first run installs a private copy of R inside the folder, installs the R
packages, sets up Poppler and Tesseract, creates `config\config.yaml`, and starts
the app on `http://<this-server>:8100`. Every run after that just starts it.

If a Windows permission prompt appears during the R install, accept it and run
`RUN-ME.bat` again. Setup retries until it succeeds — the "already done" marker
is only written when the install actually worked.

Leave the window open while the app runs; `Ctrl-C` stops it.

## 4 — Set the admin password

Open `config\config.yaml` in Notepad and change:

```yaml
app:
  admin_password: changeme
```

**Until you change it, the Admin tab refuses to open for anybody** — not even
with the right password typed, because the placeholder is printed in this repo
and in the example file. Admin manages templates, the shared label dictionary and
the analytics feed, so it stays shut. The app prints
`Admin is CLOSED - no admin password is set` at startup while that is the case.

Set it here, or set the `BSO_ADMIN_PASSWORD` environment variable (which wins),
then **restart the app**.

## 5 — Open the firewall port

On the server itself, browse to the printed address. It works.

For anyone else to reach it, open the port once. Windows Server blocks
unsolicited inbound TCP by default, so until you do this the app works on the
server and **times out for every other machine** — which looks exactly like the
app being broken.

Administrator Command Prompt, on the server:

```
netsh advfirewall firewall add rule name="Statement Studio 8100" dir=in action=allow protocol=TCP localport=8100
```

Then browse to `http://<server-name>:8100` from another machine. Still nothing?
Ask your network team — a separate network firewall between the users and the
server can block it too, and that one is not yours to change.

---

## Next

- Keep it running after reboots → [running-and-keeping-it-up.md](running-and-keeping-it-up.md)
- Convert your first statement → [converting-statements.md](converting-statements.md)
- Wire up the Qlik dashboards → [connecting-qlik.md](connecting-qlik.md)
- Get the irreplaceable folders off the box → [backup-and-restore.md](backup-and-restore.md)
- Inherited this tool? → [maintaining-the-engine.md](maintaining-the-engine.md)

## If it goes wrong

| Symptom | Fix |
|---|---|
| `Could not set up the private R` | The build PC could not download R, or a permission prompt was declined. Check `offline\prereqs` holds an `R-*-win.exe`; if not, rebuild on a PC with clean internet and re-copy. |
| `Offline setup did NOT complete` / packages reported `MISSING` | Usually the bundle was built under a different R x.y. Rebuild with `make-bundle.bat`, re-copy the whole folder, run `RUN-ME.bat` again. |
| Works on the server, times out from everywhere else | The firewall port is not open — step 5. |
| Scanned PDFs do not read | Check `offline\manifest.txt`. `MISSING` → the build PC never downloaded Poppler/Tesseract; rebuild and re-copy. `included` → delete `offline\.installed` and run `RUN-ME.bat` again to redo the OCR install. Text PDFs, CSV and Excel keep working either way. |
| Admin will not open with the right password | The password is still `changeme` or blank. Step 4, then restart. |
| Which version is on this server? | `offline\manifest.txt` — app version, the exact R, and every bundled package version. |
