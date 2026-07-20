# Setup & deployment — get it running for the team

Written for your exact situation: a locked-down work environment, **you download
files (no git)**, it runs on **your own VM that has internet**, you can **give
people folder access**, you **can't change firewalls or install R on everyone's
machine**, and it must be **centralised**.

The whole idea: **install R once, on the VM.** Nobody else installs anything.
People reach the tool one of two ways — a browser, or a shared folder. Pick
whichever your network allows; the folder way needs nothing but the folder
access you already have.

---

## Part 1 — Put the files on the VM (5 minutes)

1. On GitHub, click **Code → Download ZIP** for the `bank-statement-ocr` repo
   (branch `bank-statement-ocr-platform`). No git needed.
2. Copy the ZIP to the VM and unzip it to a folder, e.g. `C:\BankStatements\`
   (Windows) or `/opt/bankstatements/` (Linux). That folder — with `app.R`, `R/`,
   `templates/`, `scripts/` inside — is the whole app.
3. That's the install. To update later, download a fresh ZIP and replace the
   folder (keep your `templates_user/`, `logs/`, and any `inbox/outbox/`).

---

## Part 2 — Install R + the packages, once, on the VM

> **Air-gapped / no-internet PC?** See **`docs/OFFLINE-INSTALL.md`** — you bundle
> the packages on an internet laptop (`scripts/bundle-offline.R`), carry the
> folder over, and install with `scripts/install-offline.R`. No compiling.

**Windows VM**
1. Download and run the R installer from https://cran.r-project.org/bin/windows/base/ .
2. Open "Rgui" or a terminal and run:
   ```r
   install.packages(c("shiny","DT","yaml","jsonlite","openxlsx","readxl","pdftools","magick"))
   ```
3. For scanned-PDF OCR (optional but recommended), install **Tesseract** and
   **Poppler** for Windows and make sure they're on the PATH. Text PDFs, CSV and
   Excel work without them.

**Linux VM (Ubuntu/Debian)** — one block:
```sh
sudo apt-get update
sudo apt-get install -y r-base r-cran-shiny r-cran-dt r-cran-yaml \
  r-cran-jsonlite r-cran-openxlsx r-cran-readxl r-cran-pdftools r-cran-magick \
  tesseract-ocr poppler-utils
```
If your VM's network blocks CRAN, the `apt` route above pulls everything from the
distro mirror instead; that's the reliable path on a corporate box.

**Check it works:**
```sh
cd /path/to/BankStatements
Rscript tests/run_tests.R      # should print "failed: 0"
```

---

## Part 3 — Choose how people reach it

### Option A — Browser (nicest, if users can reach the VM over HTTP)
Run the app as an always-on service; everyone opens a URL. No user installs.

Start it (from the app folder):
```sh
Rscript scripts/run_app.R        # serves on port 8100
```
Users then open **`http://<vm-name-or-ip>:8100`** in any browser.

**Make it come online automatically on every reboot — one command.** You do not
hand-write any service file; a script installs it, enables it at boot, and keeps
it alive if it ever crashes:
- **Linux:** `sudo bash scripts/install-service.sh`
  (add `--inbox` to also run the folder poller as a service). Then:
  `systemctl status bankstatements` to check, `journalctl -u bankstatements -f`
  for logs, `sudo systemctl restart bankstatements` after a folder update.
- **Windows:** in an **Administrator** PowerShell,
  `powershell -ExecutionPolicy Bypass -File scripts\install-service.ps1`
  (add `-Inbox` for the poller). It registers a Task Scheduler job that runs at
  startup as SYSTEM, with the working directory set and auto-restart on. Manage
  it under **Task Scheduler → BankStatementsApp**.

Both installers set the port from `BSO_PORT` (default 8100) and create the
`logs/ out/ inbox/ outbox/ processed/ failed/` folders. To remove auto-start:
Linux `sudo systemctl disable --now bankstatements`; Windows
`Unregister-ScheduledTask -TaskName BankStatementsApp -Confirm:$false`.

**The firewall question:** this only needs users to reach the VM on **one port
(8100)** over the internal network — no firewall *changes*, just that internal
HTTP to the VM isn't blocked. Most corporate LANs allow that. Test from your own
machine first (`http://<vm>:8100`). If it loads, you're done. If it's blocked and
you can't get the port opened, use Option B — it needs no HTTP at all.

### Option B — Shared folder (bulletproof; needs only the folder access you have)
No web, no ports, no browser. People drop files in a folder; results appear in
another. This works entirely within "I can give people access to the folders".

1. In the app folder on the VM, create `inbox\`, `outbox\`, `processed\`,
   `failed\`. Share `inbox\` (drop box, write access) and `outbox\` (read access)
   to the team over the normal file share.
2. Schedule the converter to run every couple of minutes:
   - **Windows Task Scheduler:** action = `Rscript.exe`, argument =
     `C:\BankStatements\scripts\serve_inbox.R`, "Start in" = the app folder,
     trigger = every 2 minutes.
   - **Linux cron:** `*/2 * * * * cd /opt/bankstatements && Rscript scripts/serve_inbox.R`
   - Or run it once as a live poller: `Rscript scripts/serve_inbox.R loop`.
3. A user drops `mybank.pdf` into `inbox\`. Within ~2 minutes they get
   `outbox\mybank\mybank.xlsx` (+ .csv, .json). The original moves to
   `processed\` (or `failed\`). Every run is logged to `logs\` exactly as the app
   does, so the Admin insights still work if you also run the app.

**Which to use?** Try A first (the wizards, guided setup, and Admin panel are
browser features). Fall back to B when HTTP to the VM isn't allowed — and you can
run **both** at once off the same folder (the app for template-building and
insights, the inbox for bulk drop-and-collect).

---

## Part 4 — Who can use it (authorisation, no code)
Put the app folder (and `inbox\`/`outbox\`) behind the **AD group** on the share's
Security tab (default `RES_QLIKSENSE_PROD`; add more groups as an OR). In the
group → you can open the folder / reach the app; not in it → Windows blocks you.
No login screen, nothing to maintain. Full detail:
`docs/architecture/deployment-integration-plan.md`.

---

## Part 5 — Day-to-day maintenance (one analyst, no engineer)
- **Add a bank:** a user hits an unreadable statement → **Guided setup** pre-fills
  a template, they confirm + Save (→ `templates_user\`). Or the analyst uses the
  wizards (→ `templates\`). See `docs/wizard-tutorial.md`.
- **See what's failing / drifting:** the **Admin** tab (or read the `logs\`
  folder — each run is a readable `.json`).
- **Keep logs tidy:** click "Tidy up logs" in Admin, or schedule
  `Rscript -e 'source("R/... "); rollup_logs("logs","runs")'` monthly. Nothing is
  deleted — old runs move to `logs\archive\`.
- **Update the tool:** download a fresh ZIP, replace the folder, keep
  `templates_user\`, `logs\`, `inbox/outbox\`. Run `Rscript tests/run_tests.R`.

There is no database, no server framework, no build step. The folder *is* the
install; the YAML files are the config; the JSON files are the logs.
