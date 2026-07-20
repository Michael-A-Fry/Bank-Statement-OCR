# Run me first — get it live for your team (copy-paste)

You need **one VM with internet**. Nobody else installs anything. ~10 minutes.

---

## Step 1 — get the files onto the VM (no git needed)
On GitHub → green **Code** button → **Download ZIP**. Copy the ZIP to the VM and
unzip it. You now have a folder with `app.R`, `R/`, `templates/`, `scripts/`.

---

## Step 2 — one command sets everything up

**Linux VM** (paste in a terminal, in the unzipped folder):
```bash
bash scripts/setup.sh
```

**Windows VM** (paste in PowerShell, in the unzipped folder):
```powershell
powershell -ExecutionPolicy Bypass -File scripts\setup.ps1
```

That installs R, all packages and OCR, creates the folders, **runs the tests**,
and prints the URL to share. If the last line says the tests passed, you're good.

---

## Step 3 — choose how people reach it

### Option A — browser (try this first)
Start it:
```bash
bash scripts/start.sh          # Linux
powershell -File scripts\start.ps1   # Windows
```
Share the URL it prints: **`http://<your-vm-name-or-ip>:8100`**. Done — users just
open that link. To keep it running after you log off, register it as a service
(NSSM on Windows / a systemd unit on Linux — snippets in
`docs/SETUP-AND-DEPLOYMENT.md`).

### Option B — shared folder (if the browser URL is blocked on your network)
No web, no ports. Share the `inbox\` and `outbox\` folders to the team, then
schedule the converter every 2 minutes:
```bash
# Linux cron (every 2 min):
*/2 * * * * cd /path/to/folder && Rscript scripts/serve_inbox.R
```
```powershell
# Windows: Task Scheduler -> action Rscript.exe, argument scripts\serve_inbox.R,
# "Start in" = the folder, trigger every 2 minutes.
```
People drop a statement in `inbox\`; the Excel/CSV/JSON appears in `outbox\`.

---

## Step 4 — lock it down (authorisation, no code)
Right-click the folder → **Properties → Security** → give access to your AD group
(`RES_QLIKSENSE_PROD`; add more groups as needed). In the group = you're in; not
in it = Windows blocks you. That's the whole access-control story.

---

## Step 5 — sanity check before you tell people
Convert **3–5 statements you already know the answers to** and confirm the numbers
match (look for the green **Trust: high** and the balance reconciliation). Then
tell users: *prefer CSV/Excel exports where your bank offers them; on PDFs, glance
at the trust score and the "coverage" note.*

---

## To update later
Download a fresh ZIP, replace the folder, but **keep** your `templates_user\`,
`logs\`, and `inbox/outbox\`. Re-run `bash scripts/setup.sh` (it just re-checks).

**Stuck?** `docs/SETUP-AND-DEPLOYMENT.md` has the detail; `docs/LAUNCH-AUDIT.md`
covers what's ready and the honest limits.
