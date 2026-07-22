# Qlik + Statement Studio - step-by-step dev setup

A hands-on guide to standing the integration up on a **dev** Qlik Sense (Windows)
box, with the Qlik code. By the end: a user uploads a statement in Qlik and gets the
converted transactions back in an ODAG app - same experience as the legacy
`Statement Converter`, with this R engine underneath instead of Mole + per-bank
scripts.

It builds up in provable stages - **prove the engine first, wire Qlik second** - so
you never debug two unknowns at once. The design behind it is in
`qlik-sense-integration.md` (read §13-§16 for the why); this is the *how*.

> **Which path?** For dev, use the **async folder poller** (Part 4). It needs
> nothing installed or unlocked inside Qlik, works even if R and Qlik are on
> different boxes, and reuses your existing ODAG plumbing. Two one-click
> alternatives (EXECUTE, Rserve/SSE) are in Part 6 for later.

---

## Part 0 - Prerequisites

- Qlik Sense (dev) with your existing **Inphinity Forms** upload + **ODAG**
  statement-viewer flow.
- A **Windows account** that can install software and create a Scheduled Task on
  the box where R will run (the Qlik node is fine for dev).
- **The server is air-gapped (no internet).** So you also need **one
  internet-connected Windows machine** to collect the installers/packages, and an
  approved way to move a folder across (USB, transfer share). **Nothing here needs
  internet at *runtime*** - only this one-time package install does; the engine, the
  Shiny app and the poller all run fully offline.
- The Statement Studio folder (this repo) copied onto both machines, e.g.
  `D:\StatementStudio\`.

---

## Part 1 - Install R + the engine OFFLINE (air-gapped)

Because the server has no internet, you build a bundle on an online machine and
carry it over. Two scripts already do the whole thing; full detail in
[`docs/OFFLINE-INSTALL.md`](../OFFLINE-INSTALL.md).

> **The one rule:** run the bundler under the **same R x.y** the server will run
> (e.g. both `4.4`). Windows package binaries are built per R minor version - a
> mismatch is the only thing that makes this fail.

**1a. On the INTERNET-connected machine** (same R x.y as the server):
```bat
cd /d D:\StatementStudio
Rscript scripts\bundle-offline.R
```
This creates a self-contained **`bso-offline\`** folder (~a few hundred MB):
```
bso-offline\
  repo\            every R package + all dependencies, as Windows binaries
  prereqs\         the R installer (R-x.y-win.exe) + Tesseract + Poppler (best effort)
  install-on-pc.R  the one script you run on the server
  packages.txt     the list, for reference
```
(If `prereqs\` couldn't fetch Tesseract/Poppler behind a proxy, the console prints
the exact URLs to grab by hand - only needed for *scanned*-PDF OCR.)

**1b. Move it across.** Copy the whole **`bso-offline\`** folder (and the
`StatementStudio` repo, if not already there) to the server.

**1c. On the AIR-GAPPED server:**
1. Install **R** from `bso-offline\prereqs\R-x.y-win.exe` (default options).
2. Install every R package from the bundle - no internet used:
   ```bat
   cd /d <path>\bso-offline
   "C:\Program Files\R\R-4.x.x\bin\Rscript.exe" install-on-pc.R
   ```
   It installs the packages from `repo\`, wires Poppler onto your PATH, and points at
   the Tesseract installer (run it once if you need scanned-PDF OCR). It prints
   `R packages: N/N installed.` - if any are MISSING, the R x.y didn't match; rebuild
   1a under the right R.
3. Sanity-check the engine converts a bundled sample (offline):
   ```bat
   cd /d D:\StatementStudio
   Rscript run.R samples\raw\bnz\bnz_transaction_export_01.csv BNZ out
   dir out
   Rscript tests\run_tests.R          &:: expect: failed: 0
   ```
   You should see `bnz.xlsx / .csv / .json`. **The engine works offline. Stop here
   until it does.**

---

## Part 2 - The shared folder + config

1. Pick a share both Qlik and R can read/write, e.g. `\\fileserver\BankStatements\`
   (dev can just be a local folder like `D:\StatementStudio\qlik`). Grant it to your
   Qlik AD group (`RES_QLIKSENSE_PROD`) - the same way your Qlik shares are secured.
2. Create the config file (all settings live here):
   ```bat
   copy config\config.example.yaml config\config.yaml
   ```
   Edit `config\config.yaml`:
   ```yaml
   app:
     admin_password: <pick-a-dev-password>
     shiny_url: http://<dev-host>:8100      # where a "no template yet" click goes
   paths:
     templates: templates                    # PROVEN templates Qlik may use
   qlik:
     inbox: D:/StatementStudio/qlik/inbox     # Inphinity writes uploads here
     outbox: D:/StatementStudio/qlik/outbox   # ODAG loads outbox/<key>/statement.csv
     index: D:/StatementStudio/qlik/index     # the Qlik file-list table
     processed: D:/StatementStudio/qlik/processed
   ```
   (Use forward slashes or doubled backslashes in YAML. Leave a key out to accept its
   default.)

---

## Part 3 - Prove the pipe with NO Qlik yet

1. Start the poller (leave it running):
   ```bat
   Rscript scripts\qlik_poller.R loop
   ```
2. In another window, drop a statement into the inbox:
   ```bat
   copy samples\raw\bnz\bnz_transaction_export_01.csv D:\StatementStudio\qlik\inbox\march.csv
   ```
3. Within ~15s the poller prints `ok  march.csv -> outbox/<key>/` and you get:
   ```
   qlik\outbox\<key>\statement.csv    <- what ODAG will LOAD
   qlik\outbox\<key>\status.json      <- {status, needs_template, shiny_url, ...}
   qlik\index\<key>.csv               <- one status row for the file-list table
   ```
   Open `statement.csv` - clean transactions. **The whole engine half is now proven
   without touching Qlik.**

4. Make it a service so it's always on: **Task Scheduler -> Create Task**, trigger
   "At startup", action `Rscript.exe` with argument `D:\StatementStudio\scripts\qlik_poller.R loop`,
   "Run whether user is logged on or not".

---

## Part 4 - Wire Qlik (the dev path: poller + ODAG)

Your Qlik app already has the Inphinity upload, the file-list table, the *generate*
navigation point, and the ODAG *Statement Converter* template app. You change three
small things.

### 4.1 A folder data connection

In the QMC (or the app's *Data load editor -> Create connection*), add a **Folder**
connection named **`StatementQlik`** pointing at the `qlik\` share
(`D:\StatementStudio\qlik` on dev).

### 4.2 Point Inphinity's upload at the inbox

In the Inphinity Forms `type=upload` control, set the destination folder to the
**inbox** (`lib://StatementQlik/inbox`, i.e. `D:\StatementStudio\qlik\inbox`). That
is the only Inphinity change - the "save & reload" button stays as is.

### 4.3 Selection app: load the file-list-with-status table

The poller writes one CSV per file into `index\`. Load them as the table the user
picks from (this *replaces/augments* whatever currently populates that table):

```qvs
// Selection app load script - the uploaded files and their conversion status
Files:
LOAD
    key,
    file,
    status,
    needs_template,
    row_count,
    converted_ts,
    csv,
    shiny_url
FROM [lib://StatementQlik/index/*.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);
```

Now: user uploads (Inphinity) -> save & reload -> the poller converts within seconds
-> the next reload shows each file with `status` = `ok` / `needs_template` /
`failed`. Let the user pick a row and hit **generate** only when `status = ok`.

### 4.4 ODAG template app: LOAD our CSV instead of extracting

In your existing *Statement Converter* ODAG app, **replace the Mole + per-bank
extraction block** with a load of the converted CSV for the selected `key`:

```qvs
// $(vKey) = the selected file's `key`, delivered by the ODAG navigation-link
// binding (see 4.5). Everything else in this app - the sheet, the charts, the
// download button - stays as it was.
Statement:
LOAD *
FROM [lib://StatementQlik/outbox/$(vKey)/statement.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);

// trivially-derived legacy columns (no engine change needed)
Enrich:
LOAD *,
    Year(Date)                               as [Year],
    If(Amount <= 0, 'Withdrawal', 'Deposit') as [Transaction Type]
Resident Statement;
DROP TABLE Statement;
```

The CSV already carries `date, description, amount, debit, credit, direction,
balance, particulars, code, reference, ..., flags` - map your existing sheet objects
to these column names.

### 4.5 The ODAG binding (selected key -> `vKey`)

In the **ODAG navigation link** properties, add a **binding** that carries the
selected file's `key` into the template app. You already do this for the file
identifier in the current flow - point it at the `key` field. A single-selection
binding expression looks like:

```
// binding value expression (navigation link):
=Only(key)            // the one selected file's key
```

and the template app references it as the variable `$(vKey)`. **Match your existing
ODAG binding convention** - you have a working ODAG statement-viewer, so reuse the
exact variable/field wiring it already uses to receive the selected file; only the
*use* of it changes (build the CSV path from it, as in 4.4).

### 4.6 The "no proven template yet" branch

When `status = needs_template` (a bank we have no proven template for), do **not**
generate. On the selection sheet, show a message + a button for those rows:

- A **text object**, shown when `Only(status) = 'needs_template'`, reading
  *"No proven template for this bank yet - set it up in the full app."*
- A **button** with action **Open website**, URL = `=Only(shiny_url)` (the Shiny app
  link the poller wrote into the row). It opens Statement Studio, where the team
  builds the template.

Meanwhile the poller has already filed that statement into Statement Studio's
**Admin -> Uploads** pickup queue automatically, so the team sees the new bank
without anyone re-sending it. Once they build + prove its template (it lands in
`templates\`), the next upload of that bank converts in Qlik with no further wiring.

---

## Part 5 - End-to-end test checklist

1. Poller running (Part 3 / Task Scheduler). Reload the selection app.
2. **Happy path:** upload a statement for a **proven** bank via Inphinity -> save &
   reload -> the row shows `status = ok` -> select it -> **generate** -> the ODAG app
   opens with the transactions + your charts + download. ✔
3. **Unknown bank:** upload a statement for a bank with no proven template -> the row
   shows `status = needs_template` -> the message + **Open the full app** button
   appear -> clicking it opens Statement Studio -> the statement is already in
   **Admin -> Uploads**. ✔
4. **Isolation:** two people upload different files named the same (`statement.pdf`)
   at once -> two different `key` folders (content-hashed) -> each generates its own
   correct app, no bleed. ✔

---

## Part 6 - One-click alternatives (later, not needed for dev)

The poller is async (upload, then generate once it's converted). If you want a single
**generate = convert-and-show** click, invoke R *inside* the ODAG reload. Both need R
on the Qlik node; pick by your Qlik security policy (see the decision table in
`qlik-sense-integration.md` §13.2a).

### 6a. EXECUTE (simplest - needs the `Allow Execute` override)

Enable standard-mode override / `Allow Execute` in the QMC, then in the ODAG app:

```qvs
// $(vFile) = the uploaded file's full path (from the selection row);
// $(vKey) = its key. Converts THEN loads, in one reload.
EXECUTE "C:\Program Files\R\R-4.x.x\bin\Rscript.exe"
        "D:\StatementStudio\scripts\convert_for_qlik.R" "$(vFile)" "D:\StatementStudio\qlik\outbox\$(vKey)";

Statement:
LOAD * FROM [lib://StatementQlik/outbox/$(vKey)/statement.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);
```

### 6b. Rserve / SSE (no `Allow Execute`; the sanctioned channel)

1. On the R box: install **Rserve** - it's already in your offline bundle
   (`install-on-pc.R` installs it), so no internet needed. Then run it:
   `Rscript -e "library(Rserve); Rserve(args='--no-save')"` (port 6311).
2. Install the **SSE-to-Rserve** connector (Qlik's open-source R plugin) and register
   an **Analytic Connection** in the QMC (e.g. name `R`, host, port 6311).
3. Expose a wrapper to Rserve (source the engine once in the Rserve session):
   ```r
   for (f in list.files('D:/StatementStudio/R', full.names=TRUE, pattern='\\.R$')) source(f)
   # convert_statement_sse(path) is then callable from Qlik
   ```
4. In the app load script, call it (exact call shape depends on the SSE-R connector's
   table mode):
   ```qvs
   Statement:
   LOAD *
   EXTENSION R.ScriptEval('convert_statement_sse(path)', (FileList){[path]});
   ```
   The wrapper still writes the audit artifacts to disk and returns the transactions
   table for Qlik to load.

---

## Troubleshooting

| Symptom | Fix |
|---|---|
| Row never shows `ok` | Is the poller running? Check its console / the Task Scheduler task. Look in `qlik\processed\` (converted) vs the file still in `inbox\`. |
| `status = failed` | Open `qlik\outbox\<key>\status.json` and `logs\runs\<run_id>.json` for the reason. A scanned PDF needs Tesseract on PATH. |
| Every bank says `needs_template` | Qlik reads **proven** templates only (`paths.templates`). Confirm that folder holds the bank's YAML; drafts in `templates_user\` are Shiny-only by design. |
| ODAG app loads nothing | Check `$(vKey)` resolves to the selected row's `key`, and that `outbox\<key>\statement.csv` exists. |
| Special characters garbled | Keep `codepage is 65001` (UTF-8) on every `LOAD`. |
| Offline install: packages `MISSING` | The bundler (1a) ran under a different **R x.y** than the server. Rebuild `bso-offline` on the online machine using the same R minor version, re-copy. |
| Admin password not taking | It's `app.admin_password` in `config\config.yaml`; the `BSO_ADMIN_PASSWORD` env var overrides it if set. |
