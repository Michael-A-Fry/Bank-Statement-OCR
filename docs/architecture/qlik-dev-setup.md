# Qlik + Statement Studio - step-by-step dev setup

Stand the integration up on a **dev** Windows box, air-gapped (no internet). By the
end: accountants open the converter from a **Qlik tile**, convert statements in it,
and every conversion feeds a **Qlik analytics app** via a folder + scheduled reload.

Built up in provable stages - engine first, feed second, Qlik last - so you never
debug two unknowns at once. The design is in
[`qlik-sense-integration.md`](qlik-sense-integration.md).

---

## Part 0 - Prerequisites

- A **Windows box** for the Shiny app / engine (the Qlik node is fine for dev), where
  you can install software and (optionally) create a Scheduled Task.
- Qlik Sense (dev).
- **Air-gapped:** you also need **one internet-connected Windows machine** to build
  the offline bundle, and an approved way to copy a folder across. **Nothing runs
  against the internet at runtime** - only the one-time install does.
- The Statement Studio repo copied onto both machines, e.g. `D:\StatementStudio\`.

---

## Part 1 - Install R + the engine OFFLINE

Detail: [`../OFFLINE-INSTALL.md`](../OFFLINE-INSTALL.md).

> **The one rule:** run the bundler under the **same R x.y** the server will run
> (Windows package binaries are per R minor version - the only thing that makes this
> fail).

**1a. On the INTERNET machine** (same R x.y as the server):
```bat
cd /d D:\StatementStudio
Rscript scripts\bundle-offline.R      &:: -> a self-contained bso-offline\ folder
```
**1b.** Copy `bso-offline\` **and** the app repo to the air-gapped box, with
`bso-offline\` next to (or inside) the app folder.

**1c. On the air-gapped box: double-click `offline-setup.bat`** (in the app folder).
One shot, no internet, works wherever the folder lives - it finds R (or installs it
from the bundle), installs every R package, sets up Poppler **and Tesseract** for
scanned-PDF OCR, creates `config\config.yaml`, and runs the test suite. When it
prints `failed: 0`, **the engine works offline.**

<details><summary>Prefer to run the steps by hand?</summary>

```bat
:: install R from bso-offline\prereqs\R-x.y-win.exe, then:
cd /d <path>\bso-offline
"C:\Program Files\R\R-4.x.x\bin\Rscript.exe" install-on-pc.R
cd /d D:\StatementStudio
Rscript run.R samples\raw\bnz\bnz_transaction_export_01.csv BNZ out
Rscript tests\run_tests.R
```
</details>

---

## Part 2 - Config

`offline-setup.bat` already created **`config\config.yaml`** from the example. Open it
and set:
```yaml
app:
  admin_password: <pick-a-dev-password>
  shiny_url: http://<this-host>:8100      # the URL the Qlik tile opens
feed:
  enabled: true
  feed_dir: D:/StatementStudio/feed        # the share Qlik will read
  min_trust: high                          # only reconciled, proven-template results feed
```
(Forward slashes or doubled backslashes in YAML. Any omitted key uses its default.)

---

## Part 3 - Run the app, prove a conversion

**Double-click `start.bat`** (serves on the port in `config.yaml`, default 8100).
Open `http://<this-host>:8100`, go to **Convert**, drop in
`samples\raw\bnz\bnz_transaction_export_01.csv`, click **Convert**. You get the
verdict, the analysis, the transactions, and the downloads. **That's the accountant
experience.** (Note the bank/template picker lists **audited** templates only.)

---

## Part 4 - Prove the feed fills itself

That real conversion (a Convert-button upload) already wrote the feed. Check:
```bat
dir /s /b D:\StatementStudio\feed
```
You should see:
```
feed\transactions\<hash>.csv    <- accepted (reconciled + proven) - the dashboard rows
feed\runs\<run_id>.csv          <- one row per conversion (accepted or withheld)
```
Open `transactions\<hash>.csv` - every row is stamped with run/bank/template/trust +
the transaction fields. **This is what Qlik loads.** (A conversion that doesn't
reconcile, or uses a draft template, is withheld - it appears in `runs\` with
`gate_result = withheld:...` and, if enabled, in `feed\review\`.)

Make the app a service so it's always up: **Task Scheduler -> Create Task**, trigger
"At startup", action `Rscript.exe` arg `D:\StatementStudio\scripts\run_app.R`, "Run
whether user is logged on or not".

---

## Part 5 - Wire Qlik

Two small pieces, both standard Qlik.

### 5.1 The analytics app loads the feed
Add a **Folder** data connection **`StatementFeed`** -> `D:\StatementStudio\feed`,
then in the app's load script:
```qvs
Transactions:
LOAD
    *,
    Year(Date)                               as [Year],
    If(Amount <= 0, 'Withdrawal', 'Deposit') as [Transaction Type]
FROM [lib://StatementFeed/transactions/*.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);

Runs:
LOAD *
FROM [lib://StatementFeed/runs/*.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);
```
Build your sheets on `Transactions` (money in/out, balances, cross-statement views)
and `Runs` (volumes, coverage, what was withheld). **Schedule the reload** (e.g.
hourly) in the QMC so new conversions appear.

### 5.2 The "Convert a statement" tile
On a Qlik sheet, add a **button** (or text object) with action **Open website**,
URL = your `app.shiny_url` (`http://<this-host>:8100`), opening in a new tab. Same
AD-group login covers both, so a user in the group lands straight in the converter.

---

## Part 6 - End-to-end test checklist

1. App running (Part 3 / service). Reload the Qlik analytics app.
2. **Convert path:** open the app from the Qlik tile -> upload a proven-bank statement
   -> convert -> download. ✔
3. **Feed path:** after that conversion, reload the Qlik app -> the transactions show
   up on the dashboard; the `Runs` table shows the conversion as `accepted`. ✔
4. **Governance:** convert a statement that doesn't reconcile (or with a draft
   template) -> it does **not** appear in the dashboard; `Runs` shows
   `withheld:...`. ✔
5. **Isolation:** two people convert different files named the same at once -> two
   different content-hash feed files, no bleed. ✔

---

## Troubleshooting

| Symptom | Fix |
|---|---|
| Nothing in `feed\transactions\` | Was it a **Convert-button** upload (the sample/auto-reconvert don't feed)? Did it reconcile with a **proven** template? Check `feed\runs\*.csv` -> `gate_result`. |
| Everything is `withheld:not_proven` | The template lives in `templates_user\` (a draft), not the proven `templates\` set. Promote it (move the YAML into `templates\`), or set `feed.allowed_template_origins: [default, user]`. |
| `withheld:low_trust` / `withheld:needs_review` | It didn't reconcile. Lower `feed.min_trust`/`require_status_ok` only if you truly want unreconciled data in dashboards (not recommended). |
| Qlik shows nothing | Check the `StatementFeed` connection points at `feed\`, files exist, and the reload ran. |
| Special characters garbled | Keep `codepage is 65001` (UTF-8) on every `LOAD`. |
| Offline install: packages `MISSING` | The bundler ran under a different **R x.y** than the box. Rebuild `bso-offline` under the matching R, re-copy. |
| Admin password not taking | It's `app.admin_password` in `config\config.yaml`; `BSO_ADMIN_PASSWORD` env var overrides it. |
