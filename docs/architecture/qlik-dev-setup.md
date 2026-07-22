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
  you can create a Scheduled Task. Nothing to pre-install - `RUN-ME.bat` installs R
  and everything else for you.
- Qlik Sense (dev).
- **Air-gapped:** you also need **one internet-connected Windows PC** to build the
  package once, and an approved way to copy a folder across. **Nothing runs against
  the internet at runtime** - only the one-time build does.

---

## Part 1 - Install the whole thing OFFLINE (two double-clicks)

Detail: [`../OFFLINE-INSTALL.md`](../OFFLINE-INSTALL.md).

> **Isolated, no version-matching:** the server gets its **own private R** inside
> the folder (`R-runtime\`) and uses only that, so whatever R/RStudio is already on
> the box (even an old one) is ignored and untouched. The build PC just needs **any
> recent R** with internet - that exact R ships in the bundle, so the packages
> always match.

**1a. On the INTERNET PC** (this repo, R same x.y as the server): **double-click
`make-bundle.bat`.** It gathers the whole app plus every package and installer into
one self-contained folder, **`StatementStudio-offline`**.

**1b.** Copy that whole **`StatementStudio-offline`** folder to the air-gapped box.
That one folder is everything - there is nothing else to carry.

**1c. On the air-gapped box: double-click `RUN-ME.bat`** inside the folder. First run,
with no internet, it finds R (or installs it silently from the bundle), installs every
R package, sets up Poppler **and** Tesseract for scanned-PDF OCR, creates
`config\config.yaml`, and **starts the app** - printing the `http://...:8100` URL.
Every run after that just starts the app. **That's the entire server setup.**

---

## Part 2 - Config

The first `RUN-ME.bat` run already created **`config\config.yaml`** from the example.
Open it and set:
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
Re-run `RUN-ME.bat` to pick up the change.

---

## Part 3 - Prove a conversion

With the app running (from `RUN-ME.bat`), open `http://<this-host>:8100`, go to
**Convert**, drop in `samples\raw\bnz\bnz_transaction_export_01.csv`, click
**Convert**. You get the verdict, the analysis, the transactions, and the downloads.
**That's the accountant experience.** (By default the bank/template picker lists
**proven** templates; a tick-box brings in user-created ones with a "not guaranteed
tested" warning.)

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
| Offline install: packages `MISSING` | The build PC couldn't download them all (a proxy/partial build). Re-run `make-bundle.bat` on a PC with clean internet and re-copy the folder. |
| Admin password not taking | It's `app.admin_password` in `config\config.yaml`; `BSO_ADMIN_PASSWORD` env var overrides it. |
