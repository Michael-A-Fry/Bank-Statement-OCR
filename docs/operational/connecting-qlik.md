# Connecting the Qlik dashboards

Statement Studio does the converting; **Qlik does the analytics**. Every clean
conversion automatically writes a row to a **feed** folder, and Qlik loads that
folder on a schedule to build dashboards. Qlik never parses a statement itself — it
just reads the clean, reconciled data the tool produces.

Two small pieces to wire up, both standard Qlik. Only **proven templates that
reconcile** reach the dashboards (the governance gate), so unvetted output never
becomes org data — the full architecture is in
[`../context/architecture/qlik-sense-integration.md`](../context/architecture/qlik-sense-integration.md).

---

## Step 1 — Point the feed at the share
In `config\config.yaml` on the server (see
[running-and-keeping-it-up.md](running-and-keeping-it-up.md)), set the feed folder to
a location Qlik can read, and set the app URL for the tile:

```yaml
app:
  shiny_url: http://your-server:8100
feed:
  enabled: true
  feed_dir: D:/StatementStudio/feed     # or a UNC share, e.g. //fileserver/share/feed
  min_trust: medium                     # medium (default) = every clean conversion; 'high' = only balance-proven results (stricter than default)
```
Restart the app (`RUN-ME.bat`) after changing it, then convert one statement so the
feed has something in it.

The feed folder fills itself:
| File | Contents |
|---|---|
| `feed\transactions\<hash>.csv` | the clean transaction rows the dashboards show (accepted conversions), keyed by statement content hash |
| `feed\runs\<hash>.csv` | one manifest row per **statement** — keyed by the same content hash — covering accepted **and** withheld runs; re-converting a statement overwrites its row (idempotent, latest attempt wins) |
| `feed\review\<hash>.csv` | rows from withheld conversions (optional, a separate review table), keyed the same way |

---

## Step 2 — Load the feed in Qlik
In the QMC, add a **Folder** data connection named **`StatementFeed`** pointing at
the `feed` folder. Then in your analytics app's load script:

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
Build sheets on **Transactions** (money in/out, balances, cross-statement views) and
**Runs** (volumes, coverage, what was withheld). **Schedule the reload** (e.g.
hourly) in the QMC so new conversions appear. Keep `codepage is 65001` (UTF-8) on
every load so special characters stay intact.

---

## Step 3 — Add a "Convert a statement" tile
On a Qlik sheet, add a **button** (or text object) with an **Open website** action,
URL = your `app.shiny_url` (`http://your-server:8100`), opening in a new tab. The same
login covers both, so a user lands straight in the converter.

---

## Checklist
1. Convert a proven-bank statement in the app → download works. ✔
2. Reload the Qlik app → those transactions appear; the `Runs` table shows the run as
   `accepted`. ✔
3. Convert something that doesn't reconcile (or a user template) → it does **not**
   appear on the dashboard; `Runs` shows it `withheld:…`. ✔

---

## If Qlik shows nothing
| Symptom | Fix |
|---|---|
| Nothing in `feed\transactions\` | Was it a real **Convert-button** upload (the sample doesn't feed)? Did it reconcile with a **proven** template? Check `feed\runs\*.csv` — the `gate_result` column says why. |
| Everything is `withheld:not_proven` | The template is user-created, not proven. Promote it (ask the engine maintainer to move it into `templates\`), or set `feed.allowed_template_origins: [default, user]`. |
| `withheld:low_trust` | It didn't reconcile. Only lower `feed.min_trust` if you truly want unreconciled data on dashboards (not recommended). |
| Qlik shows nothing at all | Check the `StatementFeed` connection points at `feed\`, files exist, and the reload ran. |
| Special characters garbled | Keep `codepage is 65001` on every `LOAD`. |
