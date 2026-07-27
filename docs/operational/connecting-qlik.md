# Connecting the Qlik dashboards

Statement Studio converts; **Qlik does the analytics**. Every conversion writes a
row to a **feed** folder, and Qlik loads that folder on a schedule. Qlik never
parses a statement — it reads clean, reconciled data the tool has already checked.

Two things to wire up, both standard Qlik.

## 1 — Point the feed at the share

In `config\config.yaml` on the server:

```yaml
app:
  shiny_url: http://your-server:8100    # the address the Qlik tile opens
feed:
  enabled: true
  feed_dir: D:/StatementStudio/feed     # or a UNC share, //fileserver/share/feed
  min_trust: medium                     # medium (default) = every clean conversion
  require_status_ok: true
  allowed_template_origins: [default]   # shipped/tested templates only
  template_allowlist: []                # optional: restrict to specific ids
  include_review_feed: true             # also write withheld runs, as a separate table
```

Restart the app, then convert one statement so the feed has something in it.

Point `feed_dir` at a location Qlik can read. If the share goes read-only the
feed write fails **loudly** — the conversion still succeeds and the failure is
recorded under `logs\feed\` with the reason.

## 2 — What the feed contains

| Path | Contents |
|---|---|
| `feed\transactions\<hash>.csv` | Transaction rows from **accepted** conversions — the dashboard dataset |
| `feed\runs\<hash>.csv` | **One manifest row per statement**, accepted *and* withheld — the coverage/QA table |
| `feed\review\<hash>.csv` | Transaction rows from **withheld** conversions, if `include_review_feed` is on |

All three are keyed by the statement's **content hash** (the first 16 characters
of its SHA-256), so re-converting a statement **overwrites** its files — the
latest attempt wins, and the manifest counts each statement once, not once per
re-run. Files are written atomically (temp file, then rename), so a Qlik reload
can never read a half-written CSV.

### Transaction row schema

Every row starts with these columns, in this order, always present:

```
run_id, converted_ts, source_file, source_sha256, bank, statement_type,
template_id, template_version, template_origin, trust_level, gate_result,
period_start, period_end, account_number, statement_index,
row_id, date, description, amount, debit, credit, direction, balance,
particulars, code, reference, other_party, type, currency, flags
```

A template's named **extras** (card `fx_amount`, KiwiSaver `units`, …) are
appended after those, keeping their own names.

The fixed prefix is why adding a bank template — the cheap path — does not break
your dashboard. A wildcard `LOAD *` concatenates two CSVs only when their field
sets match **exactly**; differ by one column and Qlik invents a synthetic key and
quietly splits your totals in two. A per-bank extra simply arrives as a
mostly-null field, which is what it is.

`gate_result` is on **every transaction row**, not just the manifest — because
Qlik concatenates on field set rather than table name, and `transactions` and
`review` are otherwise field-identical. Filter on it if you load both.

### Manifest row schema

```
run_id, converted_ts, source_file, source_sha256, bank, template_id,
template_origin, status, trust_level, row_count, period_start, period_end,
gate_result, feed_file, engine_version, template_sha256
```

`engine_version` and `template_sha256` are the reproducibility pair: which build
and which exact template content produced a figure, so a dashboard number can be
reproduced years later.

## 3 — The governance gate

A conversion reaches `feed\transactions\` only when **all** of these hold:

1. the status is clean (`require_status_ok`),
2. the confidence level meets `min_trust`,
3. the template is one of the shipped, tested set (`allowed_template_origins`),
4. `template_allowlist` is empty, or names that template.

Otherwise it is **withheld**: still recorded in the manifest, optionally copied to
`review\`. `gate_result` says which rule stopped it — `withheld:not_proven`,
`withheld:low_trust`, `withheld:needs_review`, `withheld:not_in_allowlist`.

The gate is **machine-only**. No human can wave a conversion through; the way in
is to fix the template and convert again, which is the durable fix and leaves an
audit trail. The Convert screen states the gate's verdict on every conversion, so
the person converting is never surprised.

Note that the tool **converts** with templates built on this box by default, and
the gate is on template **origin** — the two are independent by design. An
analyst gets her bank working immediately; the dashboards still only take the
proven set.

Only the **Convert button** feeds. Admin's bulk re-audit and the command-line
scripts deliberately do not: they exist to test templates over piles of files,
and publishing those runs would put unreviewed rows on org-wide dashboards.

## 4 — Load it in Qlik

In the QMC, add a **Folder** data connection named **`StatementFeed`** pointing
at the `feed` folder. Then in the analytics app's load script:

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

Build sheets on **Transactions** (money in/out, balances, cross-statement views)
and **Runs** (volumes, coverage, what was withheld and why). **Schedule the
reload** in the QMC — hourly is typical. Keep `codepage is 65001` on every load
or special characters in descriptions are garbled.

The feed is extraction only. `Year` and `Transaction Type` above are derived in
the load script; any richer categorisation belongs in Qlik, not in the extraction
engine.

## 5 — A "Convert a statement" tile

On a Qlik sheet, add a button or text object with an **Open website** action
pointing at `app.shiny_url`, opening in a new tab.

## Check it works

1. Convert a statement with a shipped template → the download works.
2. Reload the Qlik app → those transactions appear, and `Runs` shows the run as
   `accepted`.
3. Convert something that does not reconcile, or one read by a template built
   here → it does **not** appear, and `Runs` shows it `withheld:…`.

## If Qlik shows nothing

| Symptom | Fix |
|---|---|
| Nothing in `feed\transactions\` | Was it a real **Convert-button** upload? Did it clear the gate? Read the `gate_result` column in `feed\runs\*.csv` — it says why. Then check `logs\feed\` for a write failure. |
| Everything is `withheld:not_proven` | The template is user-built. Promote it ([maintaining-the-engine.md](maintaining-the-engine.md) §3), or add `user` to `feed.allowed_template_origins`. |
| `withheld:low_trust` | It did not reconcile. Only lower `feed.min_trust` if you genuinely want unreconciled data on dashboards. |
| Qlik shows nothing at all | Check the `StatementFeed` connection points at `feed\`, that files exist, and that the reload ran. |
| Totals split in two | A field set mismatch. Check whether one CSV carries template extras the others do not, and load `transactions` and `review` as separate tables. |
| Special characters garbled | `codepage is 65001` on every `LOAD`. |
