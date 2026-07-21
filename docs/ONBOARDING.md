# Onboarding - running and maintaining the statement engine

For the analyst who looks after this. No software-engineering background needed.
You will do three things: **convert** statements, **add a bank** when a new
layout shows up, and **keep the tests green** so nothing silently breaks.

---

## 0. One-time setup

On a Debian/Ubuntu machine or server:

```sh
apt-get install -y r-base-core \
  r-cran-yaml r-cran-jsonlite r-cran-openxlsx r-cran-pdftools r-cran-readxl \
  r-cran-shiny r-cran-dt r-cran-magick r-cran-testthat \
  tesseract-ocr poppler-utils
git clone -b bank-statement-ocr-platform <your-repo-url>
cd Bank-Statement-OCR
Rscript tests/run_tests.R     # sanity: should print "292 passed, 0 failed"
```

Start the app:

```sh
# for the team (0.0.0.0:8100):        Rscript scripts/run_app.R
# just for you (loopback, dev only):  R -e 'shiny::runApp(".", launch.browser = TRUE)'
```

---

## 1. Convert a statement (daily job)

1. **Convert** tab → **Browse** → pick the statement file (`.csv/.tsv/.tdv`).
2. Leave **Bank** on *auto-detect* (only force a bank if detection is wrong).
3. Click **Convert**.
4. Read the result:
   - **Status** - `OK` (good), `NEEDS_REVIEW` (parsed but a check failed),
     `UNSUPPORTED` (no template matched - add one, see §2), `FAILED` (unreadable
     - the message says why).
   - **Trust** - high / medium / low, from the checks.
   - **Checks** - the reconciliation KPIs (balance, running-balance, count,
     completeness…). A `fail` is flagged, never silently corrected.
   - **Transactions** - a preview of the exact table you'll download.
5. **Download** the Excel / CSV / JSON.

> Rule of thumb: **trust `high`** = ship it. **`medium`** = usually a CSV export
> with no balance to reconcile against - fine, but glance at the preview.
> **`low` / a failed check** = open the source and compare before you rely on it.

---

## 2. Add a new bank - the template toolkit (no code)

When a statement comes in that says `UNSUPPORTED`, you teach the engine its
layout **once**:

1. **Add a template** tab → **Browse** a *sample* of that export → **Open the
   toolkit**. It pre-fills everything it can detect, with the sample rows on
   screen while you check.
2. The toolkit guesses the column mapping - description / reference / balance
   have pickers on the Simple tab; every other field is editable on the
   Advanced tab. What each field means:

   | Field | What to pick | Notes |
   |---|---|---|
   | **date** | the transaction-date column | set **Date format** below |
   | **amount** | the money column | set **Amount style** below |
   | **description** | payee / details / narrative | kept **verbatim** |
   | particulars / code / reference | NZ bank fields, if present | optional → `(none)` |
   | **type** | e.g. "Tran Type" | optional |
   | **other_party** | counterparty account/name | optional |
   | **balance** | running balance column | pick it if present → unlocks the balance check |

3. Check the small settings (all pre-detected; fix only if the preview looks
   wrong):
   - **How are the dates written?** - plain-English choices, e.g. 31/12/2025
     (day/month/year), 31/12/25 (2-digit year), 2025-12-31 (ISO).
   - **How are amounts shown?**:
     - one signed column, minus = money out *(most NZ CSV exports)*
     - a `D`/`C` column decides the sign *(credit cards)*
     - separate withdrawals and deposits columns
     - a `DR`/`CR` suffix on the number (`123.45 DR`)
   - The **fingerprint** (which header names must all be present for this
     template to match) is drafted automatically; fine-tune it on the
     **Advanced** tab if it's too loose or too strict.
4. Watch the **preview** at the bottom. Check: dates look ISO, debits are
   negative, descriptions are intact, no rows missing.
5. **Save template** - writes `templates_user/<id>.yaml`. Done - that bank is
   now supported for everyone on this install.

### Worked example (BNZ everyday)
Header: `Date,Amount,Payee,Particulars,Code,Reference,Tran Type,This Party Account,…`
→ date=`Date` (`%d/%m/%y`), amount=`Amount` (`signed`), description=`Payee`,
type=`Tran Type`, fingerprint = `Date, Amount, Payee, Tran Type, This Party Account`.
That produces exactly `templates/bnz_everyday_csv.yaml` (24 lines, open it to see).

---

## 3. Add a bank by editing YAML (the same thing, in a file)

Copy any `templates/*.yaml`, change the values. Every key is explained in
[`docs/architecture/build-contract.md` §5](architecture/build-contract.md). The
`columns:` block is just *"this canonical field ← that source column"*.

---

## 4. Keep the tests green (so you never break another bank)

After adding/changing a template:

```sh
Rscript tests/run_tests.R
```

To lock in a new bank so it can never silently regress, add a **golden test**:
see [`tests/HOWTO-add-template-test.md`](../tests/HOWTO-add-template-test.md) -
it is: save one correct output as the "expected" file, add a 1-line test. If a
future change alters that bank's output, the test goes red immediately.

---

## 5. PDFs - the honest state, and what "coding in the fields" means

Today the engine **reads** PDFs (all pages, word positions, sections, the
redaction guard, and OCR for scanned pages) but does **not yet turn a PDF into
transaction rows** - because doing that correctly needs a real statement to
measure the column positions from. See [`docs/edge-cases.md`](edge-cases.md) §G.

When you have a real multi-page PDF statement, adding it is **not** free-hand
coding - it's the same fill-in-the-blanks as a CSV, plus geometry:

```yaml
id: anz_everyday_pdf
bank: ANZ
format: pdf                 # <-- the new bit
version: 1
fingerprint:
  page1_contains_all: ["Statement", "Account number"]
table:
  start_anchor: "Date"            # row where the table begins
  end_anchor:   "Closing balance" # row where it ends
  columns:                        # x-position bands (points), read off the sample
    date:        {x_min: 40,  x_max: 110, format: "%d %b %Y"}
    description: {x_min: 110, x_max: 360}
    amount:      {x_min: 360, x_max: 450, align: right}
    balance:     {x_min: 450, x_max: 540, align: right}
  multipage: stitch               # continue the table across pages
  drop_repeating_headers: true    # remove repeated page headers/footers
```

That template is the "fields you code in." The one thing that has to be built
**once** (by me, or whoever maintains this) is the small PDF table reader that
consumes it - a ~day of work against a real sample, then it's tested and it's
just-add-a-YAML forever after, exactly like the CSV path.

---

## 6. Troubleshooting

| You see | It means | Do |
|---|---|---|
| `UNSUPPORTED` | no template matched | add one via the toolkit (§2); the message shows the closest match and which columns were missing |
| `NEEDS_REVIEW` | parsed but a check failed | open Checks; compare against the source; if the template is wrong, fix it and re-run |
| `FAILED` | file unreadable | the message says why (empty, wrong type, password) |
| a check says `na` | that data isn't in the file | expected for balance-less CSV exports; not an error |
| trust `low` | a reconciliation check failed | do not rely on the output until resolved |

## 7. Where everything lives
`app.R` (GUI) · `R/` (engine) · `templates/` (banks) · `tests/` (safety net) ·
`docs/` (this guide, the contract, the edge-case register, the decision log).
