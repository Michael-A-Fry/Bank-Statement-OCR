# The architecture, locked

For whoever inherits this, and for settling an argument without re-running the
research. Every decision below is **closed**, with the evidence that closed it and
the one thing that would reopen it. If you are about to change one, read its row
first — most of them were tried the other way and measured.

> **Reopened at 2.0.0 (3 Oct 2026).** D2 (per-bank templates), D8 (the tool never
> re-aims its own columns) and D10 (a template that stops fitting says which
> column) were reopened by the product owner on new evidence: on 128 realistic
> statements the shipped templates read none perfectly, and per-file drafted
> templates read a quarter, three of them "ok" but wrong
> ([../auto-reading-spec.md](../auto-reading-spec.md) section 9.1). They are
> superseded by automatic reading, which keeps what those decisions protected:
> still **not a model** (D2's real point), still **reproducible** (D8's: every
> output is stamped with the build and the learned state, and a learned layout
> is never edited), and still **refuse rather than guess** (D6). D1, D3 to D7 and
> D9 stand. The rest of this page is kept as the record of why each was decided.

The short answer to "are we staying with the current architecture?" was **yes, and
nothing in it is provisional**. An analyst builds a template for a bank, uses it on
every statement from that bank, and when a statement stops fitting, the tool now says
**which column** and either they fix it or they escalate. That last part is the only
thing that changed; see [D10](#d10).

---

## 1. The pipeline, in order

This is the locked order of operations. Each stage may only use what the stages above
produced. Nothing here reaches outward — no network, no service, no second engine.

**Reading a PDF** (`R/read_pdf.R`):

| # | Stage | Why it is here and not later |
|---|---|---|
| 1 | text layer + **word boxes** (`pdf_text`, `pdf_data`) | the boxes are the whole basis of column assignment; text alone cannot be banded |
| 2 | page geometry + document provenance (`pdf_pagesize`, `pdf_info`) | the band frame needs the page shape before any x is interpreted |
| 3 | **ink reconciliation** (`pdftocairo -svg`, `.apply_ink_signs`) | a minus drawn as a line, or printed in the background colour, is a **silently inverted figure**. It must be fixed in the word boxes before anything reads them |
| 4 | rotation check | a rotated page faces the other way from its page box; every x below would be wrong |
| 5 | redaction guard (markers, then overlay) | a blacked-out value must become a token before it can be parsed as a number |
| 6 | OCR routing, per page | routed on **word boxes**, not a character count: a digital page is never OCR'd, a scan carrying a thin text stamp still is |
| 7 | raster occlusion scan | the last resort, and the only one that sees a box of any colour |

**Converting** (`R/convert.R`):

```
read_input  ->  detect_statement  ->  parse_statement  ->  reconcile
                                                              |
              log_run  <-  write_outputs  <-  build_diagnostics
```

Two properties of that order are load-bearing:

- **`reconcile` runs before `build_diagnostics`, and never the other way.** The trust
  level is computed from the checks alone, so it cannot be talked up or down by a
  diagnostic. `.reconcile_trust` therefore cannot see the diagnostics and must not
  rank a failing check against them — the comment in `R/reconcile.R` records the one
  time it tried and contradicted another line of the same screen.
- **`write_outputs` runs after both.** No figure reaches a spreadsheet before it has
  been checked and the checks have been turned into words.

---

## 2. The locked decisions

<a name="d1"></a>
### D1. One pipeline. Not several engines voting.

**Decision.** A single deterministic reader, with the running balance as its
verifier. Not three extractors compared and merged.

**Evidence.** N-version programming does not deliver its theoretical gain because the
failures are not independent: Knight & Leveson rejected independence at z = 100.55,
and replications put a 3-version ensemble at **0.43** of the theoretical gain, falling
**below 0.30** when the versions share a base. Ours would share a base —
`pdfplumber` *is* `pdfminer`, and Camelot was. The ICDAR-2021 table-recognition winner
measured 3-model ensembling at **+0.2% TEDS**, its smallest lever. And Stroebl et al.
show the ceiling: accuracy is bounded by the **verifier's** false-positive rate, so
resampling more readers cannot pass it.

**What would reopen it.** Nothing about model quality. Only a verifier strictly
stronger than the running balance.

> Multi-engine comparison still belongs somewhere, and it is not production.
> Buckleton et al. (ESR, STRmix) found **11 of 14** miscodes in forensic software by
> *parallel calculation of intermediate results*, and **zero** by code review. That is
> a **validation** technique: run a second implementation when you are proving the
> first one right, not on every conversion.

<a name="d2"></a>
### D2. Per-bank templates. Not a model. (SUPERSEDED at 2.0.0: still not a model; no templates)

**Decision.** A declared template per bank layout, built by an analyst, versioned,
and treated as evidence.

**Evidence.** AI-BAAM (ICLR 2026) put both on the same 110 real bank statements and 16
configurations: hand-written per-bank templates scored **100.00 exact-match at 0.11s
and $0**; the best LLM pipeline **94.79 at 11.92s and $0.53**. On this document class
generic table extraction is far worse still — Camelot's own FinTabNet.c borderless
benchmark (545 financial PDFs) reports **row accuracy 0.235 neural, 0.109
heuristic**, which is the best end-to-end figure in the literature here.

**It is also the only shape that can be audited.** A template says, in a file a person
can read, why a figure came out of that spot. A model cannot be cross-examined.

**Corroboration.** Four mature open-source ecosystems — `ofxstatement`,
`beancount`/`beangulp`, `hledger`, `bank2ynab` — converged independently on exactly
this: identify, then a reusable format reader, then per-institution declarations, then
a shared normaliser and validator. Nobody who has lived with the problem has chosen
otherwise.

<a name="d3"></a>
### D3. R. The Python port is closed.

**Decision.** The engine stays in R. Python stays a **dev-time** tool
(`tools/synth/`), and nothing in it ships to the server.

**Evidence.** The port was costed against its two claimed benefits and delivers
neither. *Access* is a deployment question, not a language one — ShinyProxy or a
reverse proxy serves an R app to the whole organisation exactly as it would a Python
one. *Guaranteeing results* is the suite and the corpus, which would have to be
rewritten, re-validated and re-baselined; the guarantee would be weaker on the day of
the port than it is now, and the figures would be unattributable across the boundary.

<a name="d4"></a>
### D4. x-bands, assigned on the word's centre.

**Decision.** Columns are gapless, non-overlapping x-ranges in one frame. A word joins
the band containing its horizontal **centre** (`.pdf_cell`).

**Why it stays.** It is the only model that makes the column assignment a
**partition** — every word lands in exactly one column, and no rule is needed to
settle a tie. `R/column_fit.R` depends on this directly: because the bands are a
partition, shifting all of them by one offset keeps them a partition, so a page-wide
drift can be searched for without any overlap logic at all. The constraint comes from
the geometry rather than from a rule somebody has to maintain.

**Measured tolerance.** Bands are 65–77pt wide, so **25pt of drift changes nothing**.
55pt does, and is caught — [D10](#d10).

<a name="d5"></a>
### D5. The running balance is the verifier.

**Decision.** Completeness and correctness are proved by arithmetic the statement
carries itself: `balance[i] - balance[i-1] == amount[i]`, and
`opening + sum(amounts) == closing`.

**Why this is the whole reason the tool can be trusted.** A bank statement is the rare
document that ships with its own answer key. This is **footing and cross-footing**, a
recognised evidence-gathering procedure under **PCAOB AU 326.19** — not an invention
of ours. It is also why [D1](#d1) holds: the verifier, not the reader, sets the
ceiling.

**And it is why the balance column matters out of proportion.** A statement with no
running balance is read, but nothing *proves* it. That is why "balance is empty on
every row" is reported even when the conversion looks perfect — the strongest check
there is had nothing to test.

<a name="d6"></a>
### D6. Refuse rather than guess.

**Decision.** A cell that cannot be read becomes `NA` and the row carries `malformed`.
A wrong figure is never emitted in preference to a gap.

**Why it is a separate decision from accuracy.** The synthetic corpus scores
**fabricated** and **refused** in different columns for this reason. When the
contaminated-cell guard went into `.num_one`, fabricated went **13 to 0** while refused
rose **0 to 20** — a strictly better engine that one combined accuracy figure would
have scored as unchanged. `FABR` must be 0. `refus` is a gap a reviewer can see.

<a name="d7"></a>
### D7. No Docker. No container runtime on the server.

**Decision.** The app runs as a **single R process** on the Windows server, started as
a service, behind IIS. There is no container runtime, no ShinyProxy, no Linux guest.

The container work was written, costed, and then **deleted** — `deploy/Dockerfile`,
`deploy/shinyproxy-application.yml`, the operational page for it, and the app's own
`SHINYPROXY_USERNAME` identity branch. Deleted rather than shelved on purpose: a
deployment option sitting in the tree that cannot run in the only environment this
tool is installed in is a thing the next maintainer has to read, cost and reject
again. If the triggers at the end of this section are ever met, it is in the history.

**The licensing trap first, because it decides it.** **Docker Desktop requires a paid
subscription for government entities, unconditionally** — the employee-count and
revenue thresholds that exempt small businesses do not apply, and NZ Police is a
government entity. Docker *Engine* on Linux is free, so the no-cost path is a Hyper-V
Linux guest inside the Windows server, with Docker Engine and ShinyProxy inside it.

**From a maintenance point of view, honestly both ways:**

| | With containers | Without |
|---|---|---|
| **Isolation** | the **file system** enforces it: a container per analyst, each mounting only its own directory. A bug in the app's own checks stops being a cross-case evidence leak | isolation is the app's own code, re-established on every commit. Every upload is owned by one service account, so NTFS cannot tell two investigators apart |
| **Concurrency** | one R process per analyst; contention moves to the host scheduler | one R process for everyone. `R/jobs.R` already pushes conversions into child processes for this reason — measured: one analyst's scan froze another's browser for 65 seconds before that change |
| **Upgrades** | one image, pinned by version, carried as a tar. A figure is attributable to a build | drag the changed files across, which is what you are doing, and which is why every commit here names the files that matter |
| **What you must learn** | Hyper-V, a Linux guest, Docker Engine, ShinyProxy config, LDAP binding, volume permissions, uid matching, heartbeat tuning | nothing new |
| **What breaks at 3am** | more layers, each with its own failure mode, and the one that bites is permissions on the mounts | the app, which you already know |
| **Cost** | free only via the Linux-guest route; Docker Desktop is licensed | nil |

**The recommendation, plainly: do not set up Docker.** The isolation it buys is real,
but it is the *second* thing to do about access, not the first, and you would be
taking on five new layers on an air-gapped box you maintain alone. Do
[who-is-using-it.md](../../operational/who-is-using-it.md) instead — IIS with
Windows Integrated Auth, a shared-secret identity header, and the app bound to
loopback. That is an afternoon, it needs no new technology, and it gets you a named
person in the download log, which is the thing you actually asked for.

**Revisit containers only if one of these becomes true**, and then recover the files
from git history rather than rewriting them: a second analyst needs to work the same
case concurrently; an audit requires OS-enforced separation rather than
application-enforced; or someone takes over the server who already runs Docker.

Two things that are **not** options, so nobody spends a week on them: **Shiny Server
does not run on Windows**, open source or Pro, and **Shiny Server Pro support ended in
March 2026**.

<a name="d8"></a>
### D8. The tool never re-aims its own columns. (SUPERSEDED at 2.0.0: the reader finds the columns on every statement; reproducibility now rests on the learned-state stamp)

**Decision.** No stage silently adjusts a template. `column_fit` reports an offset; a
person applies it.

**Why.** A template is the record of *how a statement was read*. A reader that moved
its own bands would make two runs of the same file on the same template
incomparable — and in a prosecution the question "why does this spreadsheet differ
from the one you produced in March?" has to have an answer other than "the tool
adjusted itself".

<a name="d9"></a>
### D9. No chaining to Excel, and no merging of several extractors.

**Decision.** One reader. Nothing is piggy-backed off Excel, Power Query, or a second
extraction tool whose output we then reconcile.

**This was the creative option and it is worth saying exactly why it loses.**

*Excel does extract well* — from a **delimited** file, where the columns are already
decided. That is the easy half, and this engine already does it (`R/read_input.R`
reads CSV, TSV and XLSX directly). What Excel cannot do is the hard half: Power Query's
PDF connector finds *tables*, not **bands**, and on a bank statement it produces a
different column split per page — a wrapped description becomes a new row, a right-
aligned amount lands in the neighbouring column, and a page whose header did not
repeat starts a fresh table. You would then be writing merge logic to stitch its
output back together, which is the same problem one level further from the evidence,
with no word coordinates left to reason about and nothing to check the result against.
Camelot's borderless row accuracy of **0.235** ([D2](#d2)) is the measured ceiling for
that whole family of approaches.

*Chaining and comparing outputs* fails for the reason in [D1](#d1): the ceiling is the
verifier, not the number of readers, and we already have the strongest verifier the
document offers. Two readers that disagree leave you choosing, and the only honest
basis for choosing is the arithmetic — which the single reader is already using.

*And it would break [D8](#d8).* Four tools' worth of merge heuristics is not something
an analyst can be cross-examined on.

**What we did take from the idea, because it was the right instinct.** Use *several
independent facts about one page* rather than several tools:

- the **text layer** and the **ink actually drawn** must agree about a sign
  (`.apply_ink_signs`) — two sources, one page;
- the **column bands** and the **running balance** must agree about an amount
  (`amount_from_balance` and `column_fit`). Measured, on
  `band_overflow_debit_only`: the 11 rows `column_fit` calls unreadable are *exactly*
  the 11 the engine had to derive from the balance. Two independent mechanisms,
  same answer, neither told the other.

That is cross-checking with collective knowledge. It just comes from inside the
document instead of from a second vendor.

<a name="d10"></a>
### D10. A template that stops fitting says which column. (SUPERSEDED at 2.0.0: the reader says which row and page did not add up)

**Decision.** The template-per-bank architecture stays, and the failure path is
explicit: the tool names the column, the analyst edits the template or escalates.

**The failure path, concretely.** This is the answer to "something goes wrong, user has
to edit template or send to someone who can":

| What the analyst sees | Who fixes it | How they know |
|---|---|---|
| `a column is not where the template says` (**medium**) | the analyst, or whoever maintains templates | the diagnostic names the column, the direction, and the range of offsets that work. Fix owner is `template` |
| `a column is not where the template says` (**info**) | nobody | the statement does not print that column. Recorded because if it is the balance, nothing was *proved* — [D5](#d5) |
| `the account number does not look right` (**medium**) | whoever holds the **image** | fix owner is `input`, not `template`: every other failing check points at the mapping, this one points at the scan |
| `matched the wording, read no transactions` | templates | and `column_fit` now runs on this path too — it is the case the offset is worth the most on |

**What it will not do** is tell you the exact drift. It cannot: an amount is a point
inside a 65pt band, so on the measured case the debit band read its figure for **every
offset from -69 to -4**. The honest output is the whole interval, and the
recommendation is its **centre**, which leaves 32pt of slack either side where the
real -55 leaves 14. The centre is better advice than the truth.

---

## 2b. What it costs on a real job

Not a decision, but the thing every decision above has to survive. Measured at 1.10.0
(`tools/synth/bench.py` + `bench.R`), on statements that reconcile exactly:

| pages | rows | total | per page | wrong figures |
|---|---|---|---|---|
| 100 | 3,000 | 17.0 s | 0.17 s | 0 |
| 200 | 6,000 | 33.6 s | 0.17 s | 0 |
| 400 | 12,000 | 69.5 s | 0.17 s | 0 |

**Linear in pages, flat per page, memory negligible.** There is no size at which this
falls over, which is the only property worth locking.

It was not always so, and both causes are worth remembering because they are the same
mistake in two places: **a cost that is linear in pages buying information that is
not.** The drift check read every page when eight spread across the document answer
the question (46% of a 100-page conversion, now 1.7 s flat). The vector-redaction scan
rasterised every page when the render pass already done for the sign check says which
pages draw anything that could hide text (14.6 s of reading, now 3.8 s).

`parse` is ~70% of what remains and is diffuse -- a third of it in `sub`/`gsub`/`grepl`
spread across cell normalisation, no single hot spot. Making it faster means changing
`.num()`, the money reader, and that is not a change to make for speed.

---

## 3. The decision metrics, and their status

Everything the tool can use to decide whether a conversion is trustworthy.

| Metric | What it proves | Status |
|---|---|---|
| `balance_reconciliation` | opening + every amount = closing. Completeness | **built**. `na` when every amount was derived from the balance — the sum would telescope and prove nothing |
| `running_balance_continuity` | each balance follows from the last. Per-row correctness | **built**. `na` when fewer than 2 rows were not derived |
| `amount_direction` | money in/out is the right way round | **built** |
| `transaction_count` | against the count the statement prints | **built** |
| `dates_within_period` | every date falls in the stated period | **built** |
| `dates_readable` | the date column was found and parsed | **built** |
| `no_unparsed_rows` | no row was silently dropped | **built** |
| `account_number` (shape) | the account number was read intact | **built**, this change |
| `column_bands` | the template still fits the page | **built** |
| `sign_scan_unavailable` | the minus-on-the-page check actually ran | **built**. High severity: both ink faults are invisible to the text layer, in opposite directions, and nothing else catches either on a statement with no running balance |
| `redaction_summary`, `redaction_scan`, `ocr_confidence` | informational / was anything hidden read | **built** |
| account number **check digit** | the account number is arithmetically valid | **researched, deliberately not built** — [account-number-check-digit.md](../account-number-check-digit.md) |
| printed column **totals** cross-check | the sum of our debits equals the total the statement prints | **not built**. Free where a statement prints them; needs a template field to pin the region |
| **duplicate row** detection | the same transaction read twice (a repeated header, an overlapping split) | **not built**. Cheap: same date, amount and description on adjacent rows |
| fee/interest **recomputation** | a stated rate applied to a stated balance | **not built**, and probably not worth it — it only applies to some accounts and the balance already covers it |

The first eight share one property worth stating: **every one of them is checkable by a
person holding the statement.** An `expected` figure may be something the statement
prints, or a plain target — never a number synthesised from our own skips, because a
figure in a column headed *Expected* that appears nowhere in the document leaves a
reviewer unable to check the checker.

---

## 4. What is deliberately NOT locked

Open, and honestly so:

- **Case ownership.** Each analyst can be made to see only their own uploads
  ([who-is-using-it.md](../../operational/who-is-using-it.md)), and the download log
  makes cross-case reading *visible*. Neither makes it *impossible*, and there is no
  notion of a case with an owner and a grant list. A supervisor handover has no
  mechanism except "upload it again".
- **OFX / GIFTS readers.** A better *reader* and a worse *proof*: those formats carry
  clean fields but, unlike the PDF, no per-row running balance — so [D5](#d5) would
  have nothing to work with. Worth building, but not as a replacement.
- **The check digit.** [Its own page](../account-number-check-digit.md) says what
  finishing it needs: one real statement per bank, to validate against.

## Related

- [build-contract.md](build-contract.md) — the module map and the engine's contract
- [../charter.md](../charter.md) — the interface rule, and "refuse, explain, never guess"
- [../../operational/who-is-using-it.md](../../operational/who-is-using-it.md) — do this
  instead of containers
- [../account-number-check-digit.md](../account-number-check-digit.md) — the one metric
  researched and not built
