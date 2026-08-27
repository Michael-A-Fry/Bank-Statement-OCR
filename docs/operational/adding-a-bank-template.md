# Teaching it a new layout (no code)

When Convert says **No template for this statement yet**, the tool has not seen
that layout. You teach it once, here, and it converts for everyone from then on.

You need **one example** of the statement. Any real one will do; nothing about it
leaves the server.

## A transaction statement

**Browse the example, open the toolkit, read the preview.** If the dates, the
descriptions and the money-in / money-out signs are right, name the bank and
click **Save template**. On a statement the tool drafted cleanly, that is the
entire job — the bands are already drawn and the columns already mapped from
your own file, and the preview underneath is showing you the result.

So: **Add a template** tab → leave the question on *A bank or card statement* →
**Browse** the example. The toolkit opens on the upload; there is no button to
press. (**Open the toolkit** appears under the file picker afterwards — it is the
way back in if you press Cancel, not a step.)

The toolkit prints its own numbered strip along the top. On a **PDF** it reads:

> **1** Drag a box over a column and say what it is · **2** click **Assign it** ·
> **3** name the bank on the right · **4** check the **Preview** below ·
> **5** **Save**.

On a **CSV, TSV or Excel** file:

> **1** check each column picker matches your file · **2** name the bank ·
> **3** check the **Preview** below · **4** **Save**.

**Do them in the order 4, 3, 5** on a PDF — preview, bank, Save — or 3, 2, 4 on a
delimited file. Steps 1 and 2 of the PDF strip, and step 1 of the other, are
**corrections**: what to do when the preview is wrong. The controls say so
themselves — *"Leave as detected unless the preview looks wrong."*

### Check the Preview (PDF step 4 · delimited step 3)

This is the step that matters, and it is the one you *can* do: it is your own
transactions, on screen, as the tool will write them out. Read it line by line
for four things:

- the dates are the right dates;
- **money out is negative**;
- descriptions are complete, not cut off;
- no row is missing from the middle;
- **the balances follow from one row to the next** — if there is a Balance column,
  each row's balance should be the one above it plus that row's amount. A jump
  means a row is being skipped, even though the preview looks complete. This is
  the one that catches a missing row you cannot see, because the statement's own
  arithmetic notices it and your eye does not.

**Read the line above the preview before you read the preview.** The toolkit
checks those last two for you and says so:

- *N transaction rows read* with a green tick — nothing argues against it. If the
  rows are right, Save.
- *N transaction rows read — but not all of the page*, in amber, with the reasons
  listed underneath: "the balances do not follow in 11 place(s): rows are probably
  being skipped", or how many rows were skipped for an unreadable date or a
  missing amount, or which pages carried words and kept nothing. **Do not Save on
  an amber line** — fix the column boxes or the date format and watch it change.

On a PDF the preview reads only the first few pages, and the line says so
(*"read from the first 3 of 11 pages"*) — converting reads every page, so expect a
bigger number then.

**If the preview is empty, stop and read [When the preview reads
nothing](#when-the-preview-reads-nothing)** — an empty preview is not something
you fix by pressing on.

### Name the bank (PDF step 3 · delimited step 2)

One box on the right: *Which bank is this statement from?* It arrives filled in —
the tool reads the name off the top of page one and off the bank's own legal
imprint, not off a payee line, so a bank it has never been taught still gets its
own name. **Confirm it, correct it if it is wrong**, and move on. It is the name
the template is saved under; it is not what recognises the bank next time (that is
the Identifying phrase, below).

### Save (PDF step 5 · delimited step 4)

**Save template.** The tool then re-runs full detection over your file against
every template it has — not a forced match, a real one — and tells you in words
whether it will be recognised on its own next time.

If it says *saved, but this statement is NOT recognised by it yet*, read the
whole message. Usually the Identifying phrase is not printed exactly as it was
typed (see below). If it instead says *ambiguous: X and Y both score N*, nothing
you typed is wrong: an existing template fits your statement equally well and the
tool will not choose between them. That is a duplicate for whoever maintains the
engine to retire.

## When the preview is wrong

Everything in this section is a correction. On a good draft you will not open any
of it.

**The columns are mapped to the wrong thing.** Date and Description are required;
Amount, Reference and Balance are offered too. Pick the **balance** column if the
statement has one — it is what unlocks the balance checks. Without a balance and
without a printed opening/closing pair, nothing can prove a row was not dropped,
and the confidence level is capped at *medium* whatever else is right. On a PDF
this is where the strip's steps 1 and 2 come in: drag a box over the column and
click **Assign it**. Only a box's left-right position matters — a column runs the
full height of the page.

**The dates or the signs are wrong.** Two pickers sit in front of the disclosure,
both with the tool's best guess already selected, and the preview reacts as you
change them:

- **How are the dates written?** — `31/12/2025`, `31/12/25`, `2025-12-31`,
  `31 Dec 2025`…
- **How are amounts shown?** — one signed column (minus = out); separate
  withdrawals/deposits columns; a `DR`/`CR` suffix on the number; a short `D`/`C`
  indicator column; or unsigned, where a bare number is a charge and `CR` marks a
  payment (credit cards).

  Picking the indicator style asks one more: *which indicator value means money
  OUT*. Get this backwards and every debit becomes a credit, with no error
  anywhere — check the preview's signs against the statement before saving.

**The save was refused, or it saved but is not recognised.** Open **Show the
settings for this statement** — or rather, let the tool open it: a refused save
opens that panel for you and names the problem. Behind it: the **Identifying
phrase**, the name it saves under, the **Currency**, the kind of statement, the
number punctuation, and when this layout applies. Everything is filled in
already. The one worth a glance even when nothing is wrong is **Currency**,
below.

Inside is the **Identifying phrase**, which is how the bank is recognised next
time.

The toolkit fills it in for you from phrases it actually found on your page, and
offers those phrases in a dropdown. You only touch it when the tool says so.
When you do:

- It is matched **character for character** — `OPENING BALANCE`, `Opening
  Balance` and `Opening balance` are three different phrases. Copy it off the
  page exactly, including capitals and spacing.
- Prefer the masthead or a distinctive footer, and never a customer's name. One
  phrase per line; **all of them must appear**.
- The tool refuses to save a phrase so generic it would claim other banks'
  statements, and tells you which phrase was the problem.

**Currency** is in the same panel, and it is the one setting in there that is
wrong quietly. The tool reads it off the money on your own statement — the money
cells only, never a description, because `VISA PURCHASE USD 25.00` is one
customer's foreign purchase, not the currency the statement is printed in.

**How it decides.** It collects every currency it can name from the figures, and
it saves that name **only if there is exactly one of them**. Anything else —
nothing named, or two names that disagree — leaves **NZD** standing for you to
confirm. It is not a ladder where a code outranks a symbol; they go into the
same pile.

It saves what it read when:

- a **code** is printed immediately before a figure — `NZD 25.00`, `USD 25.00`,
  and `AUD $25.00` too, because a dollar sign between a dollar code and its
  figure is how a statement disambiguates its own dollar;
- or a **distinct symbol** is — `£25.00` saves as GBP, `€25.00` as EUR.

It falls back to **NZD** when:

- **the figures carry a bare `$`.** It is the New Zealand, Australian and US
  dollar alike and the glyph does not say which, so nothing is guessed;
- **the figures carry no mark at all** — most NZ statements print `1,234.56`;
- **two marks disagree.** `£25.00` beside `$4.10`, or `GBP 25.00` beside a bare
  `$4.10`, or an `NZD` figure beside a `USD` one: all save as NZD, because the
  page contradicts itself and a template carries one currency;
- **the mark is one this build does not know.** It knows six codes — NZD, AUD,
  USD, GBP, EUR, JPY — and four symbols: `$`, `£`, `€`, `¥`. `CAD $25.00` and
  `SGD 25.00` name nothing it recognises, so both save as NZD;
- **the mark is not printed against a figure with cents.** The mark has to sit
  immediately before something shaped like `25.00` or `1,234.56`. A code printed
  *after* the figure (`25.00 GBP`) is not read, and neither is a symbol on a
  whole number (`¥2500`). `¥2,500.00` is.

So **NZD in that box is usually a fallback, not a reading** — it is what the tool
says when nothing on the page named a currency, or when two things named
different ones. It looks identical either way. **If the statement is not in New
Zealand dollars, look at this box before you Save.** A currency is a label on
money, and a wrong one is a wrong figure: it stamps the currency column and the
totals above the transactions table.

**The complete template as text** is the toolkit's **Advanced** tab. Opening the
tab fills the box with the settings as they stand, so it can never show you a
stale copy; *Check & apply* validates and updates the preview, and a syntax error
is reported with its line and column. The **Simple** tab picks up whatever you
applied.

If you have typed in the box and go back to **Simple**, your typing is still
there when you return — the box is only refreshed while it is untouched, and the
line above it says so.

## When the preview reads nothing

The toolkit's auto-draft does not succeed on every real bank PDF. Two things go
wrong, and the fix is different for each:

- **The boxes are in the wrong place.** Common on dense PDFs: the proposed date
  box is a few points too narrow for the dates it has to hold, so no row parses.
  Drag the date box wider first — everything downstream depends on rows being
  found at all.
- **It is not a transaction table.** An IRD certificate, a KiwiSaver annual
  statement, a summary letter. Click **Not a transaction table?** in the toolkit:
  it closes, switches the Add-a-template tab to *Anything else*, and brings your
  document with it, so you land in the labelled-value builder with nothing to
  re-upload.

  If the toolkit **never opened** — you got a notification instead — the draft
  found no table at all. There is no link in a window that did not appear: set
  the radio on the Add-a-template tab to *Anything else* yourself.

If neither works, do not guess your way to a template that reads *something*: a
template that reads the wrong part of the page is worse than no template. Use
[survey-a-statement-with-ai.md](survey-a-statement-with-ai.md) to describe the
layout with no client detail in it, and send that on.

## A form or summary (labelled values)

For a document with no transaction table — an IRD certificate, an annual
statement, a letter with figures in it — pick **Anything else — a report, a
form, a summary, a letter** on the Add a template tab. The builder appears below the upload:

1. **+ Add a value**, then **draw a box** round the value and one round its
   label. The tool reports what it read inside each box and pre-fills a name.
2. Check **Call this value** and **What it is** (money / date / date range /
   text), then **Save this value**. Repeat for the values that matter.
3. **Read the whole document** — it reports what each value came out as, and
   which were not found. If a value you have already drawn a box round is not
   found, the box read nothing: redraw it slightly larger, or type the wording
   instead under *Know the wording already? Type them instead*.
4. Name it (**Who produced this document?**), give it **a phrase printed on it**
   (all of them must appear), and **Save template**.

**The gotcha to know before you start,** and the save message now says it:
*"Saved '<name>'. On Convert it is used when no statement template reads the
document."* A form template is only tried when the transaction-statement path
finds nothing, so if any statement template already recognises your document,
that one wins and your form template never runs. Check the result names your
form; if it names a bank statement template, tell whoever maintains the engine.
(If you want to force it, set **What is this?** to **Something else - a report, a
form, a letter** on Convert.)

## Templates built here vs. shipped templates

- A template you save here is used **by default**, for everyone, from the next
  conversion. There is no tick-box to find — whether colleagues' templates count
  is a deployment setting, `app.user_templates_default` in `config\config.yaml`.
- What being built-here *does* change: the conversion is **held back from the
  Qlik dashboards**. Your downloads are complete either way. Promoting it is a
  maintainer job ([maintaining-the-engine.md](maintaining-the-engine.md) §3).

## Correcting a bank the tool already knows

Open the statement in the toolkit, fix what is wrong, Save. The shipped template
is left alone so it can still be updated; your copy is saved as `<id>_custom`
with a `refines:` line naming the one it replaces, and detection honours that —
which is what makes your fix win. Without it the two would tie on every statement
(same fingerprint) and the shipped one would keep winning.

Related: [converting-statements.md](converting-statements.md) ·
[when-something-goes-wrong.md](when-something-goes-wrong.md) ·
[survey-a-statement-with-ai.md](survey-a-statement-with-ai.md)
