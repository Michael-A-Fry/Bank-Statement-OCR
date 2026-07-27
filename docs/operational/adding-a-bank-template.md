# Teaching it a new layout (no code)

When Convert says **No template for this statement yet**, the tool has not seen
that layout. You teach it once, here, and it converts for everyone from then on.

You need **one example** of the statement. Any real one will do; nothing about it
leaves the server.

## A transaction statement

1. **Add a template** tab → **Browse** the example → leave the question on *A
   bank or card statement* → **Open the toolkit**.

2. The toolkit opens with the statement on the left and the settings on the
   right, already filled in from your file. **Look at the preview at the bottom
   before you change anything.**

   **If the preview is empty, stop and read the next section** — an empty preview
   is not something you fix by pressing on.

3. **Check the columns.** Date and Description are required; Amount, Reference
   and Balance are offered too. Pick the **balance** column if the statement has
   one — it is what unlocks the balance checks. Without a balance and without a
   printed opening/closing pair, nothing can prove a row was not dropped, and the
   confidence level is capped at *medium* whatever else is right.

4. **Answer the two questions the tool cannot answer for you.** Both come with
   its best guess already selected; the preview below reacts as you change them.

   - **How are the dates written?** — `31/12/2025`, `31/12/25`, `2025-12-31`,
     `31 Dec 2025`…
   - **How are amounts shown?** — one signed column (minus = out); separate
     withdrawals/deposits columns; a `DR`/`CR` suffix on the number; a short
     `D`/`C` indicator column; or unsigned, where a bare number is a charge and
     `CR` marks a payment (credit cards).

     Picking the indicator style asks one more: *which value means money out*.
     Get this backwards and every debit becomes a credit, with no error anywhere —
     check the preview's signs against the statement before saving.

5. **Check the preview**, line by line, on these four things: the dates are the
   right dates, money out is negative, descriptions are complete, and no row is
   missing from the middle.

6. **The identifying phrase.** This is what recognises the bank next time. It is
   matched **character for character** — `OPENING BALANCE`, `Opening Balance` and
   `Opening balance` are three different phrases. Copy it off the page exactly,
   including capitals and spacing. Prefer the masthead or a distinctive footer;
   the tool refuses to save a phrase so generic it would claim other banks'
   statements, and tells you which phrase was the problem.

7. **Save template.** The tool then re-checks your file against every template it
   has and tells you in words whether it will be recognised on its own next time.

   If it says *saved, but this statement is NOT recognised by it yet*, read the
   whole message. It suggests the phrase is not printed exactly as typed — usually
   true, but not always. If it also says *ambiguous: X and Y both score N*, the
   phrase is fine; an existing template fits your statement equally well and the
   tool will not choose between them. That is a duplicate for whoever maintains
   the engine to retire, not something you can fix by editing your phrase.

**PDFs** work the same way with one extra step: you drag boxes over the columns
on the page image, and the preview shows the rows being read out as you move
them.

**Everything as YAML** is on the toolkit's **Advanced** tab — *Load current
settings* fills it, *Check & apply* validates and updates the preview, and a
syntax error is reported with its line and column. The Simple tab picks up
whatever you applied.

## When the preview reads nothing

The toolkit's auto-draft does not succeed on every real bank PDF. Two things go
wrong, and the fix is different for each:

- **The boxes are in the wrong place.** Common on dense PDFs: the proposed date
  box is a few points too narrow for the dates it has to hold, so no row parses.
  Drag the date box wider first — everything downstream depends on rows being
  found at all.
- **It is not a transaction table.** An IRD certificate, a KiwiSaver annual
  statement, a summary letter. Click **Not a transaction table?** in the toolkit:
  it closes, switches the Add-a-template tab to *Something else*, and brings your
  document with it, so you land in the labelled-value builder with nothing to
  re-upload.

  If the toolkit **never opened** — you got a notification instead — the draft
  found no table at all. There is no link in a window that did not appear: set
  the radio on the Add-a-template tab to *Something else* yourself.

If neither works, do not guess your way to a template that reads *something*: a
template that reads the wrong part of the page is worse than no template. Use
[survey-a-statement-with-ai.md](survey-a-statement-with-ai.md) to describe the
layout with no client detail in it, and send that on.

## A form or summary (labelled values)

For a document with no transaction table — an IRD certificate, an annual
statement, a letter with figures in it — pick **Something else** on the Add a
template tab. The builder appears below the upload:

1. **Draw a box** round a value you want. The tool reports what it read inside
   the box and the wording printed beside it, and pre-fills a name.
2. **Add this value**, pick its kind (money / date / date range / text), repeat
   for the values that matter.
3. **Preview on the document** — it reports *N of M values found* and which are
   missing. If it says a value was not found and you have already drawn a box
   round it, the box read nothing: redraw it slightly larger, or type the wording
   instead under *Know the wording? Type them instead*.
4. Name it, give it **a phrase printed on it** (all of them must appear), and
   **Save template**.

**The gotcha to know before you start.** A form template is only tried when the
transaction-statement path finds nothing. If any statement template already
recognises your document, that one wins and your form template never runs — so
the save message *"now upload the document on the Convert tab"* can be followed
by the document converting as a statement instead. Check the result names your
form; if it names a bank statement template, tell whoever maintains the engine.

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
