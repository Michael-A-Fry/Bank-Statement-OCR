# Teaching it a new bank (no code)

When a statement comes in that the app calls **UNSUPPORTED**, it just means the app
hasn't seen that bank's layout yet. You teach it **once**, in the built-in wizard —
by pointing and clicking, never by writing code. After that, everyone on the server
can convert that bank.

---

## Steps
1. Go to the **Add a template** tab → **Browse** a sample of that export → **Open
   the toolkit**. The app pre-fills everything it can detect, with the sample rows on
   screen so you can check as you go.
2. **Confirm the columns.** The toolkit guesses which column is which; fix any it got
   wrong:

   | Field | Pick the column that holds… | Notes |
   |---|---|---|
   | **date** | the transaction date | then set the **date format** below |
   | **amount** | the money value | then set the **amount style** below |
   | **description** | the payee / details / narrative | kept exactly as written |
   | particulars / code / reference | NZ bank fields, if present | leave as `(none)` if absent |
   | **type** | e.g. "Tran Type" | optional |
   | **other party** | the counterparty account/name | optional |
   | **balance** | the running-balance column | pick it if present — it unlocks the balance check |

3. **Confirm the small settings** (all pre-detected — only change them if the preview
   looks wrong):
   - **How are the dates written?** — e.g. `31/12/2025` (day/month/year), `31/12/25`
     (2-digit year), `2025-12-31` (ISO).
   - **How are amounts shown?**:
     - one signed column, minus = money out *(most NZ CSV exports)*
     - a `D`/`C` column decides the sign *(credit cards)*
     - separate withdrawals and deposits columns
     - a `DR`/`CR` suffix on the number (`123.45 DR`)
   - The **fingerprint** (the header names that must all be present for this template
     to match) is drafted for you; tweak it on the **Advanced** tab only if it's too
     loose or too strict.
4. **Watch the preview** at the bottom. Check: dates look right, money out is
   negative, descriptions are intact, no rows missing.
5. Click **Save template**. Done — that bank is now supported for everyone, and the
   app re-converts your sample so you can confirm it worked.

There's an **ⓘ guide** in the tab covering the ways statements differ.

---

## PDF statements
It's the same fill-in-the-blanks, with one extra step: you **draw boxes** over the
columns on the page, and the live preview shows the rows being read out. Everything
else — dates, amount style, saving — is identical.

---

## Proven vs. user-created templates
- Templates you save here are **user-created**. They are **used by default** on the
  **Convert** tab — a bank you set up works for everyone from the next conversion,
  with no tick-box to find. (The switch is still there, under "It picked the wrong
  bank?" → *Include templates built here*; untick it to convert against only the
  shipped, tested set. `app.user_templates_default` in `config\config.yaml` sets
  which way it starts.)
- Only **proven** templates feed the Qlik dashboards (see
  [connecting-qlik.md](connecting-qlik.md)) — this is the one thing being
  user-created still changes, and the Convert screen says so on every conversion
  ("Held back from the dashboards — it was read by a template built here").
  To promote a user template to proven, ask whoever maintains the engine to move it
  into the built-in `templates\` set.

If a statement is genuinely a new kind the wizard can't handle, that's rare — see
[when-something-goes-wrong.md](when-something-goes-wrong.md) for how to report it
safely.

## Fixing a bank the tool already knows
Open the statement in the toolkit, correct what is wrong, and save. It saves as a
separate template (the shipped one is left alone so it can still be updated), and
records that it is a **correction of** that bank's template - which is what makes
your fix win from then on. Before this, a correction quietly lost to the template
it was correcting, because both recognise the same bank equally well.

It is still a template built here, so it converts for everyone but does not reach
the Qlik dashboards until whoever maintains the engine reviews it and promotes it.

## Stuck on the layout itself?
If you can't work out what the columns are, where the year comes from, or which
lines are totals rather than transactions, don't guess. Attach the statement to
Copilot and paste the prompt in
[survey-a-statement-with-ai.md](survey-a-statement-with-ai.md): it answers exactly
those questions, in the same order every time, and is written so that no client
detail ends up in the answer. The result fills in this wizard's fields directly —
and is safe to send on if you'd rather someone else built the template.
