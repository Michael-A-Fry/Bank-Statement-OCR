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
- Templates you save here are **user-created**. On the **Convert** tab they're hidden
  by default behind a tick-box ("Include user-created templates — not guaranteed
  tested"), so day-to-day conversions use the **proven** built-in set unless someone
  opts in.
- Only **proven** templates feed the Qlik dashboards (see
  [connecting-qlik.md](connecting-qlik.md)). To promote a user template to proven,
  ask whoever maintains the engine to move it into the built-in `templates\` set.

If a statement is genuinely a new kind the wizard can't handle, that's rare — see
[when-something-goes-wrong.md](when-something-goes-wrong.md) for how to report it
safely.
