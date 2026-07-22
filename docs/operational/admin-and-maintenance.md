# Admin and maintenance

The **Admin** tab (password-protected) is where you keep an eye on the tool and tidy
up. Open it with the password from `config\config.yaml` (`app.admin_password` — set
it before sharing the app; see [running-and-keeping-it-up.md](running-and-keeping-it-up.md)).

Everything here is point-and-click. There's no database to manage — the tool writes
one small file per conversion and reads them back.

---

## What you can do in Admin
| Task | What it's for |
|---|---|
| **Insights** | See conversions over time, trust levels, and what's failing or drifting — read from the run and feedback logs. |
| **Review queue** | When a user hits a statement the tool can't handle and clicks "raise to us", it lands here for you to look at. |
| **Template management** | See every template, hide one without deleting it, and merge near-duplicate variants. |
| **Label dictionary** | Manage the wordings for labelled values ("opening balance" vs "balance brought forward"…) as a plain list of phrases. |
| **Batch audit + folder intake** | Point the tool at a folder of statements and get a coverage report — what converts cleanly, what needs a template. |
| **Processed / failed browser** | Browse what's been processed and what failed, with the reason. |
| **Tidy up logs** | Archive old run logs so the folder stays small. Nothing is deleted — old runs move to `logs\archive\`. |

---

## Feedback closes the loop
On every conversion, anyone can rate the result (correct / minor issues / wrong) with
an optional comment. Those ratings show up in **Insights**, tagged when a run wasn't
clean, so you can see exactly what the tool got wrong and decide whether it's a
template tweak or something to escalate — see
[when-something-goes-wrong.md](when-something-goes-wrong.md).

---

## Routine upkeep
- **Watch for drift:** if Insights starts showing more failures or low-trust runs for
  a bank, its export format probably changed — open the wizard and adjust the
  template ([adding-a-bank-template.md](adding-a-bank-template.md)).
- **Keep logs tidy:** click **Tidy up logs** in Admin every so often.
- **Clear the review queue:** work through raised statements — most are a template
  tweak you can do yourself.

That's the whole maintenance story: mostly templates and a glance at Insights, done
by one analyst, no engineer.
