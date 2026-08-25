# For analysts — your five pages

You were handed this folder. Everything you need is one click away, in the order
you will want it.

| I want to… | Page |
|---|---|
| Convert a statement and download the result | [converting-statements.md](../operational/converting-statements.md) |
| Teach it a new bank layout, no code | [adding-a-bank-template.md](../operational/adding-a-bank-template.md) |
| Pull tables and figures out of anything that is **not** a statement | [pulling-tables-out-of-a-report.md](../operational/pulling-tables-out-of-a-report.md) |
| Work out what to do when something looks wrong | [when-something-goes-wrong.md](../operational/when-something-goes-wrong.md) |
| Describe a tricky layout to someone, with no client information in it | [survey-a-statement-with-ai.md](../operational/survey-a-statement-with-ai.md) |

Start with **converting-statements.md**. The other four are for when something
happens: a layout the tool has not seen, a document that is not a statement at
all, a verdict you did not expect, or a statement you need help with but may not
send.

## Why the pages are one folder over

They live in [`../operational/`](../operational/README.md), which also holds the
pages for whoever runs the server — first-time setup, the firewall port, backups,
updates, admin, Qlik. None of those are yours. This page exists so the folder
name you were given lands you on your own pages instead of on that list.

Nothing else in `docs/` is written for you. `docs/context/` is for whoever owns
and changes the engine, and [`../design.md`](../design.md) is for a developer
inheriting it.

## The two things worth knowing before you start

- **The tool never returns a silent wrong answer.** A run that is not clean says
  where, why, how bad and what to do about it. If it cannot prove something, it
  says it could not prove it — that is not the same as a problem, and
  [when-something-goes-wrong.md](../operational/when-something-goes-wrong.md)
  is how to tell the two apart.
- **You are never asked a question the tool can answer, and never one you
  cannot.** If a screen asks you something, it is because only you know it.
