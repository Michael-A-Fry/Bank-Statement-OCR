# For analysts — your four pages

You were handed this folder. Everything you need is one click away, in the order
you will want it.

| I want to… | Page |
|---|---|
| Convert a statement and download the result | [converting-statements.md](../operational/converting-statements.md) |
| Convert a statement from a bank or layout the tool has not seen | [adding-a-bank-template.md](../operational/adding-a-bank-template.md) |
| Work out what to do when something looks wrong | [when-something-goes-wrong.md](../operational/when-something-goes-wrong.md) |
| Describe a tricky layout to someone, with no client information in it | [survey-a-statement-with-ai.md](../operational/survey-a-statement-with-ai.md) |

Start with **converting-statements.md**. The other three are for when something
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

- **The tool never returns a silent wrong answer.** It converts a statement on
  its own only when the statement's own arithmetic proves the reading. Anything
  else comes to you on **Please check**, with the reason. If it cannot prove
  something, it says it could not prove it — that is not the same as a problem, and
  [when-something-goes-wrong.md](../operational/when-something-goes-wrong.md)
  is how to tell the two apart.
- **You are never asked a question the tool can answer, and never one you
  cannot.** If a screen asks you something, it is because only you know it.
