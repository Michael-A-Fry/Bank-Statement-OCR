# Operational guide — how to do things

One page per task, written for analysts rather than developers. Background — how
it works and why — is in [`../context/`](../context/README.md).

**Converting statements, not running the server?** Only the four *Every day*
pages below are yours. [`../for-analysts/`](../for-analysts/README.md) is the
short index that lists just those four, and it is the folder name to hand to
somebody new — everything from *Running the server* down is for whoever owns the
box.

## Every day

| I want to… | Page |
|---|---|
| Convert a statement and download the result | [converting-statements.md](converting-statements.md) |
| Teach it a new bank layout, no code | [adding-a-bank-template.md](adding-a-bank-template.md) |
| Pull tables and figures out of anything that is **not** a statement | [pulling-tables-out-of-a-report.md](pulling-tables-out-of-a-report.md) |
| Work out what to do when something looks wrong | [when-something-goes-wrong.md](when-something-goes-wrong.md) |
| Describe a tricky layout with no client information in it | [survey-a-statement-with-ai.md](survey-a-statement-with-ai.md) |

## Running the server

| I want to… | Page |
|---|---|
| Set it up for the first time (air-gapped) | [first-time-setup.md](first-time-setup.md) |
| **Put it on the Qlik server: service account, boot start, firewall, the address** | [deploy-on-the-qlik-server.md](deploy-on-the-qlik-server.md) |
| **Turn it on for real — the checklist for the day** | [go-live-checklist.md](go-live-checklist.md) |
| Start it, keep it running after reboots, open the firewall port, change a setting | [running-and-keeping-it-up.md](running-and-keeping-it-up.md) |
| Back up the four irreplaceable folders, and restore them | [backup-and-restore.md](backup-and-restore.md) |
| Update to a new version (build a package, replace the folder) | [updating.md](updating.md) |
| **Merge a dev folder into the live one by hand — and what must never be copied** | [updating-a-version.md](updating-a-version.md) |
| **Put a bad version back, and deal with what it already sent to Qlik** | [rolling-back.md](rolling-back.md) |
| Do admin: password, requests, dictionaries, tidy logs | [admin-and-maintenance.md](admin-and-maintenance.md) |
| Feed the Qlik dashboards | [connecting-qlik.md](connecting-qlik.md) |

## Owning it (the maintainer)

| I want to… | Page |
|---|---|
| Run the test suite on the server, re-apply an `R\params.R` change, promote a template to proven | [maintaining-the-engine.md](maintaining-the-engine.md) |
| Go back to a conversion somebody says is wrong, and reproduce the figure | [investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md) |
| Know what this tool must always do, and must never do | [../context/charter.md](../context/charter.md) — one page, the fixed points |
| See the whole thing at once: the journey, which module owns each step, where each kind of change goes | [../context/how-it-fits-together.md](../context/how-it-fits-together.md) |
| Read the whole design in one sitting, then ship a change to it | [../design.md](../design.md) |

## Where do I change…?

The things that change live in **data and config, not code**.

| To change | Where | Who, and how |
|---|---|---|
| A bank's layout — its columns, date and amount style | `templates\<bank>.yaml` | analyst, in the app — **Add a template**. No code. |
| A wording the tool recognises (another phrase for "closing balance") | `dictionaries\labels.yaml` | admin — **Admin → Templates → Label dictionary** |
| A recognition marker or pattern (a debit/credit marker word, a money or date shape) | `dictionaries\lexicon.yaml` | admin — **Admin → Data capture → Words the tool knows to look for** |
| A deployment setting (port, admin password, the Qlik feed gate, paths) | `config\config.yaml` | admin — annotated example in `config\config.example.yaml` |
| A numeric engine threshold (year window, OCR DPI, row tolerance, redaction darkness) | `R\params.R` | maintainer — [../context/engine-parameters.md](../context/engine-parameters.md) |

The first three need no code and are done in the running app. Which tier to use,
and why, is in [admin-and-maintenance.md](admin-and-maintenance.md).

## The short version

1. **Set up once** — build the offline package on an internet PC, copy one folder
   to the server, double-click `RUN-ME.bat`, set the admin password, open the
   firewall port. ([first-time-setup.md](first-time-setup.md))
2. **Turn it on** — the numbered checklist for go-live day, every step with
   something you can see that says it worked.
   ([go-live-checklist.md](go-live-checklist.md))
3. **Use it** — upload a statement, click Convert, download the Excel/CSV/JSON.
   ([converting-statements.md](converting-statements.md))
4. **Grow it** — when a new bank turns up, teach it the layout in the toolkit.
   ([adding-a-bank-template.md](adding-a-bank-template.md))
