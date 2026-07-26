# Operational guide — how to do things

Everything the data team may need to **do** with Statement Studio, one task per
page. Written for data analysts, not software developers: clear steps, no code.

Background — what it is, how it was built, the audit and research — lives in
[`../context/`](../context/README.md).

## Running the server
| I want to… | Page |
|---|---|
| Set it up for the first time (air-gapped) | [first-time-setup.md](first-time-setup.md) |
| Update to a new version | [updating.md](updating.md) |
| Start it / keep it running after reboots / open the firewall port / change settings | [running-and-keeping-it-up.md](running-and-keeping-it-up.md) |
| Back up the irreplaceable folders, and restore them | [backup-and-restore.md](backup-and-restore.md) |

## Using it day to day
| I want to… | Page |
|---|---|
| Convert a statement and download the result | [converting-statements.md](converting-statements.md) |
| Teach it a new bank layout (no code) | [adding-a-bank-template.md](adding-a-bank-template.md) |
| Describe a tricky statement's layout, with no client information in it | [survey-a-statement-with-ai.md](survey-a-statement-with-ai.md) |
| Do admin: password, review queue, tidy logs | [admin-and-maintenance.md](admin-and-maintenance.md) |
| Feed the Qlik dashboards | [connecting-qlik.md](connecting-qlik.md) |
| Work out what to do when something looks wrong | [when-something-goes-wrong.md](when-something-goes-wrong.md) |

## Owning it (the maintainer)
| I want to… | Page |
|---|---|
| Run the test suite on the server, re-apply an `R\params.R` change, promote a user template to proven | [maintaining-the-engine.md](maintaining-the-engine.md) |
| Know what this tool must always do, and must never do | [../context/charter.md](../context/charter.md) — one page, the fixed points |
| See the whole thing on one page — the journey, which module owns each step, and where each kind of change goes | [../context/how-it-fits-together.md](../context/how-it-fits-together.md) |

## Where do I change…?
The tool is designed so the things that change live in **data and config, not code**.
One place for each:

| I want to change… | Where it lives | Who / how |
|---|---|---|
| A bank's layout (its columns, date & amount style) | `templates/<bank>.yaml` | analyst — **Add a template** in the app (no code) |
| A wording the tool recognises (another phrase for "closing balance", etc.) | `dictionaries/labels.yaml` | admin — **Admin → Templates → Label dictionary** ("Teach it this wording") |
| A recognition marker or pattern (a debit/credit marker word, a money/date regex) | `dictionaries/lexicon.yaml` | admin — **Admin → Data capture → Words the tool knows to look for** |
| A deployment setting (port, admin password, the Qlik feed gate, file paths) | `config/config.yaml` | admin — copy from `config.example.yaml`, edit text |
| A numeric engine threshold (year window, OCR DPI, row tolerance, redaction darkness) | `R/params.R` | maintainer — one file, every tuning knob → [engine-parameters.md](../context/engine-parameters.md) |
| A shipped built-in default (rare) | the relevant `R/*.R` | maintainer → [customisation.md](../context/customisation.md) |

The first three need **no code** and are done in the running app; the rest are one
clearly-named file each. Full detail: [customisation.md](../context/customisation.md).

## The short version
1. **Set up once** — build the offline package on an internet PC, copy one folder
   to the server, double-click `RUN-ME.bat`. It installs everything and starts the
   app. ([first-time-setup.md](first-time-setup.md))
2. **Use it** — open the app in a browser, upload a statement, click **Convert**,
   download the Excel/CSV/JSON. ([converting-statements.md](converting-statements.md))
3. **Grow it** — when a new bank shows up, teach it the layout in the built-in
   wizard. ([adding-a-bank-template.md](adding-a-bank-template.md))
