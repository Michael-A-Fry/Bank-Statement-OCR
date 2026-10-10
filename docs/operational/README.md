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
| Convert a bank or layout the tool has not seen, and train a bank on its statements | [adding-a-bank-template.md](adding-a-bank-template.md) |
| Work out what to do when something looks wrong | [when-something-goes-wrong.md](when-something-goes-wrong.md) |
| Describe a tricky layout with no client information in it | [survey-a-statement-with-ai.md](survey-a-statement-with-ai.md) |

## Improving the screens

| I want to… | Page |
|---|---|
| Watch one accountant convert a real case unaided, and note where the screen made them stop | [five-minute-usability-test.md](five-minute-usability-test.md) |

## Running the server

| I want to… | Page |
|---|---|
| Set it up for the first time (air-gapped) | [first-time-setup.md](first-time-setup.md) |
| **Put it on the Qlik server: service account, boot start, firewall, the address** | [deploy-on-the-qlik-server.md](deploy-on-the-qlik-server.md) |
| **Turn it on for real — the checklist for the day** | [go-live-checklist.md](go-live-checklist.md) |
| **Let the whole team in, and have the audit trail name the right person** | [who-is-using-it.md](who-is-using-it.md) |
| Start it, keep it running after reboots, open the firewall port, change a setting | [running-and-keeping-it-up.md](running-and-keeping-it-up.md) |
| **Ask the box whether it is fit to convert — one command, after every update** | `scripts\health-check.R`, in [maintaining-the-engine.md](maintaining-the-engine.md) §1 |
| Back up the irreplaceable folders, and restore them | [backup-and-restore.md](backup-and-restore.md) |
| Update to a new version (build a package, replace the folder) | [updating.md](updating.md) |
| **Go from 2.x to 3.0.0 (recipes): what changes, new settings, rolling back, side by side with the QVF** | [updating-2.x-to-3.0.md](updating-2.x-to-3.0.md) |
| **Go from 1.23.1 to 2.0.0: every file to update, add or delete** | [release-2.0.0-hand-carry.md](release-2.0.0-hand-carry.md) |
| **Merge a dev folder into the live one by hand — and what must never be copied** | [updating-a-version.md](updating-a-version.md) |
| **Put a bad version back, and deal with what it already sent to Qlik** | [rolling-back.md](rolling-back.md) |
| Do admin: recipes (on/off, change, merge, accept drafts), automatic-reading counts, spot checks, dictionaries, tidy logs | [admin-and-maintenance.md](admin-and-maintenance.md) |
| Feed the Qlik dashboards | [connecting-qlik.md](connecting-qlik.md) |

## Owning it (the maintainer)

| I want to… | Page |
|---|---|
| Run the test suite on the server, re-apply an `R\params.R` change, add a statement to the test suite | [maintaining-the-engine.md](maintaining-the-engine.md) |
| Go back to a conversion somebody says is wrong, and reproduce the figure | [investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md) |
| Know what this tool must always do, and must never do | [../context/charter.md](../context/charter.md) — one page, the fixed points |
| See the whole thing at once: the journey, which module owns each step, where each kind of change goes | [../context/how-it-fits-together.md](../context/how-it-fits-together.md) |
| Read the whole design in one sitting, then ship a change to it | [../design.md](../design.md) |

## Where do I change…?

The things that change live in **data and config, not code**.

| To change | Where | Who, and how |
|---|---|---|
| How a bank's statements are read | its **recipe**: `recipes\` (shipped) and `templates\recipes\` (this server's drafts and changes; never hand-edited) | nobody needs to: a person's check on a new design writes it. A person fixes one reading on **Please check**; an admin turns recipes on and off, changes and merges them on **Admin → Recipes**, and accepts drafts on **Admin → Needs attention**. No code. |
| What a fact about the statement is called (another phrase for "closing balance") | `dictionaries\labels.yaml` | admin — **Admin → Words**, or Please check |
| What words inside the transaction table mean (a DR / CR mark, a column heading), or a money or date shape | `dictionaries\lexicon.yaml` | admin — **Admin → Words**, or Please check |
| How often automatic conversions are spot-checked | `config\config.yaml` → `auto_reading: spot_check_rate` | admin — **Admin → Automatic reading** |
| A deployment setting (port, admin password, the Qlik feed gate, paths) | `config\config.yaml` | admin — annotated example in `config\config.example.yaml` |
| A numeric engine threshold (year window, OCR DPI, row tolerance, seconds per page) | `R\params.R` | maintainer — [../context/engine-parameters.md](../context/engine-parameters.md) |

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
4. **Grow it** — when a new bank turns up, convert its statements; to teach it a
   whole pile at once, train the bank on Admin → Health → Train a bank.
   ([adding-a-bank-template.md](adding-a-bank-template.md))
