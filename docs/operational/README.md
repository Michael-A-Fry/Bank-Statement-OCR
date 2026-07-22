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
| Start it / keep it running after reboots / change settings | [running-and-keeping-it-up.md](running-and-keeping-it-up.md) |

## Using it day to day
| I want to… | Page |
|---|---|
| Convert a statement and download the result | [converting-statements.md](converting-statements.md) |
| Teach it a new bank layout (no code) | [adding-a-bank-template.md](adding-a-bank-template.md) |
| Do admin: password, review queue, tidy logs | [admin-and-maintenance.md](admin-and-maintenance.md) |
| Feed the Qlik dashboards | [connecting-qlik.md](connecting-qlik.md) |
| Work out what to do when something looks wrong | [when-something-goes-wrong.md](when-something-goes-wrong.md) |

## The short version
1. **Set up once** — build the offline package on an internet PC, copy one folder
   to the server, double-click `RUN-ME.bat`. It installs everything and starts the
   app. ([first-time-setup.md](first-time-setup.md))
2. **Use it** — open the app in a browser, upload a statement, click **Convert**,
   download the Excel/CSV/JSON. ([converting-statements.md](converting-statements.md))
3. **Grow it** — when a new bank shows up, teach it the layout in the built-in
   wizard. ([adding-a-bank-template.md](adding-a-bank-template.md))
