# Qlik integration - options analysis & decision record (archive)

**This is a reference/archive doc.** It records the Qlik-side options that were
weighed and *why we chose the one we did*, so the reasoning isn't lost. The live
architecture is in [`qlik-sense-integration.md`](qlik-sense-integration.md); the dev
setup in [`qlik-dev-setup.md`](qlik-dev-setup.md). Nothing here is a build target.

---

## The decision (what we're doing)

**Qlik opens the app; Qlik does the analytics.**
- Accountants convert in the **Shiny app** (reached from a **tile/link in Qlik**):
  upload -> pick an audited template -> convert -> download. Isolated per user.
- Each conversion writes to a **feed** folder (gated to reconciled + proven
  templates); a **Qlik folder connection + scheduled reload** turns it into
  dashboards / casework.

**Rejected:** making Qlik the *interactive converter* (Inphinity Forms upload + ODAG
+ EXECUTE/Rserve). Kept here for the record only.

---

## The reframe that settled it

**You cannot process a statement "in Qlik."** Qlik has no PDF parser and no OCR - it
physically cannot read a bank statement. The legacy tool didn't either: it paid for
the **Mole** connector to extract *outside* Qlik, then loaded the result. So "in
Qlik" can only ever mean *the upload/download screens are Qlik* - the computation is
always external (Mole before, R now). That removes the false premise behind "can we
just do it in Qlik".

---

## The options that were weighed

### A. Interactive convert inside Qlik (the legacy shape) - REJECTED
Inphinity Forms `type=upload` -> a file lands on a share -> R converts it via one of:
- **EXECUTE Rscript** in an ODAG reload (needs the `Allow Execute` security
  override many admins refuse),
- **Rserve / SSE** analytic connection (sanctioned, but built for column data not
  file->table, and **Rserve on Windows serialises** - a hard concurrency bottleneck),
- an **async poller** (R converts independently; ODAG just loads the ready CSV).
Then ODAG generates the result app; the user exports/downloads.

**Straight assessment:**
- *Consistency:* fine - same deterministic engine as any path.
- *Efficiency:* poor - every conversion is a fresh Rscript process / a full reload /
  an ODAG app generated then cleaned up. Tolerable at low volume, churns at scale.
- *Reliability:* the weak point. A long fragile chain (Inphinity licence + ODAG
  version-sensitive bindings + EXECUTE override *or* an Rserve daemon) and a
  **concurrency ceiling inside Qlik** (bounded concurrent reloads/ODAG requests;
  serialised Rserve on Windows). A lone analyst cannot own it; Qlik upgrades can
  break ODAG/Inphinity/SSE.
- *Verdict:* it *can* be done (it's the status quo), and it *works* at low volume -
  but it buys nothing technical (R still does 100% of the work) and costs the
  maintainability we want. Only justified if "the screen must be a Qlik sheet" is a
  hard, non-negotiable requirement.

### B. Shiny converts, Qlik loads the feed - CHOSEN
Accountants use the purpose-built Shiny converter (reached from Qlik); results flow
to a `feed/` folder Qlik loads for dashboards.
- *Consistency/Efficiency/Reliability:* all strong - a warm converter, per-file feed
  writes (no shared append), no ODAG/Inphinity/EXECUTE/Rserve to keep alive.
- *Maintainability:* a lone analyst runs "a folder + a reload schedule + a link".
- *Governance:* preserved by the **feed gate** (only reconciled, proven-template
  results reach the dashboards), not by crippling the converter.

### C. Hybrid (link from Qlik to Shiny) - FOLDED INTO B
A Qlik tile opens the Shiny app (same AD group). This is how B keeps users
"starting in Qlik" without the ODAG tax. Adopted as part of B.

---

## Concurrency note (why B wins cleanly)

Our engine + folder model scales fine (independent, content-hash-keyed files). The
in-Qlik path (A) moves the ceiling *into Qlik*: bounded concurrent reloads, ODAG
request caps, and single-threaded Rserve on Windows all serialise simultaneous
users. B has no such ceiling - N conversions = N independent file sets.

---

## What this means for tooling

- **Rserve / EXECUTE:** not needed. (Rserve was briefly bundled offline as insurance
  for A; removed once A was rejected.)
- The proven-only Qlik conversion entrypoint + async poller that were built for A
  were removed; the governance they provided lives in the **feed gate** instead.
