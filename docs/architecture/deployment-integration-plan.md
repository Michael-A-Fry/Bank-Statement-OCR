# Deployment & integration plan — concurrency, authorisation, Qlik

**Status: PLAN ONLY.** Nothing here is built yet. This is the design we commit
to *before* writing a line of the server layer, so the choices are on record and
a single analyst can pick it up later. It respects the same three constraints as
the engine: **radical simplicity, maintainability by one analyst, usefulness to
forensic accountants.** Pure R, deterministic, no ML, no new language.

The engine (`convert_statement()`) is already the right shape for all of this:
it is a **pure function of a file** — it reads a path, never mutates shared
state, returns a self-contained result, and writes only to an output dir and an
append-only log. Everything below is a thin shell around that function. We are
not redesigning the engine; we are deciding how requests reach it and who is
allowed to make them.

---

## 1. Concurrency — many people, many statements, at once

### 1.1 What we are actually protecting against
Two forensic accountants (or Qlik, or a scheduled batch) convert statements at
the same moment. Three things could collide:

1. **CPU / memory** — OCR (`tesseract` + `pdftoppm`) is the only heavy step. Ten
   simultaneous 60-page scanned PDFs would saturate a small VM.
2. **The output directory** — two runs writing `outputs/statement.xlsx` at once.
3. **The append-only logs** — `runs.jsonl` and `feedback.jsonl` being written by
   two processes at the same instant.

The engine itself is **stateless and re-entrant** — it holds no globals, so two
runs in two R processes never interfere in memory. So concurrency is entirely a
question of (a) how many R processes we allow and (b) the three shared resources
above.

### 1.2 The model: a small pool of stateless workers behind a queue
Recommended for V1, in order of preference:

- **Preferred — [`plumber`] REST API, run as N worker processes.** One tiny
  `plumber.R` exposes `POST /convert` (multipart file upload) and returns the
  JSON result + download links. Run it under a process manager that forks a
  **fixed pool** (e.g. 4 workers) — `plumber`'s built-in `pr_run(async)` or a
  front nginx/`rserve`/`systemd` template. The fixed pool *is* the concurrency
  limit: request #5 waits for a worker instead of thrashing the box. This is the
  same "queue in front of a bounded worker pool" pattern Qlik ODAG already uses,
  so it will feel familiar operationally.
- **Simplest possible — a folder inbox + one scheduled R process.** Users (or
  Qlik) drop files into `inbox/`; a single `Rscript watch.R` on a 1-minute cron
  processes them **one at a time** into `outbox/`. Zero web server, zero
  concurrency bugs, trivially auditable — but no live response. Good fallback if
  the network team won't allow a long-running service. (This is effectively the
  Qlik ODAG "generate on demand" flow, minus the API.)

Both reuse `convert_statement()` unchanged. The difference is only the shell.

### 1.3 The three shared resources — concrete rules

**CPU/memory → bound the pool, and serialise OCR.**
- The worker-pool size is the global concurrency cap. Set it to
  `max(1, cores - 1)` and make it a single config value (`config/server.yaml`:
  `workers: 4`). One number, one place — an analyst can turn it down if the box
  struggles.
- OCR is the expensive tail. Optionally gate OCR behind a **single-slot
  semaphore** (a lockfile, see below) so at most one OCR job runs at a time even
  if several text-PDF/CSV jobs run in parallel. CSV/Excel/text-PDF stay fully
  parallel because they are cheap.

**Output directory → per-run subdirectory, never a shared filename.**
- Every run already gets a `run_id` (content hash + timestamp). Write outputs to
  `out/<run_id>/` instead of `out/`. Two runs can *never* target the same path,
  so the file-collision problem disappears by construction — no locking needed.
- A nightly cron prunes `out/` older than N days (config: `retain_days: 30`).

**Append-only logs → atomic single-line appends, upgrade path to a DB.**
- `log_event()` already opens the file in append mode and writes **one line**.
  On POSIX, a single `write()` of a line shorter than `PIPE_BUF` (4096 bytes on
  Linux) to a file opened `O_APPEND` is atomic — concurrent workers interleave
  whole lines, never corrupt one. Our log lines are well under 4 KB, so JSONL
  append is safe for the pool sizes we are talking about (single-digit workers).
- **If** we ever outgrow that (dozens of workers, or lines near 4 KB), the
  upgrade is a one-function change: point `log_event()` at SQLite
  (`RSQLite`, WAL mode handles concurrent writers) instead of a file. The rest
  of the system doesn't know or care. We note this rather than build it now —
  simplicity first.
- A tiny cross-platform lockfile helper (`flock` on Linux, or an atomic
  `dir.create()` lock — `dir.create` is atomic on all platforms) is the belt to
  the append-mode braces if we want to be paranoid, and is the mechanism for the
  OCR single-slot semaphore above.

### 1.4 Idempotency & de-duplication (free, because of `run_id`)
Because `run_id` starts with the file's content hash, the same file converted
twice is detectable. Optional: before converting, check the run log for a recent
successful run with the same `source_sha256` and offer the cached
`out/<run_id>/` instead of re-OCRing. Pure win for repeated Qlik pulls; skip for
V1 if it adds complexity.

### 1.5 What we explicitly are NOT doing
No Kubernetes, no Redis, no message broker, no multi-node cluster. A forensic
team is tens of statements a day, not thousands a second. One VM, a bounded R
worker pool, per-run output dirs, append-only logs. If load ever justifies more,
the stateless-worker design scales horizontally by running the same `plumber`
app on a second box behind the same inbox/share — but that is a someday problem,
not a V1 one.

---

## 2. Authorisation — who is allowed to convert

### 2.1 Requirement
All users must be in the Microsoft AD group **`RES_QLIKSENSE_PROD`**. This must
be **modifiable** (the group name will change; there may be more than one group,
checked as an OR). Authorisation may also be delegated to Qlik.

### 2.2 Principle: authorise at the gateway, verify in the engine shell
Two layers, defence in depth:

1. **Gateway (primary).** The service sits behind the corporate reverse proxy /
   IIS / Qlik, which already does **Integrated Windows Auth (Kerberos/NTLM)** or
   SAML. The proxy authenticates the user and passes their identity down (e.g. a
   `X-Forwarded-User` / `REMOTE_USER` header, or a signed Qlik ticket). The proxy
   is the front door; the R service is never exposed raw to the network.
2. **Engine shell (verification).** The `plumber` layer re-checks that the
   asserted user is in an allowed AD group before calling `convert_statement()`.
   Never trust a header blindly — verify group membership server-side.

### 2.3 The group check — modifiable, OR-of-groups, one config value
A single config file drives it (`config/auth.yaml`), so the analyst edits a list,
never code:

```yaml
# Any ONE of these groups grants access (OR). Add/rename freely.
allowed_ad_groups:
  - RES_QLIKSENSE_PROD
# - RES_FORENSIC_ANALYSTS      # example: add a second group later
auth_mode: ad          # ad | qlik | trusted_header | open(dev only)
fail_closed: true      # if the group check errors, DENY (never default-allow)
```

Planned R helper (design signature only — **not built**):

```r
# user_in_allowed_group(username, groups = auth_config$allowed_ad_groups) -> TRUE/FALSE
#   Returns TRUE iff `username` is a member of ANY group in `groups`.
#   fail_closed: on any lookup error, returns FALSE (deny), never TRUE.
```

Three interchangeable implementations behind that one signature (pick per site,
config-selected, no engine change):

- **LDAP bind (most portable).** Query AD over LDAP for the user's
  `memberOf` (with nested-group expansion via
  `LDAP_MATCHING_RULE_IN_CHAIN`). Pure-ish R via the `ldap`/`curl` route, or a
  one-line `ldapsearch` shell-out. No Windows dependency.
- **PowerShell / `whoami /groups` (Windows hosts).** Shell out to
  `whoami /groups` or `Get-ADGroupMember`, parse the SIDs/names. Simplest when
  the service runs on a domain-joined Windows box.
- **Qlik-delegated.** If Qlik already gates access to the app that calls us
  (section 3), then reaching us at all *is* the authorisation — we just log the
  Qlik-asserted user. Set `auth_mode: qlik` and the R check becomes "trust the
  signed Qlik ticket, record the user."

### 2.4 Where the user identity is already flowing
Every `convert_statement()` call takes `requested_by`. Today the Shiny app passes
`"shiny"`. Under this plan the gateway-asserted username is passed through as
`requested_by`, so **the run log and feedback log already attribute every action
to a real person** — no schema change needed. Authorisation and audit share one
field.

### 2.5 Failure behaviour
`fail_closed: true`. If the directory is unreachable or the lookup throws, access
is **denied**, and the run log records `status: "denied"` with the attempted
user and reason. We never fail open. This matches the engine's existing
fail-loud stance: an uncertain answer is a refusal with a reason, not a guess.

---

## 3. Qlik integration — submit in Qlik, get data back

### 3.1 The shape of it
Forensic accountants live in Qlik Sense. The goal: they upload/select a
statement in a Qlik app and receive the structured `transactions` table back in
Qlik, without leaving it. This is exactly the pattern Qlik **ODAG (On-Demand App
Generation)** and the retired **Inphinity Mole** flow provided — a Qlik-side
trigger that runs an external generator and loads its result. We are replacing
that generator with our R engine.

### 3.2 Two integration routes (both thin, both reuse the engine)

**Route A — folder / inbox handshake (simplest, ODAG-native).**
1. Qlik app writes the uploaded statement to a watched share (`inbox/`), plus a
   small sidecar `<file>.request.json` (`requested_by`, optional `bank` hint).
2. The R service (worker pool or cron watcher from §1.2) converts it and writes
   `out/<run_id>/statement.csv` + `.json` to an `outbox/` the Qlik load script
   reads.
3. Qlik's load script (`LOAD ... FROM [lib://outbox/<run_id>/statement.csv]`)
   pulls the structured data straight into the data model.
- Pros: no live API, no new ports, trivially auditable, matches how ODAG already
  moves files. Cons: not instant (poll/cron latency).

**Route B — REST from Qlik load script (live).**
1. The `plumber` `POST /convert` endpoint (from §1.2) accepts the file and
   returns the JSON result inline.
2. Qlik's load script calls it via the **REST Connector**
   (`WITH CONNECTION (BODY ...)`) and loads the returned JSON directly.
- Pros: immediate, no shared folder. Cons: needs the REST connector configured
  and the service reachable from the Qlik engine node.

Recommendation: **start with Route A** (it is the least new machinery and mirrors
ODAG), offer Route B where a team wants live conversion.

### 3.3 What comes back to Qlik
The engine's existing outputs are already Qlik-ready:
- `transactions` (CSV/JSON) → the fact table.
- `kpis` / reconciliation → a "trust & checks" sheet Qlik can surface as KPIs.
- `diagnostics` → a "why this needs review" table.
- `metadata` → statement-level dimensions (period, account, pages).
- `run_id` → the key that ties the Qlik reload back to our run log and any
  feedback. Feedback submitted in Qlik (a button that POSTs verdict + comment)
  flows into the *same* `feedback.jsonl` via `submit_feedback()`.

No new output format is needed for Qlik — it consumes what forensic accountants
already download.

### 3.4 Custom templates in Qlik — the limitation, stated plainly
**Template *creation* stays a team activity in the internal Shiny app; Qlik is
for *consumption*.** This is a deliberate boundary, not a gap:

- Building a template means **mapping columns, drawing PDF column boxes, and
  eyeballing a live preview** — an interactive, visual, judgement task. The Shiny
  Template/PDF wizards are built for exactly that. Qlik is a BI surface; it is
  the wrong tool to draw bounding boxes on a scanned PDF.
- Templates are a **shared, version-controlled-by-hand asset** (`templates/*.yaml`
  on the server). One analyst curates them so every conversion across the team is
  consistent — that consistency is one of the three core constraints. Letting
  every Qlik user invent ad-hoc templates would destroy it.
- **So in Qlik:** users *select* an existing bank template (or rely on
  auto-detection) and convert. If a statement matches nothing, they get the
  engine's normal `unsupported` diagnostic — "no template matched; ask the
  analyst to add one in the wizard" — with the closest match and missing columns
  named. The new-template request goes to the analyst, who builds it once in
  Shiny, and from then on every Qlik user gets it for free.
- Net limitation to document for users: **Qlik = pick a template + convert +
  give feedback. Adding a new bank = the analyst, in the wizard, once.** That
  split keeps the powerful/visual work in the right tool and the templates
  consistent for everyone.

---

## 4. Build order when this becomes real (not now)
1. Wrap `convert_statement()` in `plumber.R` (`POST /convert`) — ~40 lines.
2. Add `config/server.yaml` (worker count, retention) + per-run output dirs.
3. Add `config/auth.yaml` + `user_in_allowed_group()` (LDAP first).
4. Point the gateway/proxy at it; pass `REMOTE_USER` → `requested_by`.
5. Wire Qlik Route A (inbox/outbox) + a feedback button.
6. Only if load demands it: move logs to SQLite; add Route B.

Every step is additive and leaves the engine untouched. That is the whole point:
the hard part (deterministic, auditable extraction) is done and frozen; the
server, auth, and Qlik layers are thin, boring, and swappable.
