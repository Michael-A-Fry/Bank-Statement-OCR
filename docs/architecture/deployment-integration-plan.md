# Deployment, authorisation & concurrency — the dead-simple plan

**Status: PLAN + the concurrency piece is already built.** This is deliberately
the *stupidly simple* version. No web server, no API, no database, no login
screen, no LDAP code, no service account, no scripts to babysit. A data analyst
who is **not an engineer** maintains it by editing a folder and, occasionally,
asking IT to change one AD group. That is the whole thing.

The rule we held to: **if a step needs an engineer, it's wrong. Redesign it
until it doesn't.**

---

## The one-paragraph version

Put the tool in a folder on a Windows file share. Ask IT to give that folder's
permissions to the AD group **`RES_QLIKSENSE_PROD`**. Done. If you're in the
group, Windows lets you open the folder and run the tool; if you're not, Windows
stops you before a single line of our code runs. Everyone runs the same tool off
the same share at the same time, and because **every conversion writes its own
little file** (its outputs and its one-line log), nobody ever steps on anyone
else. To change who has access, IT changes the AD group. To see who did what,
open the `logs/runs/` folder — each conversion left a file stamped with the
Windows username.

That's it. The rest of this document just explains *why* each choice is the
simple one, and lists the complicated things we deliberately did **not** build.

---

## 1. Authorisation = a folder permission. There is no auth code.

### How it works
1. The tool lives in one folder on a share, e.g. `\\fileserver\BankStatements\`.
2. IT opens that folder's **Properties → Security**, and grants access to the AD
   group **`RES_QLIKSENSE_PROD`**. (Point-and-click, the thing IT does every day.)
3. Windows now enforces it: a person in the group can open and run the tool; a
   person who isn't gets "access denied" from Windows itself — our code is never
   even reached.

### Why this is the right answer
- **Nothing to code, nothing to inject.** We do not read a username and password,
  we do not build a query, we do not call an API. There is no login form to
  attack and no string that reaches a database or a shell. The attack surface for
  "someone bypasses auth" is *the Windows file system* — the most battle-tested
  permission system in the building.
- **You already trust it.** This is exactly how the Qlik/finance shares are
  already secured. We are not inventing a new security model; we are reusing the
  one the org already runs and audits.
- **Audit is free.** Windows can already log share access, and every conversion
  additionally drops a run-log file stamped with `%USERNAME%` (see §3). "Who
  converted what, when" is answerable without us building anything.

### Modifiable, and OR-of-groups — still zero code
You said the group must be changeable and there might be more than one (an OR).
Both are free here: **add another AD group to the folder's Security tab.** Being
in *any* of the listed groups grants access — that is literally how NTFS
permissions combine. Renaming `RES_QLIKSENSE_PROD` later? IT renames the group;
the tool doesn't know or care.

### Optional, ONLY if you want a friendly message instead of a Windows error
Strictly not required — the folder permission is the real gate. But if you'd
like the app to greet an unauthorised person with *"You're not in the
BankStatements group, ask IT"* rather than a raw Windows denial, drop a plain
text file `config/allowed_groups.txt` (one AD group per line) and the app can do
a 3-line check using the built-in Windows command `whoami /groups`. It passes no
user input to anything — it just asks Windows "what groups am I in?" and compares
to the list. This is a nicety, not a security boundary, and it ships **off**.
```
# config/allowed_groups.txt  (optional; edit this list, no code)
RES_QLIKSENSE_PROD
RES_FORENSIC_ANALYSTS
```

---

## 2. Concurrency = one file per conversion. No locks, no server. (BUILT)

### The whole mechanism
The engine is a **pure function of a file**: it reads a statement, holds no
shared state, and writes only into its own places. Concurrency is handled by
making sure two runs never write the *same* place:

- **Outputs:** each conversion writes to `out/<run_id>/…` (a folder named by the
  run's own id). Two people converting at once produce two different folders.
- **Run log:** each conversion writes **one file**, `logs/runs/<run_id>.json`.
  Never an append to a shared file. Two people at once = two different files.
- **Feedback:** each submission writes one file, `logs/feedback/<id>.json`.

Because nobody ever appends to a shared file, there is **nothing to lock, nothing
to corrupt, and nothing to configure.** Ten people, a hundred conversions —
each is a separate little file. To read the log you list the folder; to read one
record you open a `.json` in Notepad.

> Why this matters specifically on a share: appending to one shared log file from
> several PCs over a network share (SMB) is the one operation that genuinely can
> interleave and corrupt. One-file-per-run sidesteps it completely. This is why
> the logging was rewritten to per-file — the plan and the code agree.

### How the tool actually runs (pick the simpler one for your site)
- **Simplest: a desktop shortcut per analyst.** A `Run-BankStatements.bat` on the
  share launches the Shiny app locally (`R -e "shiny::runApp('.')"`), pointing at
  the shared `templates/`, `dictionaries/`, and `logs/`. Everyone runs their own
  copy; the only shared things are those folders, and per-file writes keep them
  safe. Zero server to keep alive.
- **If you prefer one address for everyone: Shiny Server (open-source).** One
  install, everyone browses to it. Shiny already isolates each person's session.
  Still no API, still per-file logs. This is one piece of infrastructure and the
  only "install" in the whole plan — use it only if a shared URL is worth it.

Either way the engine, templates, dictionary, and logs are identical.

### Heavy jobs (OCR) don't need managing either
The only slow step is OCR of scanned PDFs. If the box ever feels busy, the
single knob is *"how many at once"* — and with the desktop-shortcut model that's
just "how many people clicked convert," which self-limits in practice. No queue
to build. If you outgrow that, you host the Shiny Server version on a slightly
bigger box. That's the entire scaling story.

---

## 3. Who-did-what = the Windows username, captured automatically

Every run-log and feedback file records `requested_by`. The tool fills it with
the **Windows logged-in username** (`%USERNAME%`) automatically — no login
prompt, because Windows already authenticated the person at sign-in and only
group members can run the tool at all. It is stored as a plain string and never
used in a query or evaluated, so it carries **no injection or audit risk**. If a
caller passes an explicit name (e.g. Qlik passing the Qlik user), that is used
instead.

---

## 4. Qlik — the same folder idea, nothing new

Keep it identical in spirit: **folders, not an API.**
1. The Qlik app writes the uploaded statement into an `inbox/` folder on the same
   share (which is already permissioned to the group).
2. The tool converts it and writes the result to `out/<run_id>/statement.csv`.
3. The Qlik load script reads that CSV straight into its data model
   (`LOAD … FROM [lib://out/<run_id>/statement.csv]`).

No REST connector, no service, no ports. This is the same pattern Qlik ODAG
already uses to move files, so it will feel familiar. Feedback from Qlik, if you
want it, writes a `logs/feedback/` file exactly like the app does.

**Template creation stays in the Shiny wizards (a team activity); Qlik is for
selecting a template + converting + optional feedback.** Adding a new bank =
the analyst, in the wizard, once — then everyone (including Qlik) gets it. This
keeps every conversion consistent, which is one of the three core constraints.

---

## 5. What a non-engineer actually maintains

The complete maintenance manual is four bullets:
- **Add/curate a bank template** → the Shiny Template/PDF wizard (point-and-click).
- **Teach a new wording** ("balance b/f") → add one line to
  `dictionaries/labels.yaml`.
- **Change who has access** → ask IT to change the AD group on the folder.
- **See usage / errors / feedback** → open the `logs/` folders; each file is a
  readable `.json`.

No servers to restart, no database to back up (the logs *are* the files), no
certificates, no code.

---

## 6. Things we deliberately did NOT build (and why)

Each of these is the "enterprise" answer and each one adds a moving part a lone
analyst would eventually have to understand. Rejected on purpose:

| Tempting thing | Why we said no |
|---|---|
| A REST API (plumber) | Needs a running service, ports, and a process manager. A shared folder needs none of that. |
| LDAP / PowerShell AD lookups in code | The folder permission already does the check, in the OS, with no code to get wrong. |
| A database for logs (SQLite/SQL Server) | One file per run needs no schema, no driver, no backup job. The folder *is* the database. |
| A login screen / tokens / service account | Windows already logged the person in; reusing that is both simpler and safer. |
| A job queue / worker pool | Per-file writes + per-user desktop launch self-limit. Build a queue only if load ever proves it — it hasn't. |

If load or requirements ever genuinely outgrow this, the upgrade path is
additive (host the Shiny Server version; if logs ever get huge, a nightly script
can roll `logs/runs/*.json` into one archive). None of it is needed now, and
none of it changes the engine.
