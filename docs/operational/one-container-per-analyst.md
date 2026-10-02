# One container per analyst — isolation the operating system enforces

For whoever runs the server. This is the step that turns *"the app's own checks are
correct"* into *"the file system will not permit it"*.

Read [who-is-using-it.md](who-is-using-it.md) first — it is the cheaper half, it
does not need any of this, and it must be done either way.

---

## What problem this actually solves

`shiny::runApp()` runs **one R process for the whole app**. Today that means:

- every analyst shares one memory space, one `tempdir()`, and one set of globals;
- every uploaded statement is owned by the **same service account**, so NTFS cannot
  tell one investigator from another — neither of them is the process identity;
- so isolation lives entirely in the app's own checks, which is code you must keep
  correct on every change, not a boundary anything enforces.

ShinyProxy starts **a container per session**. Globals cannot leak because there is
no shared process, and each container mounts only its own volume, so a bug in the
app's checks stops being a cross-case evidence leak.

It also fixes something smaller and more visible: R is single-threaded, so one
analyst's 300-page scan competes with everyone else's work. `R/jobs.R` already
pushes conversions into child processes for exactly this reason (measured: one
analyst's scan froze another's browser for 65 seconds before that change). With a
container each, the contention moves to the host scheduler where it belongs.

## What it does **not** solve

**It is not a substitute for case ownership.** Each analyst can only see their own
uploads — which is most of what you asked for — but there is still no notion of a
*case* with an owner and a grant list, so a supervisor handover or a second
investigator on the same matter has no mechanism except "upload it again". That
remains unbuilt.

---

## The licensing trap, before you plan anything

**Docker Desktop requires a paid subscription for government entities,
unconditionally** — the employee-count and revenue thresholds that exempt small
businesses do not apply. NZ Police is a government entity.

**Docker Engine on Linux is free.** So the no-cost path is a Linux guest, not
Docker Desktop on Windows:

```
Windows Server (your existing box, Qlik on it)
 └─ IIS  ── Windows Integrated Auth, TLS, loopback-only to the guest
     └─ Hyper-V Linux guest
         └─ Docker Engine (free)
             └─ ShinyProxy (Apache-2.0, free)
                 └─ one statement-studio container per analyst
```

Two things that are **not** options, so nobody spends a week on them:

- **Shiny Server does not run on Windows** — open source *or* Pro. Linux only.
- **Shiny Server Pro support ended in March 2026.** Do not build on it.

And one that costs money but needs no Linux skills: **Posit Connect**. If you go
that way, note that Connect shares one R process between **up to 20 users** by
default — you must turn `RunAsCurrentUser` on, or you have paid for a licence and
got none of the isolation.

---

## The three files

| File | What it is |
|---|---|
| `deploy/Dockerfile` | the image: R 4.3.3 pinned, the four poppler binaries, tesseract, the eleven R packages the app actually loads |
| `deploy/shinyproxy-application.yml` | the ShinyProxy config: per-user volumes, LDAP, limits |
| this page | the order to do it in |

**Neither file was built or run in the session that wrote them** — there was no
container runtime available. The Dockerfile's package list *is* measured
(`loadedNamespaces()` after a real CSV conversion and a real PDF conversion, minus
base packages), and the build commands at the foot of it are how you prove the rest.
The ShinyProxy key names vary between major versions; check each against the
configuration reference for the version you install.

---

## Order of work

### 1. Build the image, and make it prove itself

```
docker build -f deploy/Dockerfile -t statement-studio:1.9.0 .
docker run --rm statement-studio:1.9.0 Rscript scripts/health-check.R
```

The health check is the gate. It answers seven questions, and two of them matter
especially here:

- **Signs** — are `pdftocairo` and `pdftotext` present? Without them the reader
  **fails quiet**: a minus drawn as a line, or printed in the background colour, is
  invisible, and a statement that prints its signs that way is read with the signs
  **inverted** with nothing to catch it. An image missing them looks fine.
- **Folders** — can it write to `uploads\`, `logs\`, `feed\`? Inside the image these
  are empty directories; after step 3 they are mounts, and the answer can change.

Then run the **suite** in the image. This is the one that matters, because it proves
the image reads a statement the same way the machine you built it on does — a
different poppler or R minor version is exactly the kind of change that moves a
figure:

```
docker run --rm -e BSO_ALLOW_SKIPS=1 statement-studio:1.9.0 Rscript tests/run_tests.R
```

> That needs `COPY tests/` adding to the Dockerfile for the test run. Keep it out of
> the shipped image — a production container should not carry its own test corpus.

### 2. Carry the image to the offline host

Build where there is internet, then hand-carry:

```
docker save statement-studio:1.9.0 | gzip > statement-studio-1.9.0.tar.gz
# copy, then on the air-gapped host:
gunzip -c statement-studio-1.9.0.tar.gz | docker load
```

**Pin the tag to the VERSION, never `latest`.** A forensic tool has to be able to
say which build produced a figure, and `latest` makes that unanswerable.

### 3. Make the per-user directories

This is the isolation. The uid must match the Dockerfile's `studio` user:

```
for u in $(list-of-users); do
  mkdir -p /srv/studio-data/$u/{uploads,logs,feed,templates-user}
done
chown -R 10001:10001 /srv/studio-data
chmod -R 0700 /srv/studio-data/*
```

`0700` is deliberate: the directories are per-user and nothing on the host should
be reading across them either.

### 4. Point ShinyProxy at the directory

LDAP to your AD, so there is no second password list for evidence access and a
leaver loses access when their account is disabled rather than when somebody
remembers. **Not `simple` and not `none`** on a server holding evidence — `none`
makes every session anonymous, which makes the download log worthless.

### 5. The one app change you must make — and it is already done

ShinyProxy sets `SHINYPROXY_USERNAME` inside the container, and the app now reads it
as a **`host`** identity. That is a *stronger* guarantee than the shared-secret
header arrangement in who-is-using-it.md: nothing on the network can reach into a
container's environment, so there is no forgery to defend against and no secret to
keep in step.

Verify it, because if it silently fails the audit trail records `os` — which
`R/logging.R` documents as identifying **nobody** — and the download log cannot name
a person:

```
docker run --rm -e SHINYPROXY_USERNAME=test.analyst statement-studio:1.9.0 \
  Rscript -e 'cat(Sys.getenv("SHINYPROXY_USERNAME"), "\n")'
```

Then convert one statement through ShinyProxy as yourself and read
`/srv/studio-data/<you>/logs/runs/<run_id>.json`. You want
`identity_source: "host"` and `detected_identity` showing **your** account.

### 6. Turn the old route off

Stop the Windows service that runs `scripts\run_app.R` and set
`app.bind_host: "127.0.0.1"` in the config the container mounts. Two ways in is one
way in that nobody is watching.

---

## What to watch once it is live

| Symptom | Almost certainly |
|---|---|
| `identity_source: "os"` in a run record | `SHINYPROXY_USERNAME` is not reaching the container — step 5 |
| An analyst sees somebody else's uploads | the volume mount is not per-user — `#{proxy.userId}` missing from step 4 |
| Signs on a statement look inverted | `pdftocairo` missing from the image — step 1 |
| Containers accumulate | `heartbeat-timeout` unset; a closed tab leaves a copy of a statement sitting there |
| Conversions slow to a crawl with 3+ users | `max-total-instances` too high for the core count. `R/jobs.R` has the measurement: three concurrent scans on four cores had not finished a single **page** after ten minutes |
| Figures differ from the Windows deployment | a different R or poppler in the image. Run the suite in it (step 1) — that is what the suite is for |

## Related

- [who-is-using-it.md](who-is-using-it.md) — the cheaper half, needed either way
- [deploy-on-the-qlik-server.md](deploy-on-the-qlik-server.md) — the current Windows install
- [maintaining-the-engine.md](maintaining-the-engine.md) — the health check and the suite
- [backup-and-restore.md](backup-and-restore.md) — now `/srv/studio-data`, not the app folder
