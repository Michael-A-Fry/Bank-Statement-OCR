# Who is using it — letting the whole team in without letting them see each other's cases

For whoever runs the server. Two separate questions, and they have different
answers:

1. **Can the app tell who you are?** — this page, and it is solvable now.
2. **Can one investigator reach another's statements?** — not fully solvable on
   this architecture yet. The honest limits are in *What this does not fix* at the
   bottom. Read that section before you tell anyone the app is multi-user.

---

## The short version

| | Default as shipped | What you want |
|---|---|---|
| `app.bind_host` | `0.0.0.0` — every network card | `127.0.0.1` — this machine only |
| In front of it | nothing | a reverse proxy doing Windows Authentication |
| `app.identity_header` | empty — no header believed | the one header your proxy sets |
| `app.identity_shared_secret` | empty | a long random string, here **and** on the proxy |

Until all four are set, the run log records conversions against **the server's own
service account**, which identifies nobody, or against whatever name the person
typed on the Convert screen. That is honest. What it must never do is record a name
it cannot stand behind — see *Why the secret* below.

---

## Why the secret, and not just the header

A reverse proxy that authenticates against the directory can forward the signed-in
user in a request header. Every write-up of this pattern shows that, and it works.
The part usually left out is the half that makes it true.

**A header is a claim, not proof.** While the app listens on every network card,
anyone who can route to the server can skip the proxy entirely:

```
curl -H "X-Remote-User: someone.elses.name" http://statement-studio:8100/
```

and the audit record for that conversion then says `identity_source: sso`, which
`R/logging.R` defines as *"an identity forwarded by a proxy/gateway. Also
per-person."* The record certifies something it cannot know. For a file that may
be produced in court, that is worse than recording nothing: a blank is a gap, a
wrong name is evidence of the wrong thing.

So the app wants two things on the same request:

- **the header** — who you are, from the proxy;
- **the secret** — a long random string that exists nowhere but the proxy's own
  configuration, proving the request came through something entitled to make the
  claim.

No secret, no `sso`. The app **downgrades** rather than refusing — a mistyped
secret must not take the tool away from a whole office — so the only symptom of
getting this wrong is that people are asked to type their name again.

---

## Step 1 — generate a secret

Any long random string. On the server:

```
powershell -Command "[Convert]::ToBase64String((1..48 | ForEach-Object {Get-Random -Max 256}))"
```

Put it in `config\config.yaml`:

```yaml
app:
  identity_header: X-Remote-User
  identity_shared_secret: "<the string you just generated>"
  identity_secret_header: X-Statement-Studio-Secret
```

Treat `config.yaml` as a secret-bearing file from now on: NTFS permissions should
let the service account read it and nobody else.

## Step 2 — put a proxy in front

IIS is the first choice here, because it is already on the box for Qlik, it is
free with Windows Server, it runs as a service, and your platform team already
knows it. You need **Windows Authentication**, **URL Rewrite**, **Application
Request Routing 3.0** and the **WebSocket Protocol** role feature.

The proxy must do all four of these:

1. **Authenticate** the user against the directory (Windows Authentication,
   anonymous **disabled**).
2. **Strip** any inbound copy of `X-Remote-User` **and** `X_Remote_User` — both
   spellings, see the next section — so a client cannot supply its own.
3. **Set** `X-Remote-User` from the authenticated user, and
   `X-Statement-Studio-Secret` from step 1. Set, not append.
4. Forward **WebSocket** upgrades. Shiny does not work without them.

### Two things that will catch you out

**Test that the proxy can actually see the user name before you build the rest.**
URL Rewrite's inbound rules may run *before* IIS's authentication stage, in which
case `{LOGON_USER}` is empty and you get a blank header rather than an error.
Point the proxy at something that echoes headers and look, first. If it is empty,
the working alternatives are Apache httpd with `mod_authnz_sspi` (whose
`mod_headers` runs after authentication, so the problem does not arise) or a small
ASP.NET Core YARP service using `AddNegotiate()` and a request transform.

**The header must be on the WebSocket upgrade, not just the page load.** Shiny's
`session$request` is *the request that opened the WebSocket*, not the one that
fetched the page. A proxy configured to set the header only on the HTML GET will
look correct in a browser's network tab and deliver nothing to the app. This is
the single most common way this setup silently fails.

## Step 3 — close the back door

Once the proxy works, set:

```yaml
app:
  bind_host: "127.0.0.1"
```

and restart. The app now accepts connections only from the server itself, so the
proxy is the only way in and the header cannot be forged from the network.

**Do this as the same change as step 2, not before it.** On `127.0.0.1` with no
proxy running, nobody can reach the app at all.

A Windows detail worth knowing: **Windows Firewall does not filter loopback
traffic**, so a firewall rule on the port is not a substitute for the bind
address. If the socket is not listening on the LAN interface, there is nothing to
filter. Keep a deny-inbound rule on the port anyway — two controls, not one.

## Step 4 — check it

```
Rscript scripts\health-check.R
```

It reports which address the app is bound to and whether a forwarded identity will
be believed. Then convert one statement and read `logs\runs\<run_id>.json`: you
want `identity_source: "sso"` and `detected_identity` showing *your* account.

---

## Why one header name and not a list

The app used to accept any of eight: `X-Forwarded-User`, `X-Auth-Request-User`,
`X-Auth-Request-Email`, `X-Forwarded-Email`, `Remote-User`, `X-Remote-User`,
`X-Forwarded-Preferred-Username` and `CF-Access-Authenticated-User-Email` — a
Cloudflare header, on an air-gapped server. Eight names are eight things a client
can send and seven that no proxy here will ever set. It takes one configured name.

## Two header tricks the app refuses

Both come from how `httpuv` (the web server under Shiny) presents headers to R,
and both defeat a proxy that is otherwise configured correctly.

**Duplicates are joined with a comma.** The header map is case-insensitive, so
`X-Remote-User` and `x-remote-user` are one entry and two copies arrive as a single
value `attacker,real.detective`. A plain "is it non-empty" test accepts that as a
username. The app treats a comma in the identity value as an attack and discards
the claim.

**The underscore spelling is a different header that wins the same slot.** The map
*is* underscore-sensitive, so `X_Remote_User` is its own entry — but it normalises
to the same name R sees, and it is written second, so it overwrites the hyphen
form. A proxy that carefully strips and sets only `X-Remote-User` is therefore
bypassed by a client sending `X_Remote_User`. nginx drops underscore headers by
default for exactly this reason; **IIS and Apache do not.** The app checks the raw
header list and refuses the claim if more than one spelling of the name arrived.
Strip both at the proxy as well — two controls, not one.

---

## What this does not fix

Say this plainly to whoever owns the information, and get the residual risk
accepted rather than assumed away.

**Everyone still shares one R process.** `shiny::runApp()` — and open-source Shiny
Server — run **one R process for the whole app**, not one per user. Anything
defined outside `server()` is one object shared by every concurrent session, and
`tempdir()` (where an uploaded file first lands) is shared too. The app is written
correctly against this — every piece of per-user state lives inside `server()` —
but that is a property of the code, re-established on every change, not a boundary
the operating system enforces.

**So file permissions cannot help you.** Every uploaded statement is owned by the
same service account, so NTFS cannot tell one investigator from another: neither
of them is the process identity. Isolation is entirely in the app's own checks.

**There is no case ownership yet.** Uploads are not owned by anybody, any
signed-in user can list and download any upload, and cross-case reading is gated
on one shared admin password rather than on a reasoned, logged, per-case grant.

**Downloads ARE logged** — one record per download in `logs\downloads\`, naming
who, what, when, which conversion it came from, and the SHA-256 of the bytes handed
over. That last field is the one that matters after retention deletes the source
statement: it is then the only proof of what was produced and taken. Every download
handler writes one, including a download that handed back only an explanation.

**But a record is not a gate.** It says who took a copy; it does not stop anyone
taking one. While every analyst shares one server account there is no case
ownership to check against, so cross-case reading is still gated on the shared admin
password rather than on a reasoned, logged, per-case grant. The log makes it
*visible* after the fact, which is worth having and is not the same as prevention.

**Real per-user isolation would need one R process per analyst, and it is not being
built.** It was costed and rejected — see
[locked-decisions.md, D7](../context/architecture/locked-decisions.md). On Windows it
means a Linux guest running a container runtime: five new layers on an air-gapped box
maintained by one person, and Docker Desktop is licensed for government entities
unconditionally. Posit Connect is the paid alternative, and note that **without**
`RunAsCurrentUser` it shares one R process between up to 20 users, so buying it does
not buy isolation by itself. Shiny Server is not an option at all — it does not run on
Windows, and Pro support ended in March 2026.

So the honest position is that **this page is the isolation story**: identity is
recorded, and a download is logged against a named person. Separation is enforced by
the app's own checks, not by the file system. Revisit that only if a second analyst
must work the same case concurrently, or an audit requires OS-enforced separation.

---

## Related

- `docs/operational/deploy-on-the-qlik-server.md` — the install itself: service
  account, boot start, firewall, the address
- `docs/operational/investigating-a-wrong-conversion.md` — reading a run record
- `docs/operational/backup-and-restore.md` — what is irreplaceable
