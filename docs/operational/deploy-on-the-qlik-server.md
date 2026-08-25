# Putting it on the Qlik server, start to finish

One page. Follow it top to bottom once and you get: **Statement Studio running on
the Qlik server, starting itself after every reboot under a service account, and
reachable from every desk on the network at `http://qlikserver:8100/`.**

Nobody but the server needs R, or anything else, installed.

Do these in order. Each step says how to prove it worked before you move on.

> **What you need before you start:** the offline bundle folder copied onto the
> server, a service account, and one session of local-administrator rights on the
> server (for the firewall rule, the port reservation and the scheduled task).

---

## Step 1 — Put the folder somewhere permanent

Copy the app folder to a **data drive**, not the desktop and not a user profile:

```
D:\StatementStudio
```

**Why it matters:** the folder *is* the install. Settings, templates, logs and
the dictionaries all live inside it. A profile folder is wiped when a service
account is recycled; a desktop folder is invisible to a service account
altogether.

Avoid a path with spaces if you can. `D:\Statement Studio` works, but every
command below then needs quoting, and one missing quote is a scheduled task that
silently does nothing.

**Prove it:** `dir D:\StatementStudio\RUN-ME.bat` lists the file.

---

## Step 2 — Decide the service account

Use a dedicated account. Any of these is fine:

| Account | When to use it |
|---|---|
| A domain service account, e.g. `DOMAIN\svc_statementstudio` | Standard choice. Use this if you have one. |
| The account Qlik already runs under | Works, but ties two unrelated services together — if one is locked out, both stop. |
| `NT AUTHORITY\NETWORK SERVICE` | Only if the app never needs to write to a **network** share. It cannot reach the Qlik feed folder if that folder is a UNC path. |

**The account needs three things.** All three are easy to miss and each produces
a different confusing failure:

1. **"Log on as a batch job"** — without it the scheduled task fails instantly
   with `2147943785` and never starts the app.
   Local Security Policy → *Local Policies* → *User Rights Assignment* →
   **Log on as a batch job** → add the account.
2. **Modify** on `D:\StatementStudio` and everything under it — it writes
   settings, logs, uploads, templates and the feed.
   ```
   icacls "D:\StatementStudio" /grant "DOMAIN\svc_statementstudio:(OI)(CI)M" /T
   ```
3. **Write** to the Qlik feed folder, if that is somewhere else (`feed.feed_dir`
   in `config\config.yaml`). If it is a UNC path the account must be a domain
   account — a local or `NETWORK SERVICE` account cannot reach it.

**A password that expires stops the app on the next reboot, not today.** Set the
account to *Password never expires*, or put a calendar reminder against it. When
the password changes you must re-enter it in the scheduled task as well.

**Prove it:** `whoami /priv` run **as that account** lists `SeBatchLogonRight`.

---

## Step 3 — Claim the port so nothing else can take it

The app listens on **TCP 8100**.

### Why 8100 and not something else

- It is not one of the ports Qlik Sense, QlikView or NPrinting listen on. Those
  cluster in **80/443**, the **42xx–47xx** block (repository, proxy, engine,
  scheduler, certificates), **5050/5151**, **5432/5672/6379** (the databases and
  queues NPrinting brings), and **90xx** (the service dispatcher's children).
  8100 is in none of them.
- It is well away from the common web-alternative ports (8080, 8443, 8000, 8888)
  that someone else on the box is most likely to want.
- It is below 49152, so it is outside the range Windows hands out to *outgoing*
  connections — which matters, and is the subject of the next paragraph.

**Do not take my word for it — check on the box.** Run this on the server:

```
netstat -ano | findstr :8100
```

Nothing back means nothing is listening on it right now.

### Make it undisputed

"Nothing is listening right now" is not the same as "nothing will take it". The
one that bites is Windows itself: it allocates *outgoing* connections from a
dynamic range, and if that range has been widened, an outgoing socket can grab
8100 minutes before the app starts at boot — after which the app cannot bind and
the service looks broken for no visible reason.

Reserve it. Administrator command prompt, **once**:

```
netsh int ipv4 add excludedportrange protocol=tcp startport=8100 numberofports=1
```

Check it took, and see the whole dynamic range while you are there:

```
netsh int ipv4 show excludedportrange protocol=tcp
netsh int ipv4 show dynamicport tcp
```

After this, Windows will refuse to hand 8100 to anything else, and the app's
claim on it is as close to guaranteed as the operating system allows.

> **Changing the port later.** Set `app.port` in `config\config.yaml`, redo the
> reservation and the firewall rule for the new number, and delete the old ones.
> The app prints the port it actually came up on, so you can always check.

---

## Step 4 — Start it once by hand, and watch it

Do this **before** wiring up the scheduler. It proves the install, and the first
run is the slow one — it installs the private R inside the folder.

Log on to the server (as anyone) and **double-click `RUN-ME.bat`**.

First run takes a few minutes and says what it is doing. Every run after that
starts in seconds. Leave the window open — it prints:

```
Statement Studio - starting on port 8100 (from D:\StatementStudio).
  Open  http://QLIKSERVER:8100/   (or http://<this-server-name-or-ip>:8100/)
Listening on http://0.0.0.0:8100
```

`0.0.0.0` means "every network card", not an address. The line above it has the
address you want, with this machine's real name already in it.

**Prove it:** open `http://localhost:8100/` in a browser **on the server**. You
should get the Statement Studio home page.

Then press `Ctrl-C` in the window to stop it.

> **It also wrote `logs\startup.log`.** One line per start: version, folder,
> account, port, address, and any reason it refused to start. That file is the
> first thing to read whenever it will not come up, and the only evidence you get
> once it is starting at boot with no window.

---

## Step 5 — Set the admin password

Open `D:\StatementStudio\config\config.yaml` in Notepad. `RUN-ME.bat` created it
in step 4.

```yaml
app:
  admin_password: changeme                 # REPLACE THIS with your own
  shiny_url: http://qlikserver:8100        # the address the Qlik tile opens
  port: 8100
```

`changeme` above is the shipped placeholder, printed verbatim so that copying
this block leaves the Admin lock **on**. It is not a password: the app treats it
as **"nobody has set one yet"** and refuses Admin to everybody, including anybody
typing `changeme`. A blank value means the same thing.

Replace it with your own — or set the `BSO_ADMIN_PASSWORD` environment variable
on the service account, which wins over the file and keeps the password out of a
file that gets copied around with the folder. Until you do, the console and
`logs\startup.log` both say `Admin is CLOSED - no admin password is set`.

**Prove it:** restart the app and that line is gone.

---

## Step 6 — Open the firewall

Until you do this, the app works **on the server** and times out for everyone
else — which looks exactly like the app being broken.

Administrator PowerShell on the server:

```powershell
New-NetFirewallRule -DisplayName "Statement Studio 8100" `
  -Direction Inbound -Action Allow -Protocol TCP -LocalPort 8100 `
  -Profile Domain -RemoteAddress LocalSubnet
```

`-Profile Domain` and `-RemoteAddress LocalSubnet` keep it to your own network.
Widen `-RemoteAddress` to your actual client subnets if the desks are on a
different one from the server; drop it entirely only if you mean *anyone who can
route to this box*.

The `netsh` equivalent, if you prefer it:

```
netsh advfirewall firewall add rule name="Statement Studio 8100" dir=in action=allow protocol=TCP localport=8100
```

Check it, or remove it:

```powershell
Get-NetFirewallRule -DisplayName "Statement Studio 8100"
Remove-NetFirewallRule -DisplayName "Statement Studio 8100"
```

**Prove it:** from **another** machine, `Test-NetConnection qlikserver -Port 8100`
returns `TcpTestSucceeded : True`. If it does not and the app is running, the
block is between you and the server — ask the network team, there may be a second
firewall in the path.

---

## Step 7 — Make it start itself at every boot

Task Scheduler on the server. **Create Task…**, not *Create Basic Task* — the
basic wizard cannot set the three things that matter.

**General tab**
- Name: `Statement Studio`
- **Change User or Group…** → the service account from step 2
- **Run whether user is logged on or not** ✔
- **Do not store password** ✘ (leave unticked — it needs the password to run at
  boot with no session)
- **Run with highest privileges** ✔
- Configure for: your Windows Server version

**Triggers tab** → New
- Begin the task: **At startup**
- **Delay task for: 1 minute** ✔ — gives the network stack and the drives time to
  come up. Without it, a boot-time start can fail to bind or fail to see `D:`.

**Actions tab** → New → *Start a program*
- Program/script: `D:\StatementStudio\RUN-ME.bat`
- Add arguments: `/service`
- Start in: `D:\StatementStudio`

> **`/service` is not optional.** Without it `RUN-ME.bat` ends on a
> "Press any key" pause, so Windows sees the task as **Running** long after the
> app has stopped — and restart-on-failure never fires. `Start in` is not
> optional either: without it the task runs from `C:\Windows\System32`.

**Conditions tab**
- **Start the task only if the computer is on AC power** ✘ — untick. On a VM this
  is meaningless and can block the start.

**Settings tab — the one everybody skips**
- **Stop the task if it runs longer than: 3 days** ✘ — **untick it.** It is
  ticked by default, and it kills the app mid-week, every week.
- **If the task fails, restart every:** `1 minute`, **up to:** `3 times` ✔
- If the task is already running: **Do not start a new instance** (two copies
  would fight over the port)

**OK**, and enter the account's password when asked.

**Prove it:** right-click the task → **Run**. Within a minute:

| Check | What you should see |
|---|---|
| Task Scheduler → *Last Run Result* | `(0x41301)` — "currently running". `0x0` means it **exited**. |
| `logs\startup.log` | a fresh block ending `listening on every network card` |
| `netstat -ano \| findstr :8100` | a `LISTENING` line |
| From another desk | `http://qlikserver:8100/` opens |

Then **reboot the server** and check the same four. That is the only test of
"starts at boot" that counts.

---

## Step 8 — The address people use

**`http://qlikserver:8100/`** — put that in the team's bookmarks and in
`app.shiny_url` so the Qlik tile opens it too.

**Who can get in is the network, exactly as it is for Qlik.** There is no sign-in
screen. The firewall rule from step 6 is the control: anyone who can reach the
Qlik server can reach Statement Studio, and nobody else can. That is deliberate —
it is the same boundary your Qlik installation already has, administered in the
same place, rather than a second set of accounts for somebody to keep in step.

Where the app cannot work out who someone is, the Convert tab asks once per
session for a **QID** and refuses to convert without one, so no conversion is ever
recorded against nobody.

### A friendlier name, if you want one

One **CNAME** in your internal DNS, and nothing changes on the server:

```
statementstudio.yourdomain.local.   CNAME   qlikserver.yourdomain.local.
```

giving **`http://statementstudio:8100/`**. That is the whole job — a CNAME is
resolved before the browser ever connects, so the request arrives at the same
machine on the same port and the app needs to know nothing about it.

Update `app.shiny_url` to match and tell the team the new address.

### Why the address keeps its `:8100`

A URL with no port number means **port 80**, and on this machine port 80 belongs
to the Qlik Sense proxy. Two services cannot share it, and taking it from Qlik is
not a trade worth making for five characters in a bookmark.

---

## When it will not come up

**Read `logs\startup.log` first.** It is written before anything that can fail,
so there is always something in it.

| What the log says | What it means |
|---|---|
| `FAILED TO START - PORT 8100 IS ALREADY IN USE` | Another copy is running, or something took the port. `netstat -ano \| findstr :8100`, then `tasklist /fi "pid eq <pid>"`. |
| `SETTINGS FILE NOT LOADED` | `config\config.yaml` does not parse — usually a tab character or a stray quote. It is running on defaults, including the wrong feed folder. |
| `Admin is CLOSED` | `app.admin_password` is still `changeme` or blank. |
| `STOPPED WITH AN ERROR` | The app started and then fell over; the message is on the same line. |
| Nothing at all, ever | The task is not running the batch file. Check *Start in*, the account's **Log on as a batch job** right, and *Last Run Result*. |

| Symptom | Likely cause |
|---|---|
| Works on the server, times out everywhere else | Firewall rule (step 6), or a second firewall in the network path. |
| Task says it ran and finished, nothing listening | The `/service` argument is missing (step 7). |
| Worked for months, dead after a reboot | The service account's password expired or changed. Re-enter it in the task. |
| Everything works except scanned PDFs | Poppler/Tesseract missing — check `offline\manifest.txt`. |

More: [when-something-goes-wrong.md](when-something-goes-wrong.md) ·
[running-and-keeping-it-up.md](running-and-keeping-it-up.md) ·
[first-time-setup.md](first-time-setup.md)

---

## The whole thing, as a checklist

```
[ ] folder on a data drive, e.g. D:\StatementStudio
[ ] service account chosen
[ ]   "Log on as a batch job" granted
[ ]   Modify on the app folder (icacls ... /T)
[ ]   Write on the Qlik feed folder
[ ]   password set not to expire
[ ] port 8100 free      netstat -ano | findstr :8100
[ ] port 8100 reserved  netsh int ipv4 add excludedportrange protocol=tcp startport=8100 numberofports=1
[ ] RUN-ME.bat double-clicked once, http://localhost:8100/ opens
[ ] admin password set in config\config.yaml (app.admin\_password)
[ ] firewall rule added, Test-NetConnection from another desk succeeds
[ ] scheduled task created  (At startup + 1 min delay + /service + Start in
    + restart on failure + "stop after 3 days" UNTICKED)
[ ] task Run once: 0x41301, startup.log fresh, port listening
[ ] SERVER REBOOTED and all four checks still pass
[ ] app.shiny_url set to the address people type, so the Qlik tile matches
[ ] address given to the team
```
