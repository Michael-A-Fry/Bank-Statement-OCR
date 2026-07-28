# Running it and keeping it up

Start it, keep it running after reboots, change its settings.

## Start / stop

- **Start:** double-click **`RUN-ME.bat`** in the app folder. Leave the window
  open.
- **Stop:** `Ctrl-C` in that window, or close it.

**It does not print an address you can click.** Its startup line names the port
and the folder it is running from, and then says `Open http://<this-vm>:8100` —
where `<this-vm>` is printed exactly like that. It is a placeholder the app does
not fill in, because it does not know what name your network calls this box.
Shiny then prints `Listening on http://0.0.0.0:8100`, which means "every network
card", not an address either.

Substitute the server's own name or IP yourself: `http://<server-name>:8100`.
What those two lines *do* prove is the port it actually came up on, which is the
thing worth reading when you have changed `app.port`.

`RUN-ME.bat` is the Windows server's way in, and it does more than start the app:
it installs the private R the first time, seeds `config\config.yaml` from the
example, and then starts the app. If you are not on Windows, or you already have
R and want to start it from a shell, run this from the app folder instead:

```
Rscript scripts/run_app.R
```

It reads the port and the upload ceiling from `config\config.yaml`, prints the
port it is starting on and the folder it is running from, says how many
statements it will convert at once, and warns on the console if the settings file
did not parse or the admin password is still the placeholder. It does **not** do
any of the setup — it will not create `config\config.yaml` for you (see
**Settings** below).

The concurrency line reads *"Converting up to N statement(s) at once
(app.max_concurrent_jobs). Anyone past that is queued and told so."* Each
conversion runs in its own short-lived R process, so one person converting a
scanned statement no longer freezes everybody else's browser. N comes from
`app.max_concurrent_jobs` in `config\config.yaml`; leave it out and the app works
one out from the box. Anyone past N is queued and told they are queuing — nobody
gets a dead screen.

Anyone on the network opens `http://<server-name>:8100` in a browser. Nobody else
installs anything — but the first time, that needs one firewall rule.

## Let other people reach it (one-time, needs admin)

The app listens on port **8100** on every network card, but Windows Server blocks
unsolicited incoming connections. Until you open the port the app works **on the
server itself** and **times out for everyone else** — which looks like the app is
broken when it is not.

Administrator Command Prompt on the server:

```
netsh advfirewall firewall add rule name="Statement Studio 8100" dir=in action=allow protocol=TCP localport=8100
```

Check it, or remove it:

```
netsh advfirewall firewall show rule name="Statement Studio 8100"
netsh advfirewall firewall delete rule name="Statement Studio 8100"
```

Change `app.port` and you need a matching rule for the new port (and to delete
the old one). If it is still unreachable, ask your network team: a separate
network firewall between the users and the server can block it too.

## Keep it running after every reboot (one-time)

Register it as a scheduled task. Three ticked-by-default settings quietly break
this; they are called out at the step, so do not skip the **Settings** tab.

1. **Task Scheduler** → **Create Task…** (not "Create Basic Task").
2. **General:** name it `Statement Studio`. Tick **Run whether user is logged on
   or not** and **Run with highest privileges**.
3. **Triggers** → New → **Begin the task: At startup**.
4. **Actions** → New → **Start a program**:
   - **Program/script:** the app folder's **`RUN-ME.bat`**
   - **Add arguments:** `/service`
   - **Start in:** the app folder, e.g. `D:\StatementStudio-offline`

   `/service` matters. Without it `RUN-ME.bat` ends on a "Press any key" pause, so
   Windows reports the task as **Running** long after the app has stopped — and
   never restarts it.
5. **Settings** tab — **the step everyone misses**:
   - **UNTICK "Stop the task if it runs longer than: 3 days".** Ticked by default,
     so the app is killed mid-week, every week.
   - Tick **"If the task fails, restart every:"** `1 minute`, **"Attempt to
     restart up to:"** `3 times`.
   - Leave **"If the task is already running… Do not start a new instance"** —
     two copies would fight over the same port.
6. **OK**, enter the account password if asked.

To stop auto-start, disable or delete the task. To restart now: right-click →
**End**, then **Run**.

### Is it running?

| Check | How |
|---|---|
| Quickest | Open `http://<server-name>:8100` from another machine. |
| On the server | Task Scheduler → **Last Run Result** `(0x41301)` = currently running. `0x0` means it **exited**. |
| From a command prompt | `tasklist /fi "imagename eq Rscript.exe"` — one `Rscript.exe` = the app is up. |
| Is the port listening | `netstat -ano \| findstr :8100` |

## Who can use it, and who did what

There is no sign-in screen. Access is controlled the way the rest of your file
shares are: **the network path to the server and the firewall rule**. If your
site puts a reverse proxy or SSO in front of it, the app reads the signed-in user
from the usual proxy headers and stops asking.

Where it cannot establish an identity, the Convert tab asks once per session for
a **QID** (six letters or digits) and refuses to convert without one, so no run
is ever recorded against nobody. Every conversion writes `logs\runs\<run_id>.json`
carrying both the typed claim and whatever the machine could detect, as separate
fields — so a reader can tell an attested name from a proven one.

## Settings — `config\config.yaml`

One file. **`RUN-ME.bat` creates it**, by copying `config\config.example.yaml` the
first time it runs (and it restores your saved copy from `%LOCALAPPDATA%` after an
update instead, if there is one). Nothing else creates it: start the app with
`Rscript scripts/run_app.R` on a fresh folder and `config\` still holds only
`config.example.yaml`, the app runs on built-in defaults, and it says
`Admin is CLOSED - no admin password is set` with no file there to edit. If that
is where you are, copy `config.example.yaml` to `config.yaml` yourself.

Edit in Notepad and **restart the app** to apply. Any key you leave out uses a
built-in default, so a short file is fine.

```yaml
app:
  admin_password: changeme             # PLACEHOLDER. Admin refuses to open until you replace it
  shiny_url: http://your-server:8100   # the address the Qlik tile opens
  port: 8100
  max_upload_mb: 200
retention:
  uploads_keep_days: 90                # how long a copy of each uploaded statement is kept
feed:
  enabled: true
  feed_dir: D:/StatementStudio/feed    # point at the Qlik share
  min_trust: medium                    # medium = every clean conversion; high = balance-proven only
```

`changeme` above is the shipped placeholder, printed verbatim so that copying this
block leaves the Admin lock **on**. It is not a password — the app treats it as
"nobody has set one yet" and refuses to open Admin for anybody, including with
`changeme` typed. Replace it with your own (or set `BSO_ADMIN_PASSWORD`, which
wins over the file) and restart. A blank value means the same thing.

Paths take forward slashes (`D:/folder`) or doubled backslashes (`D:\\folder`).
The full annotated list is `config\config.example.yaml`.

## Where things live (inside the app folder)

| Folder | What is in it |
|---|---|
| `config\` | your settings (`config.yaml`) |
| `templates\` | the shipped, tested bank templates |
| `templates_user\`, `fields_templates_user\` | templates your team made in the app — **irreplaceable** |
| `dictionaries\` | the wordings and markers Admin has taught it — **irreplaceable** |
| `logs\` | one small file per conversion; `logs\metadata\` is kept forever — **irreplaceable** |
| `feed\` | what the Qlik dashboards load ([connecting-qlik.md](connecting-qlik.md)) |
| `uploads\` | copies of uploaded statements, deleted after `uploads_keep_days` |
| `R-runtime\`, `R-lib\`, `offline\` | the private R, its packages, the installer bundle — leave alone |

The folder **is** the install: no database, no service framework. The YAML files
are the settings and the JSON files are the logs.

Copy the irreplaceable rows off the box regularly —
[backup-and-restore.md](backup-and-restore.md).

## When it will not come up

| Symptom | Likely cause |
|---|---|
| Works on the server, times out from every other machine | Firewall port not open — the `netsh` line above. |
| Window closes instantly, nothing happens | Setup did not finish. Run `RUN-ME.bat` by double-click (not the task) and read the message; it retries the install by itself. |
| Everything works except scanned PDFs | Poppler/Tesseract not installed — check `offline\manifest.txt`; if `MISSING`, rebuild the package on the internet PC. |
| Task says it ran and finished, nothing is listening | The task is missing the `/service` argument, so it stopped at a pause. |
| Admin will not open with the right password | `app.admin_password` is still `changeme` or blank. Change it and restart. |

More: [when-something-goes-wrong.md](when-something-goes-wrong.md) ·
[maintaining-the-engine.md](maintaining-the-engine.md).
