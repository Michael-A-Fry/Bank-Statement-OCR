# Running it and keeping it up

How to start the app, keep it running after reboots, and change its settings.

---

## Start / stop
- **Start:** double-click **`RUN-ME.bat`** in the app folder. It prints a web
  address (`http://<this-server>:8100`). Leave the window open.
- **Stop:** press `Ctrl-C` in that window, or close it.

Anyone on the network opens `http://<server-name>:8100` in a browser — nobody else
installs anything. **The first time, that needs one firewall rule** — see below.

---

## Let other people reach it (one-time, needs admin)
The app listens on port **8100** on every network card, but Windows Server blocks
unsolicited incoming connections by default. Until you open the port, the app works
**on the server itself** and **times out for everyone else** — which looks like the
app is broken when it isn't.

Open an **Administrator** Command Prompt on the server and run this one line:

```
netsh advfirewall firewall add rule name="Statement Studio 8100" dir=in action=allow protocol=TCP localport=8100
```

To check the rule is there:

```
netsh advfirewall firewall show rule name="Statement Studio 8100"
```

To remove it again:

```
netsh advfirewall firewall delete rule name="Statement Studio 8100"
```

If you change `app.port` in `config\config.yaml`, add a matching rule for the new
port (and delete the old one). If the port is still unreachable after this, ask your
network team — a separate network firewall between the users and the server can block
it too, and that one isn't yours to change.

---

## Keep it running after every reboot (one-time)
So the app comes back on its own after a restart, register it as a scheduled task.
The three ticked-by-default settings that quietly break this are called out below —
don't skip the **Settings** tab.

1. Open **Task Scheduler** → **Create Task…** (not "Create Basic Task").
2. **General:** name it `Statement Studio`. Tick **Run whether user is logged on or
   not**. Tick **Run with highest privileges**.
3. **Triggers** → New → **Begin the task: At startup**.
4. **Actions** → New → **Start a program**:
   - **Program/script:** browse to the app folder's **`RUN-ME.bat`**
   - **Add arguments:** `/service`
   - **Start in:** the app folder (e.g. `D:\StatementStudio-offline`)

   `/service` matters: without it `RUN-ME.bat` ends on a "Press any key" pause, so
   Windows keeps reporting the task as **Running** long after the app has stopped —
   and never restarts it.
5. **Settings** tab — **this is the step everyone misses**:
   - **UNTICK "Stop the task if it runs longer than: 3 days".** It is ticked by
     default, so the app is killed mid-week, every week.
   - Tick **"If the task fails, restart every:"** `1 minute`, **"Attempt to restart
     up to:"** `3 times`.
   - Leave **"If the task is already running, then the following rule applies: Do
     not start a new instance"** (the default) — two copies would fight over the
     same port.
6. **OK**, enter the account password if asked. It now starts on boot, using the
   app's own private R.

### Is it running?
| Check | How |
|---|---|
| Quickest | Open `http://<server-name>:8100` from another machine. |
| On the server | Task Scheduler → the task's **Last Run Result** should be `(0x41301)` = *currently running*. `0x0` means it exited — it is **not** running. |
| From a command prompt | `tasklist /fi "imagename eq Rscript.exe"` — one `Rscript.exe` means the app is up. |
| The port is actually listening | `netstat -ano \| findstr :8100` |

To stop auto-start, disable or delete that task. To restart it now: right-click the
task → **End**, then **Run**.

---

## Settings — `config\config.yaml`
All settings live in one file, **`config\config.yaml`**, created for you on first
run. Open it in Notepad, change what you need, and **restart the app** (`RUN-ME.bat`)
to apply.

```yaml
app:
  admin_password: change-me            # the password for the Admin tab
  shiny_url: http://your-server:8100   # the address the Qlik tile opens (see connecting-qlik.md)
  port: 8100                           # the web port
feed:
  enabled: true                        # write the Qlik feed on every conversion
  feed_dir: D:/StatementStudio/feed    # where the feed is written (point at the Qlik share)
  min_trust: medium                    # medium = every clean conversion; high = only balance-proven ones
```

Rules of thumb:
- **Paths:** use forward slashes (`D:/folder`) or doubled backslashes (`D:\\folder`).
- **Anything you leave out** uses a sensible default — the file only needs the lines
  you want to change.
- **Password:** set `admin_password` to something real before you share the address.

---

## Where things live (inside the app folder)
| Folder | What's in it |
|---|---|
| `config\` | your settings (`config.yaml`) |
| `templates\` | the built-in, proven bank templates |
| `templates_user\`, `fields_templates_user\` | templates your team made in the app — **irreplaceable** |
| `dictionaries\` | the wordings and recognition markers Admin has taught it — **irreplaceable** |
| `logs\` | one small file per conversion (the run history the Admin tab reads) |
| `feed\` | the data the Qlik dashboards load ([connecting-qlik.md](connecting-qlik.md)) |
| `uploads\` | statements uploaded through the app |
| `R-runtime\`, `R-lib\`, `offline\` | the private R, its packages, and the installer bundle — leave these alone |

Nothing here is a database or a service framework — the folder **is** the install,
the YAML files are the settings, the JSON files are the logs.

The two rows marked **irreplaceable** are the accumulated work of your team and
exist nowhere else — copy them off the box regularly:
[backup-and-restore.md](backup-and-restore.md).

---

## When it won't come up
| Symptom | Likely cause |
|---|---|
| Works on the server, **times out from every other machine** | The firewall port isn't open — run the `netsh` line above. |
| Nothing happens at all, window closes instantly | Setup didn't finish. Run `RUN-ME.bat` by double-click (not the task) and read the message; it retries the install by itself. |
| "Scanned PDFs won't read" but everything else is fine | Poppler/Tesseract aren't installed — check `offline\manifest.txt`; if it says MISSING, rebuild the package on the internet PC. |
| The task says it ran and finished, but nothing is listening | The task is missing the `/service` argument, so it stopped at a pause. |

More: [when-something-goes-wrong.md](when-something-goes-wrong.md) ·
[maintaining-the-engine.md](maintaining-the-engine.md).
