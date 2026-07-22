# Running it and keeping it up

How to start the app, keep it running after reboots, and change its settings.

---

## Start / stop
- **Start:** double-click **`RUN-ME.bat`** in the app folder. It prints a web
  address (`http://<this-server>:8100`). Leave the window open.
- **Stop:** press `Ctrl-C` in that window, or close it.

Anyone on the network opens `http://<server-name>:8100` in a browser — nobody else
installs anything.

---

## Keep it running after every reboot (one-time)
So the app comes back on its own after a restart, register it as a scheduled task:

1. Open **Task Scheduler** → **Create Task…**
2. **General:** name it `Statement Studio`. Tick **Run whether user is logged on or
   not**.
3. **Triggers** → New → **Begin the task: At startup**.
4. **Actions** → New → **Start a program** → Program/script: browse to the app
   folder's **`RUN-ME.bat`**.
5. **OK.** It now starts automatically on boot, using the app's own private R.

To check it's running, open `http://<server-name>:8100`. To stop auto-start, disable
or delete that task.

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
| `logs\` | one small file per conversion (the run history the Admin tab reads) |
| `feed\` | the data the Qlik dashboards load ([connecting-qlik.md](connecting-qlik.md)) |
| `uploads\` | statements uploaded through the app |
| `R-runtime\`, `R-lib\`, `offline\` | the private R, its packages, and the installer bundle — leave these alone |

Nothing here is a database or a service framework — the folder **is** the install,
the YAML files are the settings, the JSON files are the logs.
