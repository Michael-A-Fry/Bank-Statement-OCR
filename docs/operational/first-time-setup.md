# First-time setup (air-gapped server)

Two double-clicks: build a package on a PC with internet, copy one folder to the
server, run it. Nothing needs to be pre-installed on the server — not even R.

You need four things:

1. a Windows PC **with internet and R installed** — any recent version, from
   `cran.r-project.org`. R's own installer does not put it on the `PATH`, and it
   does not need to be: `make-bundle.bat` looks under `Program Files\R\` too.
2. **the app folder on that PC** — the source folder for this tool, holding
   `make-bundle.bat`, `app.R`, `R\`, `templates\` and `VERSION`. However it
   reaches you (a copy, a zip, a checkout), unpack it somewhere you can write to
   and work from there. It is not the same thing as the package you build in
   step 1.
3. the air-gapped Windows server;
4. an approved way to copy a folder between them.

Whatever R or RStudio the server already has is ignored and left untouched — the
package brings its own and installs it inside its own folder.

---

## 1 — Build the package (on the internet PC)

In the app folder, double-click **`make-bundle.bat`**.

A few minutes later you have one folder, **`StatementStudio-offline`**, holding
the app plus every R package, the R installer, and the Poppler/Tesseract
installers the server needs.

Read the last lines before you walk away. If it reports anything **MISSING**,
the PC's internet was blocked or proxied — fix that and run it again. A bundle
missing Poppler/Tesseract installs and runs fine and then cannot read a single
scanned PDF, which is the failure you least want to discover on the server.

### If the window flashes and disappears

**Every failure in that script ends by waiting for a keypress, so the window
cannot close on its own.** A flash therefore means the script never ran at all —
your PC's policy stopped it. Two ways to tell for certain:

1. Look for **`make-bundle.log`** beside `make-bundle.bat`. Writing it is the
   first thing the script does. **No log = it was blocked**, not broken.
2. Open **Command Prompt** first, then run it by typing the path — the window is
   yours, so nothing can vanish and you will see the actual message:
   ```
   cd /d C:\path\to\the\app
   make-bundle.bat
   ```

If it is blocked — often reported as *"blocked by your system administrator"* when
you use **Run as administrator** — that is AppLocker or a Software Restriction
Policy on a managed build. **No change to the file can get around it**, and on a
locked-down build *every* `.bat` is blocked, not just this one. Test it with any
throwaway `.bat` you like: if that flashes too, it is the policy, not this app.

> **Do not "Run as administrator" to get past this.** Building the package needs
> no special rights, and elevation does not lift a policy block — it just changes
> the error you get. Worth knowing before you ask IT for anything.

Use **`make-bundle.ps1`** instead. It does exactly the same build; PowerShell is
usually still permitted where `.bat` is not.

**Make a shortcut to it** (this is the part that gets past the policy):

1. Right-click in the app folder → **New** → **Shortcut**.
2. For the location, paste this, with your own path in place of `C:\path\to\app`:
   ```
   powershell.exe -ExecutionPolicy Bypass -NoProfile -File "C:\path\to\app\make-bundle.ps1"
   ```
3. Name it *Build the offline package*, and double-click it.

`-ExecutionPolicy Bypass` lifts PowerShell's **own** script policy for that one
launch. It is not an administrator action, needs no rights, and changes nothing
on the machine.

**If even that is blocked**, you can run the build with no script file at all —
open Command Prompt (or PowerShell) and type one line:

```
"C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" scripts\bundle-offline.R
```

Adjust the R version folder to the one you have. That runs R *with an argument*
rather than executing a script, so script policy has nothing to act on. It
produces the identical `StatementStudio-offline` folder — the `.bat`, the `.ps1`
and this line all do the same one thing.

If you would rather have the exception than the workaround, what to ask IT for is
permission to run `make-bundle.bat` (build PC) and `RUN-ME.bat` (server) from
their folders. That is a one-off setup step on two machines, not something every
analyst needs.

## 2 — Copy it to the server

Copy the whole `StatementStudio-offline` folder across, e.g. to
`D:\StatementStudio-offline`. That folder is the entire product.

## 3 — Run it (on the server)

Double-click **`RUN-ME.bat`**.

The first run installs a private copy of R inside the folder, installs the R
packages, sets up Poppler and Tesseract, **copies `config\config.example.yaml` to
`config\config.yaml`** — which is why step 4 finds the placeholder password
already in it — and starts the app on port **8100**. Every run after that just
starts it. The console line naming a URL contains the literal placeholder
`<this-vm>`; the app cannot know what name your network calls this box, so
substitute the server's own name or IP yourself.

`RUN-ME.bat` is the only thing that does that setup. If you are not on Windows,
the equivalent start command is `Rscript scripts/run_app.R` from the app folder,
but it starts the app and nothing else: it installs nothing and it will not
create `config\config.yaml`, so copy the example file across by hand first.

If a Windows permission prompt appears during the R install, accept it and run
`RUN-ME.bat` again. Setup retries until it succeeds — the "already done" marker
is only written when the install actually worked.

Leave the window open while the app runs; `Ctrl-C` stops it.

## 4 — Set the admin password

Open `config\config.yaml` in Notepad and change:

```yaml
app:
  admin_password: changeme
```

**Until you change it, the Admin tab refuses to open for anybody** — not even
with the right password typed, because the placeholder is printed in this repo
and in the example file. Admin manages templates, the shared label dictionary and
the analytics feed, so it stays shut. The app prints
`Admin is CLOSED - no admin password is set` at startup while that is the case.

Set it here, or set the `BSO_ADMIN_PASSWORD` environment variable (which wins),
then **restart the app**.

**Then check the lock actually moved.** Admin is reached by adding `?admin` to
the address — `http://localhost:8100/?admin` — and it is not offered as a tab
otherwise. Before you set a password that page says *"Admin is closed - no admin
password has been set"* and offers **no password box at all**; after you set one
and restart it says *"Admin - password required"* with a box. Type your password
and get in. A mis-indented line in `config.yaml` silently reverts the whole file
to built-in defaults, which puts the lock straight back on, so this is worth the
thirty seconds. Day-to-day use of Admin:
[admin-and-maintenance.md](admin-and-maintenance.md).

## 5 — Open the firewall port

On the server itself, browse to `http://localhost:8100`. It works.

For anyone else to reach it, open the port once. Windows Server blocks
unsolicited inbound TCP by default, so until you do this the app works on the
server and **times out for every other machine** — which looks exactly like the
app being broken.

Administrator Command Prompt, on the server:

```
netsh advfirewall firewall add rule name="Statement Studio 8100" dir=in action=allow protocol=TCP localport=8100
```

Then browse to `http://<server-name>:8100` from another machine. Still nothing?
Ask your network team — a separate network firewall between the users and the
server can block it too, and that one is not yours to change.

## 6 — Check the build stamp before anybody converts anything real

Convert `samples\raw\anz\anz_transaction_export_01.csv` — a synthetic specimen
that ships with the tool, with no client data in it — and open the **JSON**
download. Its third line is `build.engine_version`, and it must read the version
you shipped: the same value as `app_version` in `offline\manifest.txt`.

That one value is stamped into every run log, every JSON download and every row
of the Qlik feed manifest, and it is the whole of the answer to *"which build
produced this figure?"* years later. **If it reads `unknown`, the `VERSION` file
did not reach the server.** Copy `VERSION` from the source folder (the one you
built the package from) into the app folder by hand, restart, and check again.

---

## Next

- **Turning it on for the unit today?** → [go-live-checklist.md](go-live-checklist.md),
  which picks up from here: retention, the concurrency cap, proving a row reaches
  Qlik, and what "it is working" looks like an hour later.
- Keep it running after reboots → [running-and-keeping-it-up.md](running-and-keeping-it-up.md)
- Convert your first statement → [converting-statements.md](converting-statements.md)
  (it asks each analyst once per session for a **QID**, and Convert does nothing
  until that is filled in — that is the audit trail, not a fault)
- Wire up the Qlik dashboards → [connecting-qlik.md](connecting-qlik.md)
- Get the irreplaceable folders off the box → [backup-and-restore.md](backup-and-restore.md)
- Inherited this tool? → [maintaining-the-engine.md](maintaining-the-engine.md)

## If it goes wrong

| Symptom | Fix |
|---|---|
| `Could not set up the private R` | The build PC could not download R, or a permission prompt was declined. Check `offline\prereqs` holds an `R-*-win.exe`; if not, rebuild on a PC with clean internet and re-copy. |
| `Offline setup did NOT complete` / packages reported `MISSING` | Usually the bundle was built under a different R x.y. Rebuild with `make-bundle.bat`, re-copy the whole folder, run `RUN-ME.bat` again. |
| Works on the server, times out from everywhere else | The firewall port is not open — step 5. |
| Scanned PDFs do not read | Check `offline\manifest.txt`. `MISSING` → the build PC never downloaded Poppler/Tesseract; rebuild and re-copy. `included` → delete `offline\.installed` and run `RUN-ME.bat` again to redo the OCR install. Text PDFs, CSV and Excel keep working either way. |
| Admin will not open with the right password | The password is still `changeme` or blank. Step 4, then restart. |
| `build.engine_version` in a conversion says `unknown` | The `VERSION` file did not reach the server. Step 6. |
| Which version is on this server? | `offline\manifest.txt` — app version, the exact R, and every bundled package version. |
