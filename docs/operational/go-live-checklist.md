# Go-live checklist

The day you turn it on, in order. **Every step has a check** — something you can
see with your own eyes that says the step worked. If a check does not come back
the way it is written here, stop on that step; the ones after it assume it passed.

Set aside an hour. You need: the server, an Administrator command prompt on it,
and one other machine on the network to test from.

Building the package and copying it across is [first-time-setup.md](first-time-setup.md);
this page starts from a folder already on the server.

If it goes wrong later in the day → [rolling-back.md](rolling-back.md).

---

## 1. Start it

**Do:** double-click **`RUN-ME.bat`** in the app folder. First run installs the
private R, the packages and the OCR tools; leave it alone until it settles.

**Check:** the console shows all four of these lines, and then stays open.

```
Statement Studio: Admin is CLOSED - no admin password is set. ...
Statement Studio ... starting on port 8100 (from D:\StatementStudio-offline). ...
Converting up to N statement(s) at once (app.max_concurrent_jobs). ...
Listening on http://0.0.0.0:8100
```

`Admin is CLOSED` is **correct at this point** — step 3 is what changes it.
`0.0.0.0` means "every network card", not an address, and
`http://<this-vm>:8100` on that same line is a placeholder the app prints
literally: substitute the server's own name. Note the `N`, you need it at step 6.

**If instead** you get `Offline setup did NOT complete` or `Could not set up the
private R` — the bundle is incomplete. [first-time-setup.md](first-time-setup.md),
last table. Do not continue.

## 2. Prove the build stamp

Nothing later can be traced back to a build without this, and it is silent when
it is wrong.

**Do:** on the server, browse to `http://localhost:8100`, convert
`samples\raw\anz\anz_transaction_export_01.csv` (a synthetic specimen that ships
with the tool — no client data in it), and download the **JSON**.

It reads as **7 transactions**, bank **ANZ**, confidence **medium** — six
payments out and one salary in. If that is not what you get, the install is
wrong, not the statement; go no further.

**Check:** the third line of that file is `build.engine_version`, and it reads
**the version you just shipped** — the same value as `app_version` in
`offline\manifest.txt`.

```
{
  "build": {
    "engine_version": "...",
```

**If it says `unknown`,** the `VERSION` file did not reach the server. Every run
log, every JSON and every row in the Qlik manifest will carry `unknown`, and
"which build produced this figure?" becomes unanswerable for everything converted
from now on — including the incident procedure's headline test
([investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md) §4).
Copy `VERSION` from the source folder into the app folder by hand, restart, and
repeat this step. **Do not convert anything real until it reads the version.**

## 3. Set the admin password, and see the lock move

**Do first, before setting anything:** browse to
`http://<server-name>:8100/?admin`.

**Check:** the page says **"Admin is closed - no admin password has been set"**
and there is **no password box at all**. That is the shipped state: the
placeholder in the example settings file is printed in these documents, so the
app treats it as "nobody has set one" and refuses for everybody. If you see a
password box here, somebody has already set one — find out who.

**Do:** open `config\config.yaml` in Notepad and replace the placeholder under
`app:` with your own password (or set the `BSO_ADMIN_PASSWORD` environment
variable, which wins over the file). Save. **Restart the app.**

**Check, all three:**

1. the startup console no longer prints `Admin is CLOSED`;
2. `?admin` now shows **"Admin - password required"** with a **Password** box;
3. your password gets you in, and a wrong one does not.

Do not skip 3 — a typo in YAML indentation reverts the whole file to built-in
defaults, which puts the lock straight back on.

## 4. Open the firewall port

**Do:** Administrator command prompt, on the server:

```
netsh advfirewall firewall add rule name="Statement Studio 8100" dir=in action=allow protocol=TCP localport=8100
```

**Check:** from **another machine**, `http://<server-name>:8100` loads the
Convert tab. Until this rule exists the app works on the server and times out for
everybody else, which looks exactly like the app being broken.

Still nothing? A separate network firewall between the users and the server can
block it too, and that one is not yours to change — ask your network team.

## 5. Confirm retention against the unit's own policy

`uploads\` keeps a copy of every converted statement. **These are real client
statements**, and this number is the only thing that deletes them.

**Do:** decide the number **with whoever owns the unit's evidence-retention
policy** — not on your own. Then set it in `config\config.yaml`:

```yaml
retention:
  uploads_keep_days: 90
```

`0` means keep them indefinitely. Restart the app.

**Check:** **Admin → Health → Saved statements - retention** states **your**
number — *"Uploads are kept N days…"*, or *"kept indefinitely"* if you set `0` —
and the red button beside it names the same number.

Two things to know before you shorten it. The tidy-up **also runs at startup**,
so a number smaller than the age of anything already under `uploads\` deletes it
the next time the app starts, with no confirmation. And the analyst uploading a
statement is **not** shown this number — the sentence lives in Admin, behind the
password — so if your policy requires telling her, that is a conversation, not a
screen.

## 6. Set the concurrency cap for this box

**Do:** if you have not measured this server, **leave `app.max_concurrent_jobs`
out entirely.** Left out, the app works it out from the hardware: one less than
the core count, never fewer than 2, never more than 6. Set it only if you know
better than that — for example when the box also runs something else, or when
you have measured OCR on it.

**Check, two lines of the same console:**

1. *"Converting up to N statement(s) at once"* reads the number you intend. This
   is the number the box will actually use.
2. **No** `Warning: app.max_concurrent_jobs is '...', which is not a number of
   jobs` further down, after the Shiny banner. That warning means your value was
   refused and the worked-out one is in use instead — and line 1 will have
   printed the worked-out number quite happily, so line 1 alone cannot tell you.

Anyone past N is queued and told they are queuing, by name and position. Nobody
gets a dead screen, so a cap that is too low is a slow day, not an outage — start
conservative.

## 7. Point the feed at the real folder, and prove a row lands

**Do:** in `config\config.yaml`, set `feed.feed_dir` to the folder the Qlik
**`StatementFeed`** connection reads — the same path, not a similar one:

```yaml
feed:
  enabled: true
  feed_dir: D:/StatementStudio/feed
```

Forward slashes or doubled backslashes. A UNC share (`//fileserver/share/feed`)
works, and the account the app runs under must be able to write to it. Restart.

**Check, in this order — the first two can both pass while Qlik has nothing:**

1. Convert the ANZ specimen from step 2 again. The Convert page says **"Sent to
   the dashboards"**.
2. On disk, `<feed_dir>\transactions\` gained a `.csv`, and `<feed_dir>\runs\`
   gained one too.
3. **Reload the Qlik app** and confirm the rows are in it, with the run showing
   as `accepted` in `Runs`.

Step 3 is the step that matters. **The app creates `feed_dir` if it is not
there**, so a typo in the path produces a brand-new empty folder, a green "Sent
to the dashboards", files on disk — and a dashboard that never gains a row.
Nothing on any screen will tell you; only Qlik can.

Left out entirely, `feed_dir` defaults to a `feed` folder **inside the app
folder**, which is fine for a trial and is not where Qlik is looking.

**Check the failure path too, once, now** — it is the one that goes quiet later.
Point `feed_dir` at a path the app cannot write, convert, and confirm the page
says *"The feed folder could not be written to, so the dashboards did NOT receive
this run."* Then put the real path back and convert again. Now you know what it
looks like.

## 8. Run one real statement end to end, and read the figures

The specimen proves the plumbing. **A statement the unit already has an answer
for** is what proves the tool.

**Do:** pick a statement somebody has previously worked by hand, from a bank with
a shipped template. Convert it. Download the Excel.

**Check, by eye, against the paper:**

- the **row count** matches the statement;
- the **first and last transactions** match — date, description, amount;
- the **opening and closing balances** on the `Summary` sheet match what is
  printed on the statement;
- descriptions are **verbatim** — same wording, same punctuation, nothing tidied;
- the **confidence level** is what the file deserves: *high* only for a CSV/TSV
  that reconciles, *medium* is the ceiling for any PDF or Excel file and is the
  normal healthy result there.

A figure that disagrees is a **stop**. Do not go live around it — take it to
[investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md).

## 9. Make it survive a reboot

**Do:** register the scheduled task exactly as
[running-and-keeping-it-up.md](running-and-keeping-it-up.md) describes —
including the `/service` argument and the **Settings** tab, where three
ticked-by-default options quietly kill it.

**Check:** reboot the server. When it comes back, without anybody logging in,
`http://<server-name>:8100` answers from another machine. That is the only check
that proves it; Task Scheduler reporting *Running* does not.

## 10. Back it up before anybody uses it

**Do:** run the backup from [backup-and-restore.md](backup-and-restore.md) once,
now, into a dated folder.

**Check:** open the folder you just wrote. `dictionaries\labels.yaml` is in it.
A backup nobody has ever opened is not a backup.

---

## An hour later: what "it is working" looks like

Come back after the first hour of real use and take these five readings. None of
them needs the app to be idle.

| Reading | Where | Healthy |
|---|---|---|
| People are converting | **Admin → Health**, *Refresh from logs*, *Uploads* | rows, with today's timestamps and more than one QID |
| Conversions are clean | same page, *Conversions by status* | mostly `ok`; a few `needs_review` is normal, a wall of `unsupported` means a bank has no template yet |
| The feed is still being written | `<feed_dir>\transactions\` | the file count has grown since step 7 |
| No feed write has failed | **Admin → Health → Analytics feed** | a green line. It counts the conversions that did not reach the dashboards as intended, so nobody has to go looking |
| Qlik is current | the Qlik app's last reload time | within its schedule, and its row count has grown |

**Read the Analytics feed line.** A share that goes read-only mid-morning used to
fail *loudly on the converting analyst's screen* and nowhere else: every
conversion still succeeded, every download was complete, and the dashboards
simply stopped gaining data. It is a server fault, so it is now reported on the
server's own page. (Headless, if you want it from a console:
`findstr /s /m "write_failed" logs\feed\*.json` — empty is the good answer.)

Then tell the team three things and nothing else:

1. the address, `http://<server-name>:8100`;
2. that they will be asked once per session for their **QID**, and Convert does
   nothing until it is filled in — that is the audit trail, not a bug;
3. that if a conversion looks wrong they should rate it **Wrong** in the app,
   because that is what withdraws its rows from the dashboards immediately.

## If it is going wrong

- A figure is wrong → [investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md)
- It will not come up, or nobody can reach it → [running-and-keeping-it-up.md](running-and-keeping-it-up.md)
- The new version is the problem → [rolling-back.md](rolling-back.md)
