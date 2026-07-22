# First-time setup (air-gapped server)

Get Statement Studio running on a server that has **no internet**. It's **two
double-clicks**: build a package on a PC that has internet, copy one folder to the
server, run it. No typing, no compiling, no admin knowledge needed.

You need:
- **One Windows PC with internet**, with **R** installed (any recent version — get
  it from `cran.r-project.org` if it isn't already there). This is only used to
  build the package.
- **The server** (air-gapped Windows). Nothing needs to be pre-installed on it —
  not even R. Whatever R or RStudio the server already has is **ignored and left
  untouched**.
- A copy of the **Statement Studio app folder** on the internet PC, and an approved
  way to copy a folder to the server (share or USB).

---

## Step 1 — Build the package (on the internet PC)
In the app folder, **double-click `make-bundle.bat`**.

It gathers the whole app plus every package and installer the server needs into a
single self-contained folder called **`StatementStudio-offline`**. This takes a few
minutes (it's downloading R packages and the R/OCR installers). When it says it's
done, that folder is everything the server needs.

> No version-matching to worry about: the R on this PC ships inside the package and
> the server uses that exact R, so it always matches.

---

## Step 2 — Copy it to the server
Copy the whole **`StatementStudio-offline`** folder to the server, e.g. to
`D:\StatementStudio-offline`. That one folder is the entire product — there is
nothing else to carry.

---

## Step 3 — Run it (on the server)
Open the folder and **double-click `RUN-ME.bat`**.

The **first** run, with no internet, it automatically:
1. installs a **private copy of R** inside the folder (isolated — your existing R /
   RStudio is not touched),
2. installs all the **R packages** it needs,
3. sets up **Poppler** and **Tesseract** so scanned-PDF reading works,
4. creates the settings file `config\config.yaml`,
5. **starts the app** and prints a web address like `http://<this-server>:8100`.

Every run after that just starts the app. **Leave the window open** while the app
is running; press `Ctrl-C` in it to stop.

> If a Windows permission prompt appears during the one-time R install, accept it,
> then run `RUN-ME.bat` again.

---

## Step 4 — Open it
On the server, open the printed address in a browser (or from another machine on
the network, use the server's name/IP: `http://<server-name>:8100`). You should see
the Statement Studio home page.

**Set the admin password and, if you use Qlik, the app URL** — see
[running-and-keeping-it-up.md](running-and-keeping-it-up.md) for the settings file.

---

## What next
- Keep it running after reboots → [running-and-keeping-it-up.md](running-and-keeping-it-up.md)
- Convert your first statement → [converting-statements.md](converting-statements.md)
- Wire up the Qlik dashboards → [connecting-qlik.md](connecting-qlik.md)

---

## If something goes wrong
| Symptom | Fix |
|---|---|
| "No R installer in offline\prereqs" | The build PC couldn't download R. Re-run `make-bundle.bat` on a PC with clean internet, then re-copy the folder. |
| A permission prompt was declined | Run `RUN-ME.bat` again and accept it. |
| Some R packages reported `MISSING` on first run | The build PC couldn't download them all (proxy/partial build). Re-run `make-bundle.bat` on a PC with clean internet and re-copy. |
| Scanned PDFs don't read | Poppler/Tesseract didn't install; text PDFs, CSV and Excel still work. Re-run `RUN-ME.bat`. |
