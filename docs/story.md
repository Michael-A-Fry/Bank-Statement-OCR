# Statement Studio — how it got its shape

The short version of where this started and every big turn that moved it. Not a
list of fixes; the moments the thing became something different.

---

### It began as a replacement for something that already worked

There was a Qlik app that converted bank statements for casework, and it did the
job. It also had a paid PDF connector underneath it, a page of hand-written script
for every bank, and its logic locked inside a binary nobody could read. Adding a
bank meant writing more script. That was the brief: same result, no licensed
dependency, and maintainable by one analyst instead of a developer.

### The first decision was the one that shaped everything else

Before a line was written, the north star was set: **never guess**. No machine
learning, no probabilistic inference, no clever recovery. If it matches, it
matches; if it does not, it says so. A wrong figure that looks right was named the
cardinal failure, because these numbers end up in evidence. Everything downstream
— the checks, the confidence levels, the refusals, the whole tone of the screen —
is a consequence of that one call.

### Real statements before any code

The first work was not the engine. It was collecting public specimen statements
from every major bank, cataloguing them, and writing down the data contract the
engine would have to honour — the exact columns, the exact statuses, the exact
guarantees — before anything was built against it. The declarative approach was
explicitly provisional until real files proved it. They did.

### A bank became a file, not a function

The engine landed as pure R with one idea holding it up: a bank layout is a
declarative description, not a piece of code. Which words identify it, which
column means what, how its dates and signs are written. Every layout got a golden
test that pins its output against a real statement. From then on, "we need to
support another bank" stopped being an engineering estimate.

### PDFs stopped being a special case

The hardest format got the same treatment as the easiest. Instead of per-bank
scripts walking the page, a PDF template describes its columns as bands across the
page, and one reader turns word positions into rows for every bank. Scanned pages
were never a phase two — OCR was day one, with the results always marked as
machine-read. Then redactions were made a first-class concern, because a black box
drawn over a PDF usually leaves the text still in the file, and reading it would
leak the exact thing somebody chose to hide.

### The wizard changed who owns a new bank

Adding a layout was still a YAML file, which still meant a developer. So the tool
grew a point-and-click toolkit: upload one example, and the tool proposes what it
found while your statement stays on screen beside it; drag boxes over the columns
on a PDF; confirm against a live preview; save. The template system had been the
architecture. Now it was the product, and the person who adds a bank became the
analyst who needs it.

### The checks became the product

Speed was never the hard part. Being able to say *nothing was missed* was. So
reconciliation moved from a feature to the point of the thing: opening plus every
transaction against the printed closing, the running balance followed row by row,
dates, row counts, redaction counts, OCR quality — each stated as one plain
sentence with a confidence level, and each able to say "could not be checked"
rather than show a green tick it had not earned. Alongside them came diagnostics
that name not just what went wrong but **who fixes it**: you, the file, or a
developer.

### It turned from a converter into a governed data source

Until this point the output was a file somebody downloaded. Then it became org
data: a governed feed that the dashboards load automatically. That changed what
"finished" means. A conversion now has to pass every check *and* come from a
tested template before its rows are published — and the gate is machine-only, so
nobody can wave a conversion through because they are in a hurry. Everything else
is held back, visibly, with the reason on screen and in a separate table the team
can look at.

### The deployment turned out to be a locked room

The real environment was an air-gapped Windows server with no internet, no
database, no admin rights to spare and no version control. That reshaped the
delivery end of the product: build a bundle on a machine that has internet, carry
one folder across, double-click. The install brings its own private copy of R.
Concurrency was solved by never appending to a shared file — one file per event —
so ten people can use it at once with nothing to lock and nothing to tune.

### The charter, and the correctness campaign it started

With the thing built, the fixed points were written down: what it must always do,
what it must never do, who it is for. Then it was audited against them,
adversarially, and the audit found silently-wrong paths — a catch-all template
that parsed a foreign bank's statement as another's, a date format that could
invent a year, a sign that could invert, redactions that could leak. Each was
closed with a test. That campaign is where "never silently wrong" stopped being a
slogan and became a property with guards on it.

### Beth, and the discovery that the screen is part of the answer

The user got a name and a bar: careful, non-technical, evidence-minded, and the
correctness and usability standard is set for her. That produced a second cardinal
rule, for the screen rather than the figures: **never ask a question the tool can
answer, and never ask one the person in front of you cannot.** Two templates
fitting equally well is not a question for an accountant — pick the tested one,
convert, and flag it. Whole screens were rewritten around it. Nothing was deleted,
only re-homed: every diagnostic is still one click away, because some people want
all of it. The default just stopped assuming everybody does.

### From statements to documents, and from files to cases

Two widenings landed together. Excel became a first-class path, and a key-value
mode was added for documents that have no transaction table at all — KiwiSaver
summaries, IRD-style forms, letters — with the same front door deciding which kind
of document it is looking at. At the same time the unit of work grew: a case
arrives as a folder of ten to fifty statements, so conversion became a batch with
one row per file, and a single upload holding several statements is split at
boundaries it can prove, with each statement reconciled on its own anchors.

### Reviewing itself became part of the build

Somewhere in the last stretch the process changed. Every round now starts by
attacking the thing: driving the real app against real files, reading the code
against its own claims, and recording each finding with its evidence and its fix
in a register that outlives the release. It caught real wrong figures — a credit
read as a debit, a printed balance kept as a transaction, a footer folded into the
row above. It also caught the quieter class: sentences on screen that claimed more
than the check beneath them proved. Those were treated as defects too, because a
green tick nobody earned is exactly the failure this tool exists to prevent.

### Where it stands

The engine is finished in the sense that matters: the next steps do not add code.
Watch a real analyst build a template on a real statement. Prove the install on a
real locked-down box. Add layouts as real files arrive. Everything the product
still needs to grow, it grows as a template — which was the whole argument at the
start, and is the one thing that has not changed since.
