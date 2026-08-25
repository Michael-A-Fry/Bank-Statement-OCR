# The corpus survey

Point the document engine (`mode: document` — `R/tables.R`, `R/tables_detect.R`,
`R/doc_extract.R`) at a folder of **real PDFs that nobody here wrote**, and report
what it does with each one.

## Why

Every fixture under `tests/testthat/` was written by somebody who already knew how
the engine works, and that is the one thing a fixture cannot fix about itself. The
adversarial fixtures in `helper-doc-hard.R` are better — each is a shape a real
report uses — but they are still *our* idea of what is awkward.

A folder of documents assembled by other people, for other extractors, is the only
way to find out what this engine does when it meets a shape nobody here thought
of. It found three defects that no fixture here could have (findings **N169–N172**
in `docs/context/findings-register.md`), including one where 133 words fell out of
every column on a page and the row count silently changed.

## What it is not

It is **not a test**, and it has no expected answers, because there are none:
nobody has said what the right tables are on somebody else's PDF. It is a
**survey**. It reports, per document:

* whether the engine crashed (it must never — a crash is a bug, always)
* how many tables and values were proposed, and how many rows came out
* whether the proposer's row count is the reader's row count — a disagreement
  means the template a person confirms on screen is not the template that makes
  their file
* how many words inside a table's boundary no column claimed
* how long it took

## The documents are not in this repository

They are third-party files under their own licences, and this repository does not
carry other people's PDFs. Only the *measurements* are kept — in the findings
register and the changelog. Assemble a folder yourself:

```sh
python3 tools/corpus/fetch-corpus.py     # needs network; writes ./corpus
```

It collects PDFs from the test suites of **camelot**, **pdfplumber**,
**tabula-java** and **tabula-py** — which are exactly the documents that broke
somebody else's extractor, so they carry the awkward shapes on purpose: spanning
cells, ruled and unruled tables, rotated pages, multi-line rows, several tables to
a page, forms, pages that are one big image with no text layer at all. Any folder
of PDFs works; that one is just a good one.

## Running it

```sh
Rscript tools/corpus/run-corpus.R /path/to/pdfs [report.csv]
```

Needs a real R with `pdftools`. It prints a per-document line and a summary, and
writes a CSV you can sort.

The measurement that matters most is the last-but-one line:

```
proposer and reader disagree on rows   0
```

That was **40 of 81** before N169–N172 and is **0** after. If it is not 0, start
there.

## Not shipped

This is a development tool. It is not part of the offline bundle
(`scripts/bundle-offline.R`), it is never called by the app, and it needs a folder
of documents that the deployment does not have.
