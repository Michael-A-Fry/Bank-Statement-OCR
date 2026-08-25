#!/usr/bin/env python3
"""Collect REAL documents with tables, from the test suites of open-source
table-extraction projects.

Why these: they are exactly the documents that broke somebody else's extractor,
so they carry the awkward shapes on purpose -- spanning cells, ruled and unruled
tables, rotated pages, multi-line rows, footers, several tables to a page.

The GitHub API is scoped in this session but raw.githubusercontent.com is not, so
the filenames are discovered from each project's own test sources.

Nothing here is committed: these are third-party files under their own licences,
and this repository does not carry other people's PDFs. They are downloaded to a
scratch folder, measured, and the MEASUREMENTS are what is kept.
"""
import os, re, sys, urllib.request, concurrent.futures as cf

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "corpus")
os.makedirs(OUT, exist_ok=True)
RAW = "https://raw.githubusercontent.com"

# repo, branch, folder the pdfs live in, files whose source names them
SOURCES = [
    ("camelot-dev/camelot", "master", "tests/files",
     ["tests/test_common.py", "tests/test_errors.py", "tests/data.py",
      "tests/test_cli.py"]),
    ("jsvine/pdfplumber", "stable", "tests/pdfs",
     ["tests/test_basics.py", "tests/test_table.py", "tests/test_issues.py",
      "tests/test_ctm.py", "tests/test_convert.py"]),
    ("tabulapdf/tabula-java", "master", "src/test/resources/technology/tabula",
     ["src/test/java/technology/tabula/TestBasicExtractor.java",
      "src/test/java/technology/tabula/TestSpreadsheetExtractor.java",
      "src/test/java/technology/tabula/TestTableDetection.java"]),
    ("chezou/tabula-py", "master", "tests/resources",
     ["tests/test_read_pdf_table.py"]),
    ("atlanhq/camelot", "master", "tests/files", ["tests/test_camelot.py"]),
]

def get(url, timeout=45):
    req = urllib.request.Request(url, headers={"User-Agent": "corpus/1.0"})
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return r.read()

def discover(repo, branch, folder, srcs):
    names = set()
    for s in srcs:
        try:
            txt = get(f"{RAW}/{repo}/{branch}/{s}").decode("utf-8", "replace")
        except Exception:
            continue
        for m in re.findall(r'([A-Za-z0-9._\-]+\.pdf)', txt):
            names.add(m)
    return [(repo, branch, folder, n) for n in sorted(names)]

def fetch(job):
    repo, branch, folder, name = job
    dest = os.path.join(OUT, f"{repo.split('/')[1]}__{name}")
    if os.path.exists(dest) and os.path.getsize(dest) > 1000:
        return dest, "cached"
    try:
        data = get(f"{RAW}/{repo}/{branch}/{folder}/{name}")
    except Exception as e:
        return None, f"{name}: {type(e).__name__}"
    if not data.startswith(b"%PDF"):
        return None, f"{name}: not a pdf"
    with open(dest, "wb") as f:
        f.write(data)
    return dest, "ok"

jobs = []
for repo, branch, folder, srcs in SOURCES:
    found = discover(repo, branch, folder, srcs)
    print(f"{repo}: {len(found)} candidate names", flush=True)
    jobs += found

print(f"total candidates: {len(jobs)}", flush=True)
ok = 0
with cf.ThreadPoolExecutor(max_workers=6) as ex:
    for dest, why in ex.map(fetch, jobs):
        if dest:
            ok += 1
            if why == "ok":
                print(f"  got {os.path.basename(dest)} "
                      f"({os.path.getsize(dest)//1024}K)", flush=True)
print(f"\nDOWNLOADED {ok} PDFs into {OUT}", flush=True)
