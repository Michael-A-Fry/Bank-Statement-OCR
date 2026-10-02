#!/usr/bin/env python3
"""make_bench.py -- big statements, for measuring how the engine scales.

The scored corpus (make_corpus.py) answers "is it RIGHT". This answers "does it
FINISH, and in how long" -- a different question, and the one that decides whether a
real job is usable. Before this existed the largest statement ever put through the
engine was NINE PAGES; the statements this tool is actually for run to a hundred and
more.

Each file reconciles exactly like a corpus case and carries the same `.truth.json`, so
a benchmark run also proves the engine stayed CORRECT at size -- a fast wrong answer
is not a result.

    python3 tools/synth/make_bench.py --out /tmp/bench               # 30/100/200 pages
    python3 tools/synth/make_bench.py --out /tmp/bench --pages 400   # one big one
    Rscript  tools/synth/bench.R /tmp/bench                          # the timings

Dev-time only. Nothing here ships to the server, which runs R and poppler alone.
"""
import argparse
import json
import os
import sys

MIN_PYTHON = (3, 9)
if sys.version_info < MIN_PYTHON:
    sys.exit("make_bench.py needs Python %d.%d or newer" % MIN_PYTHON)

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import make_corpus as mc            # noqa: E402  (after the version gate, deliberately)

ROWS_PER_PAGE = 30                  # draw_statement's default

# The sizes worth measuring, and why these:
#   30   a month of a busy account -- the ordinary job, for a baseline to compare to
#   100  the size the tool is actually for, and the one nothing had ever been run at
#   200  a year of a very busy account, or two statements bundled
#   400  the ceiling test: if this finishes, nothing a bank issues will not
DEFAULT_PAGES = (30, 100, 200)


def build_bench(pages, out_dir, seed=90210):
    """One statement of `pages` pages, drawn exactly as a corpus case is."""
    name = "bench_%03dp" % pages
    n = pages * ROWS_PER_PAGE
    # day_step=0 packs several transactions onto one day, which is how a busy account
    # really prints thousands of rows. Stepping a day or more per row would make a
    # 100-page statement span 25 YEARS, and then the date and period checks would be
    # measuring an impossible document instead of the engine.
    mc.build(name, "%d pages, %d rows -- scale benchmark" % (pages, n),
             out_dir, seed=seed, n=n, day_step=0)
    pdf = os.path.join(out_dir, name + ".pdf")
    return name, pdf, os.path.getsize(pdf)


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out", required=True, help="where to write the PDFs and truth files")
    ap.add_argument("--pages", type=int, nargs="*", default=None,
                    help="page counts to generate (default 30 100 200)")
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    sizes = a.pages if a.pages else list(DEFAULT_PAGES)
    index = []
    for p in sizes:
        name, pdf, nbytes = build_bench(p, a.out)
        truth = json.load(open(os.path.join(a.out, name + ".truth.json")))
        print("%-14s %4d pages  %6d rows  %8.1f KB" %
              (name, p, len(truth["rows"]), nbytes / 1024.0))
        index.append({"case": name, "pages": p, "rows": len(truth["rows"]),
                      "bytes": nbytes})
    json.dump(index, open(os.path.join(a.out, "bench_index.json"), "w"), indent=2)
    print("\n%d benchmark statement(s) in %s" % (len(index), a.out))


if __name__ == "__main__":
    main()
