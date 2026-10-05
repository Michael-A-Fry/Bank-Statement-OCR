#!/usr/bin/env python3
"""kbwp_common.py -- shared helpers for the two QVF-lookalike generators of area
"kbwp" (Kiwibank PDF transaction enquiry, Westpac credit card).

Reuses tools/synth/make_layouts.py (imported, never edited): its Sheet class
(drawing with the collision check), money formatting, the invented NAMES and
write_json, so the truth files are written in exactly make_layouts.py's format.

Everything printed is invented. Bank names are real (for identification only);
people, addresses, account digits and merchants are made up.
"""

import datetime as dt
import json
import os
import random
import re
import sys
import zlib

SYNTH = "/home/user/Bank-Statement-OCR/tools/synth"
if SYNTH not in sys.path:
    sys.path.insert(0, SYNTH)

import make_layouts as ML                      # noqa: E402  (read-only reuse)
from make_layouts import Sheet, GenError, MON, NAMES, mag, write_json, cents  # noqa: E402,F401

HERE = os.path.dirname(os.path.abspath(__file__))
QVF = os.path.dirname(HERE)
OUT_DIR = os.path.join(QVF, "sets", "kbwp")
BANNER = "Synthetic test document - not a real statement"
SEED0 = 20261005


def rng_for(name):
    return random.Random(SEED0 + zlib.crc32(name.encode("utf-8")))


def daterange(a, b):
    d = a
    while d <= b:
        yield d
        d += dt.timedelta(days=1)


def pick_dates(rng, start, end, n):
    days = list(daterange(start, end))
    return sorted(rng.choice(days) for _ in range(n))


def fill(rng, tmpl, extra=None):
    """Fill {name} placeholders from make_layouts' invented NAMES, plus a few of ours."""
    extra = extra or {}

    def sub(m):
        k = m.group(1)
        if k in extra:
            v = extra[k]
            return rng.choice(v) if isinstance(v, (list, tuple)) else v
        if k in NAMES:
            return rng.choice(NAMES[k])
        if k == "time":
            return "%02d:%02d" % (rng.randint(6, 22), rng.randint(0, 59))
        if k == "ref":
            return str(rng.randint(100, 9999))
        if k == "inv":
            return "INV%d" % rng.randint(10000, 99999)
        if k == "custno":
            return str(rng.randint(10 ** 6, 10 ** 8 - 1))
        raise GenError("unknown placeholder {%s}" % k)
    return re.sub(r"\{(\w+)\}", sub, tmpl)


def chain_check(name, rows, opening, closing, newest):
    """The truth's own arithmetic, as make_layouts.chain_check reads it."""
    seq = rows[::-1] if newest else rows
    bal = cents(opening)
    for t in seq:
        d, c = cents(t["debit"]), cents(t["credit"])
        if (d is None) == (c is None):
            raise GenError("%s: a row needs exactly one of debit/credit" % name)
        if (d is not None and d < 0) or (c is not None and c < 0):
            raise GenError("%s: a negative debit or credit" % name)
        bal = bal - (d or 0) + (c or 0)
        if t["balance"] is not None and cents(t["balance"]) != bal:
            raise GenError("%s: printed balance %s, arithmetic says %s"
                           % (name, t["balance"], bal / 100.0))
        if not re.fullmatch(r"\d{4}-\d\d-\d\d", t["date"]):
            raise GenError("%s: bad date %r" % (name, t["date"]))
    if bal != cents(closing):
        raise GenError("%s: rows do not reach the closing balance (%s vs %s)"
                       % (name, bal / 100.0, closing))


def truth_doc(case, generator, note, bank, layout, product, account_bank_code,
              account_number, features, row_order, opening, closing, rows,
              statements=None):
    t = {
        "case": case,
        "generator": generator,
        "note": note,
        "bank": bank,
        "layout": layout,
        "product": product,
        "source_format": "pdf",
        "account_bank_code": account_bank_code,
        "account_number": account_number,
        "account_redaction": None,
        "features": features,
        "row_order": row_order,
        "opening_balance": opening,
        "closing_balance": closing,
        "removed_rows": 0,
        "row_count": len(rows),
        "rows": rows,
    }
    if statements is not None:
        t["statements"] = statements
    return t


# ---------------------------------------------------------------------------
# A token-stream reader that mimics how the QVF's PDF connector ("Mole") hands
# words to the script: every whitespace-separated word, in reading order (page,
# then line top to bottom, then left to right). Used only to SELF-CHECK that a
# lookalike carries the landmarks the QVF section needs.
# ---------------------------------------------------------------------------

def mole_words(pdf_path, line_tol=2.0):
    import pymupdf
    doc = pymupdf.open(pdf_path)
    out = []
    for pno, page in enumerate(doc):
        ws = page.get_text("words")          # x0, y0, x1, y1, word, block, line, wno
        ws = sorted(ws, key=lambda w: (round(w[3], 0), w[0]))
        lines = []
        for w in ws:
            if lines and abs(lines[-1][0] - w[3]) <= line_tol:
                lines[-1][1].append(w)
            else:
                lines.append([w[3], [w]])
        for _, lw in lines:
            for w in sorted(lw, key=lambda w: w[0]):
                out.append(w[4])
    doc.close()
    return out


def write_index(out_dir, entries, fname):
    with open(os.path.join(out_dir, fname), "w") as f:
        json.dump(entries, f, indent=1)
