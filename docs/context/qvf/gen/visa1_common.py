"""visa1_common.py -- shared pieces for the visa1 lookalike generators
(make_anz_visa.py, make_asb_visa.py).

Reuses tools/synth/make_layouts.py for drawing (its Sheet: every string recorded,
collisions and off-page strings refused), money printing and the truth-file
writer, so the truth files are in exactly that generator's format.

All data is invented: merchants come from make_layouts.NAMES, people and card
digits are made up, and every page carries the SYNTHETIC banner.
"""

import datetime as dt
import json
import os
import random
import re
import sys
import zlib

REPO = "/home/user/Bank-Statement-OCR"
sys.path.insert(0, os.path.join(REPO, "tools", "synth"))
import make_layouts as ML  # noqa: E402

try:
    import pymupdf as fitz  # noqa: E402
except ImportError:          # older PyMuPDF
    import fitz              # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
QVF = os.path.dirname(HERE)
SET_DIR = os.path.join(QVF, "sets", "visa1")

MON = ML.MON
MON_SET = set(MON)
W, H = ML.A4


class GenError(Exception):
    pass


# ---------------------------------------------------------------------------
# Dates and money
# ---------------------------------------------------------------------------

def ddmon(d):
    return "%02d %s" % (d.day, MON[d.month - 1])


def ddmonyy(d):
    return "%02d %s %02d" % (d.day, MON[d.month - 1], d.year % 100)


def add_months(d, k):
    return ML.add_months(d, k)


def card_money(c, thousands=True, dollar=False):
    """A holder-side signed figure (negative = owed / a purchase) printed the way a
    card prints it: owed plain, in credit (or a payment) with CR.
    Returns (number, token)."""
    m = ML.mag(c, thousands=thousands, dollar=dollar)
    return m, ("CR" if c > 0 else "")


def joined(num, tok):
    return num + (" " + tok if tok else "")


def parse_card(num, tok):
    """Read a card figure back, independently of card_money."""
    s = num.replace("$", "")
    if not re.fullmatch(r"\d{1,3}(,\d{3})*\.\d\d|\d+\.\d\d", s):
        raise GenError("figure %r does not parse" % num)
    c = int(s.replace(",", "").replace(".", ""))
    if tok not in ("", "CR"):
        raise GenError("token %r" % tok)
    return c if tok == "CR" else -c


# ---------------------------------------------------------------------------
# What the rows say. Invented merchants (make_layouts.NAMES); amounts in cents,
# holder side: a purchase is negative, a payment or refund positive.
# ---------------------------------------------------------------------------

NAMES = ML.NAMES
FX_RATE = {"USD": (5600, 6300), "AUD": (8800, 9300), "GBP": (4400, 4800)}
FX_CITY = {"USD": ["SAN FRANCISCO", "SEATTLE", "LOS GATOS"], "AUD": ["SYDNEY", "MELBOURNE"],
           "GBP": ["LONDON"]}

PEOPLE = [
    ("MR P TESTER", ["7 PLACEHOLDER LANE", "RD 2", "MOCKBURN 9310"]),
    ("MS A EXAMPLE", ["FLAT 3", "45 SPECIMEN ROAD", "DEMOTOWN 7020"]),
    ("J SAMPLE", ["12 EXAMPLE STREET", "SAMPLEVILLE", "TESTBURY 9010"]),
    ("R DEMO", ["101 TEMPLATE TERRACE", "ILLUSTRATION HEIGHTS", "TESTBURY 9011"]),
]
SECOND = ["MS K SAMPLE", "MR B EXAMPLE", "MS C TESTER"]


def purchase(rng, big=False):
    """One purchase: (description, cents)."""
    k = "airline" if big else rng.choices(["grocer", "fuel", "cafe", "online", "utility", "telco"],
                                          weights=[6, 3, 4, 3, 1, 1])[0]
    city = rng.choice(NAMES["city"])
    if k == "airline":
        return ("%s %s TKT %d" % (rng.choice(NAMES["airline"]), city, rng.randint(10000, 99999)),
                rng.randint(110000, 165000))
    if k == "utility":
        return ("%s %s" % (rng.choice(NAMES["utility"]), city), rng.randint(6000, 34000))
    if k == "telco":
        return ("%s %s" % (rng.choice(NAMES["telco"]), city), rng.randint(3500, 19000))
    lo, hi = {"grocer": (1200, 32000), "fuel": (3500, 16000), "cafe": (400, 4000),
              "online": (600, 12000)}[k]
    return "%s %s" % (rng.choice(NAMES[k]), city), rng.randint(lo, hi)


def fx_purchase(rng, cur=None):
    """A purchase in a foreign currency: (description, fx text, nz cents, charge cents).
    The NZ$ amount INCLUDES the conversion charge, as the ANZ line says ("Incl")."""
    cur = cur or rng.choice(["USD", "USD", "AUD", "GBP"])
    fx = rng.randint(800, 24000)
    rate = rng.randint(*FX_RATE[cur])
    nz = int(round(fx * 10000.0 / rate))
    charge = max(1, int(round(nz * 0.025)))
    desc = "%s %s" % (rng.choice(NAMES["online"]), rng.choice(FX_CITY[cur]))
    return desc, "%s %s" % (cur, ML.mag(fx, thousands=False)), nz + charge, charge


def interest_line(rng):
    return "INTEREST CHARGED ON PURCHASES", rng.randint(180, 4200)


# ---------------------------------------------------------------------------
# Reading order: what a word-by-word PDF connector would hand over.
# ---------------------------------------------------------------------------

def reading_words(pdf_path):
    """Every word, page by page, top to bottom then left to right (a line is the
    words whose baselines are within 2pt). Returns [(page, y, x, word)]."""
    out = []
    doc = fitz.open(pdf_path)
    for pno, page in enumerate(doc):
        ws = page.get_text("words")
        ws = sorted(ws, key=lambda w: (round(w[3], 1), w[0]))
        lines = []
        for w in ws:
            if lines and abs(lines[-1][0] - w[3]) <= 2.0:
                lines[-1][1].append(w)
            else:
                lines.append([w[3], [w]])
        for y, items in lines:
            for w in sorted(items, key=lambda w: w[0]):
                out.append((pno, y, w[0], w[4]))
    doc.close()
    return out


def page_lines(pdf_path):
    """[(page, y, [(x0, x1, word), ...])] in reading order."""
    out = []
    doc = fitz.open(pdf_path)
    for pno, page in enumerate(doc):
        ws = sorted(page.get_text("words"), key=lambda w: (round(w[3], 1), w[0]))
        lines = []
        for w in ws:
            if lines and abs(lines[-1][1] - w[3]) <= 2.0:
                lines[-1][2].append((w[0], w[2], w[4]))
            else:
                lines.append([pno, w[3], [(w[0], w[2], w[4])]])
        for ln in lines:
            ln[2].sort()
            out.append(tuple(ln))
    doc.close()
    return out


def find_seq(words, seq, start=0):
    """Index of the first run of `seq` (exact words) in words[start:], or -1."""
    n = len(seq)
    for i in range(start, len(words) - n + 1):
        if words[i:i + n] == seq:
            return i
    return -1


# ---------------------------------------------------------------------------
# Truth, in make_layouts.py's format.
# ---------------------------------------------------------------------------

def truth_row(r, stmt_index=None):
    amt = r["amt"]
    t = {"date": r["date"].isoformat(), "description": r["desc"],
         "debit": (-amt) / 100.0 if amt < 0 else None,
         "credit": amt / 100.0 if amt > 0 else None,
         "balance": r["bal"] / 100.0 if r.get("bal_printed") else None}
    if r.get("redact"):
        t["description"] = None
        t["debit"] = t["credit"] = None
        t["redacted"] = ["description", "amount"]
    if stmt_index is not None:
        t["statement_index"] = stmt_index
    return t


def write_truth(path, obj):
    ML.write_json(path, obj)


def seed_for(name):
    return 20261005 + zlib.crc32(name.encode("utf-8"))


def check_chain(name, opening, rows, closing, off_table=0):
    """opening + every row (+ anything credited outside the table) = closing, and
    every printed running balance on the way."""
    bal = opening
    for r in rows:
        bal += r["amt"]
        if r.get("bal") is not None and r["bal"] != bal:
            raise GenError("%s: balance drift at %s" % (name, r["desc"]))
    if bal + off_table != closing:
        raise GenError("%s: rows do not reach the closing balance (%d vs %d)"
                       % (name, bal + off_table, closing))


def money_like(s):
    return bool(re.search(r"\d+\.\d\d", s))
