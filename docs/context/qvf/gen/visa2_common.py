#!/usr/bin/env python3
"""visa2_common.py -- shared helpers for the area-visa2 lookalike generators
(make_bnz_visa.py, make_kiwibank_cc.py).

What is shared:
  * drawing: tools/synth/make_layouts.py's Sheet (collision and off-page checks),
    its money printing (mag), its description filler (fill / amount_for) and its
    invented merchant names. Imported, never copied.
  * the answer key: written in EXACTLY make_layouts.py's truth format (build_case),
    checked with its own chain_check before it is written.
  * a QVF REPLAY: the PDF's words are read back in reading order and walked with
    the Qlik script's own landmark rules for the type (BNZ Visa, lines 5472-5994;
    Kiwibank Credit Card, lines 5995-6496; shared subs sBalances, sFindDeposits,
    sLoopToDate). That proves each lookalike is laid out the way the script
    expects, and shows what the script itself would make of it (the keyword
    deposit list and the subset-sum solver included).

Everything printed is invented. Dev-time only, nothing here ships.
"""

import datetime as dt
import itertools
import json
import os
import random
import re
import sys
import zlib

REPO = "/home/user/Bank-Statement-OCR"
sys.path.insert(0, os.path.join(REPO, "tools", "synth"))
import make_layouts as ML  # noqa: E402

from make_layouts import Sheet, GenError, SYNTHETIC, MON, MONTH  # noqa: E402,F401

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_OUT = os.path.normpath(os.path.join(HERE, "..", "sets", "visa2"))
A4 = ML.PAGES["A4"]
PW, PH = A4


def seeded(key):
    return random.Random(zlib.crc32(key.encode("utf-8")))


def dollars(c):
    """|c| cents as $1,234.56."""
    return ML.mag(c, thousands=True, dollar=True)


def card_balance(owed):
    """A card balance as printed: what is owed plain, a credit balance with CR."""
    if owed < 0:
        return dollars(-owed) + " CR"
    return dollars(owed)


def long_date(d):
    return "%d %s %d" % (d.day, MONTH[d.month - 1], d.year)


def fill(rng, tmpl, vals):
    return ML.fill(rng, tmpl, None, vals)


def amount_for(rng, spec, vals):
    return ML.amount_for(rng, spec, vals, 1.0)


def add_months(d, k):
    return ML.add_months(d, k)


# ---------------------------------------------------------------------------
# A banner as a picture: on a page break that the Qlik script walks straight
# through (Kiwibank's "Continued over Page 1 of 2" is followed directly by the
# next page's first row), any printed word would land inside a transaction. The
# banner is still on every page, but as ink, not text.
# ---------------------------------------------------------------------------

_BANNER_PNG = os.path.join(HERE, "_synthetic_banner.png")


def banner_png():
    if not os.path.exists(_BANNER_PNG):
        from PIL import Image, ImageDraw, ImageFont
        font = ImageFont.load_default(size=28)
        img = Image.new("L", (1100, 40), 255)
        ImageDraw.Draw(img).text((8, 4), SYNTHETIC, fill=0, font=font)
        img.save(_BANNER_PNG, optimize=False)
    return _BANNER_PNG


def draw_banner_image(sh, y_top):
    """The SYNTHETIC banner as an image (no text layer), centred, 230 x 8.4 pt."""
    w, h = 230.0, 230.0 * 40 / 1100
    sh.c.drawImage(banner_png(), (PW - w) / 2.0, PH - y_top - h, width=w, height=h)


def draw_banner_text(sh, y):
    sh.text(PW / 2.0, y, SYNTHETIC, 7, bold=True, align="center")


# ---------------------------------------------------------------------------
# The answer key.
# ---------------------------------------------------------------------------

def truth_row(r, stmt_index=None):
    t = {"date": r["date"].isoformat(), "description": r["desc"],
         "debit": r["amt"] / 100.0 if r["dir"] == "D" else None,
         "credit": r["amt"] / 100.0 if r["dir"] == "C" else None,
         "balance": None}
    if stmt_index is not None:
        t["statement_index"] = stmt_index
    return t


def write_truth(out_dir, name, *, note, bank, layout, product, features, newest,
                statements, generator):
    """statements: list of dicts {rows, opening_owed, closing_owed, start, end}.
    A card's opening/closing in the key is the amount owed NEGATED."""
    rows = []
    multi = len(statements) > 1
    for j, st in enumerate(statements):
        rows += [truth_row(r, j if multi else None) for r in st["rows"]]
    truth = {
        "case": name,
        "generator": generator,
        "note": note,
        "bank": bank,
        "layout": layout,
        "product": product,
        "source_format": "pdf",
        "account_bank_code": None,
        "account_number": None,
        "account_redaction": None,
        "features": features,
        "row_order": "newest_first" if newest else "oldest_first",
        "opening_balance": -statements[0]["opening_owed"] / 100.0,
        "closing_balance": -statements[-1]["closing_owed"] / 100.0,
        "removed_rows": 0,
        "row_count": len(rows),
        "rows": rows,
    }
    if multi:
        truth["statements"] = [
            {"statement_index": j, "period_start": st["start"].isoformat(),
             "period_end": st["end"].isoformat(),
             "opening_balance": -st["opening_owed"] / 100.0,
             "closing_balance": -st["closing_owed"] / 100.0}
            for j, st in enumerate(statements)]
    # The truth's own arithmetic, read the way the scorer reads it.
    ML.chain_check(name, truth["rows"], truth["opening_balance"], truth["closing_balance"],
                   newest, [False] * len(truth["rows"]))
    for j in range(1, len(statements)):
        if statements[j]["opening_owed"] != statements[j - 1]["closing_owed"]:
            raise GenError("%s: bundled statements do not chain" % name)
    ML.write_json(os.path.join(out_dir, name + ".truth.json"), truth)
    return truth


def settle(st):
    """Closing owed from opening owed and the rows (purchases add, credits take off)."""
    owed = st["opening_owed"]
    for r in st["rows"]:
        owed += r["amt"] if r["dir"] == "D" else -r["amt"]
    st["closing_owed"] = owed
    st["tot_d"] = sum(r["amt"] for r in st["rows"] if r["dir"] == "D")
    st["tot_c"] = sum(r["amt"] for r in st["rows"] if r["dir"] == "C")
    return st


# ---------------------------------------------------------------------------
# Reading the PDF back the way a word-position connector hands it over: every
# word, page by page, line by line (top to bottom), left to right.
# ---------------------------------------------------------------------------

def words_in_order(pdf):
    import pymupdf
    out = []
    with pymupdf.open(pdf) as doc:
        for page in doc:
            ws = page.get_text("words")
            ws = sorted(ws, key=lambda w: ((w[1] + w[3]) / 2.0, w[0]))
            lines = []
            for w in ws:
                ym = (w[1] + w[3]) / 2.0
                if lines and abs(ym - lines[-1][0]) < 2.5:
                    lines[-1][1].append(w)
                else:
                    lines.append([ym, [w]])
            for _, lw in lines:
                out += [w[4] for w in sorted(lw, key=lambda w: w[0])]
    return out


MONTHS = set(MON)
_MONEY = re.compile(r"^\$?\d{1,3}(,\d{3})*\.\d\d$|^\$?\d+\.\d\d$")


def qlik_money(s):
    """What Qlik's automatic interpretation makes of an amount string under the
    script's MoneyFormat ($#,##0.00): a number, or None (a text value, which Sum()
    ignores)."""
    if s is None or not _MONEY.match(s):
        return None
    return int(s.replace("$", "").replace(",", "").replace(".", ""))


def qvf_balance(tokens):
    """sBalances: the tokens of one balance, $ removed, DR dropped; a CR keeps it
    positive, anything else is owed and made negative (0.00 stays 0.00)."""
    toks = [t[1:] if t.startswith("$") else t for t in tokens if t != "DR"]
    s = " ".join(toks)
    if "CR" in s:
        s = s[:-3] if (" CR" in s or ",CR" in s) else s[:-2]
        v = qlik_money(s.strip())
        return v
    v = qlik_money(s)
    if v is None:
        return None
    return v if s == "0.00" else -v


def qvf_find_deposits(rows, opening, closing, exclude_exact=(), limit=2_000_000):
    """sFindDeposits for one statement. rows: dicts with amt (cents or None),
    deposit (bool), details. Fills r['qvf'] with '+', '-' or 'U' (Unidentified).
    Returns a short verdict string."""
    known = [r for r in rows if r["amt"] is not None]
    if opening is None or closing is None:
        for r in rows:
            r["qvf"] = "+" if r["deposit"] else "-"
        return "no balance read"
    dep = sum(r["amt"] for r in known if r["deposit"])
    unk = sum(r["amt"] for r in known if not r["deposit"])
    s = dep - unk
    num = opening + s - closing
    if num % 2:
        # the script divides by -2 and compares with the cents of the candidates
        diff = num / -2.0
    else:
        diff = num // -2
    for r in rows:
        r["qvf"] = "+" if r["deposit"] else "-"
    if diff == 0:
        return "balanced with keyword deposits"
    cands = [r for r in rows if not r["deposit"] and r["amt"] is not None
             and r["amt"] <= diff and r["details"] not in exclude_exact]
    cands.sort(key=lambda r: -r["amt"])
    sols, tried, timeout = [], 0, False
    for k in range(1, 7):
        for combo in itertools.combinations(range(len(cands)), k):
            tried += 1
            if tried > limit:
                timeout = True
                break
            if sum(cands[i]["amt"] for i in combo) == diff:
                sols.append(combo)
        if timeout:
            break
    if timeout:
        for r in cands:
            r["qvf"] = "U"
        return "solver timed out: %d candidates left Unidentified" % len(cands)
    if len(sols) == 1:
        for i in sols[0]:
            cands[i]["qvf"] = "+"
        return "solver found the one set of credits (%d row(s))" % len(sols[0])
    if len(sols) > 1:
        for combo in sols:
            for i in combo:
                cands[i]["qvf"] = "U"
        return "solver found %d sets: those rows left Unidentified" % len(sols)
    return "solver found nothing (difference %s): rows kept as keyword says, balance check fails" % diff


def compare_qvf(truth_rows, qvf_rows, sort=False):
    """How the script's reading compares with the key: counts of rows right
    (date and signed amount), wrong sign, Unidentified, unreadable, missing."""
    def want(t):
        return (t["date"], int(round((t["credit"] or 0) * 100 - (t["debit"] or 0) * 100)))
    W = [want(t) for t in truth_rows]
    G = []
    for r in qvf_rows:
        if r["amt"] is None:
            G.append((r["date"], None, r["qvf"]))
        else:
            G.append((r["date"], r["amt"], r["qvf"]))
    if sort:
        W = sorted(W)
        G = sorted(G, key=lambda g: (g[0] or "", g[1] or 0))
    res = {"rows": len(W), "read": len(G), "right": 0, "wrong_sign": 0, "unidentified": 0,
           "unreadable": 0, "date_wrong": 0}
    pool = list(G)
    for d, a in W:
        hit = None
        for k, g in enumerate(pool):
            if g[1] == abs(a) and g[0] == d:
                hit = k
                break
        if hit is None:
            for k, g in enumerate(pool):
                if g[1] == abs(a):
                    hit = k
                    res["date_wrong"] += 1
                    break
        if hit is None:
            continue
        g = pool.pop(hit)
        if g[2] == "U":
            res["unidentified"] += 1
        elif (g[2] == "+") == (a > 0):
            res["right"] += 1
        else:
            res["wrong_sign"] += 1
    res["unreadable"] = sum(1 for g in pool if g[1] is None)
    res["extra"] = len(pool) - res["unreadable"]
    res["missing"] = len(W) - (res["right"] + res["wrong_sign"] + res["unidentified"])
    return res


def save_report(out_dir, name, report):
    """The replay goes outside the set, so no scorer ever reads it."""
    d = os.path.join(HERE, "..", "visa2_debug", "qvf_replay")
    os.makedirs(d, exist_ok=True)
    with open(os.path.join(d, name + ".qvf_replay.json"), "w") as f:
        json.dump(report, f, indent=1, sort_keys=True)
