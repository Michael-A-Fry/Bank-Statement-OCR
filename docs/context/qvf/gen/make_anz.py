#!/usr/bin/env python3
"""make_anz.py -- lookalike statements for the QVF's two ANZ PDF types.

    python3 make_anz.py [--out DIR]        (default: ../sets/anz next to this file)

WHAT THIS DRAWS, and why each piece is there. Every landmark below is one the QVF
load script ("Statement Converter", tabs "ANZ" and "ANZ Loan") searches for, so a
statement drawn here is one the QVF would have been fed. The format cards in
../cards/anz.md and ../cards/anz_loan.md quote the script lines.

ANZ ("9 columns"), type key anz -- a business current / everyday account:
  * every statement starts with an "Account at a glance" box: Account name,
    Statement number, Account number, Statement period (in that order), with the
    statement date on the box's first line (the QVF takes the first statement's
    year five words after "glance");
  * the period is "dd Mon yyyy to dd Mon yyyy", or "START - dd Mon yyyy" on the
    first statement of a new account (no opening-balance row then);
  * table heading "Date | Transaction type and details | Withdrawals | Deposits |
    Balance", the details spread over five physical columns (code, other party,
    particulars, code, reference): nine columns in all;
  * year-less "dd Mon" dates; an overdrawn balance printed "1,234.56 OD";
  * "Opening balance" as a dated first row; "Totals at end of page" and
    "Balance brought forward from previous page" (or "Balance Carried Forward")
    at page breaks; "Totals at end of period"; "Your available credit is $X as at
    the closing date of this statement"; the transaction-code legend;
  * "No transactions for this period" for an empty statement;
  * a "CREDIT INTEREST PAID" row whose second line is
    "Premium interest $x Standard interest $y" (lines the QVF deletes);
  * several statements in one file, each with its own glance box.

ANZ Loan, type key anz_loan -- a home / term loan:
  * "Summary of Home Loan Number <n>" (or Term Loan Number), then "The following
    is a summary of your loan for the period <d> to <d>" (the QVF's statement
    start and period), Principal Paid, Interest Paid, Interest Owing as at,
    Next Loan Payment Amount / Date Due, Fixed Rate Review Date, Maturity Date;
  * optional "Account Name / Account Number" block, else the name is only on the
    letterhead (PO Box ... Helpline ...);
  * table "Date | Description | Withdrawals | Deposits | Principal Balance",
    dates "dd Mon yy", balances "245,000.00 DR";
  * "- processed on: dd Mon yy" second lines under a transaction;
  * "Opening Balance" or "Opening interest rate x% p.a." first row; "Rate change"
    rows with no money; "Balance Carried Forward" at the foot of a page and again
    under the next page's heading; "Closing Balance";
  * "Summary of total loan fees for the period ..." with Fee Description / Amount
    and Total Loan Fees;
  * fixed-rate expiry and interest-rate change NOTICES mixed into the file.

All names, numbers and merchants are invented. Every page says
"SYNTHETIC TEST DOCUMENT - NOT A REAL STATEMENT".

THE TRUTH FILE is exactly the format tools/synth/make_layouts.py writes (same keys,
same conventions: debit = money out, credit = money in, both positive, from the
account holder's side; a loan's balance owed is negative; balance = the figure
printed on that row, else null; opening, brought/carried forward, totals, rate
change and closing lines are NOT rows). Bundles carry `statements` and rows carry
`statement_index`. A "START" period has period_start null (an open interval).

Drawing reuses make_layouts.Sheet (its collision and off-page checks) and the
truth is arithmetic-checked with make_layouts.chain_check before it is written.
"""

import argparse
import calendar
import datetime as dt
import json
import os
import random
import sys
import zlib

REPO = "/home/user/Bank-Statement-OCR"
sys.path.insert(0, os.path.join(REPO, "tools", "synth"))
import make_layouts as ML  # noqa: E402
from make_layouts import Sheet, GenError, mag, chain_check, write_json, MON, MONTH, SYNTHETIC  # noqa: E402

GENERATOR = "scratchpad/qvf/gen/make_anz.py"
HERE = os.path.dirname(os.path.abspath(__file__))
OUT_DEFAULT = os.path.normpath(os.path.join(HERE, "..", "sets", "anz"))
A4 = ML.PAGES["A4"]
LEGAL = "ANZ Bank New Zealand Limited"
DOW = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]


def seed_of(name):
    return 20261005 + zlib.crc32(name.encode("utf-8"))


def d_mon(d):
    return "%02d %s" % (d.day, MON[d.month - 1])


def d_mon_y(d):
    return "%02d %s %d" % (d.day, MON[d.month - 1], d.year)


def d_mon_yy(d):
    return "%02d %s %02d" % (d.day, MON[d.month - 1], d.year % 100)


def d_long(d):
    return "%d %s %d" % (d.day, MONTH[d.month - 1], d.year)


def month_end(y, m):
    return dt.date(y, m, calendar.monthrange(y, m)[1])


def daterange(a, b):
    d = a
    while d <= b:
        yield d
        d += dt.timedelta(days=1)


def money(c):
    return mag(c)


def bal_parts(c, tok_neg):
    """(number, token) for a balance: the token only when overdrawn / owed."""
    return mag(c), (tok_neg if c < 0 else "")


# ---------------------------------------------------------------------------
# ANZ transaction account ("9 columns")
# ---------------------------------------------------------------------------

SZ = 7.5            # table type size
PITCH = 12.0        # one row
CPITCH = 9.0        # a continuation line inside a row
COLS = dict(date=(36, 72), type=(72, 90), party=(90, 216), part=(216, 274), code=(274, 324),
            ref=(324, 384), debit=(384, 442), credit=(442, 500), balance=(500, 559))
TEXT_CELLS = ("type", "party", "part", "code", "ref")
LEGEND = [("AP", "Automatic Payment"), ("BP", "Bill Payment"), ("DC", "Direct Credit"),
          ("ED", "Electronic Dishonour"), ("FX", "Foreign Exchange"),
          ("IP", "International EFTPOS Transaction"), ("IF", "International Payment"),
          ("AT", "Automatic Teller Machine"), ("CQ", "Cheque/Withdrawal"),
          ("DD", "Direct Debit"), ("EP", "EFTPOS Transaction"),
          ("IA", "International Money Machine"), ("VT", "Visa Transaction")]

ANZ_TX = [
    # weight, dir, code, other parties, particulars, code, reference, amount
    (8, "D", "EP", ["TOTARA FUELS", "KAURI SUPERSTORE", "MAIN ST MARKET", "HARBOUR FOODMARKET",
                    "BEAN THERE ESPRESSO"], "{card4}", "", "C {time}", (6, 260)),
    (4, "D", "AP", ["EXAMPLE RENTALS LTD", "SAMPLE PROPERTY TRUST"], "RENT", "UNIT {n1}",
     "{mon} RENT", (900, 2400)),
    (5, "D", "BP", ["KAURI ENERGY", "TASMAN POWER", "FIBRENET BROADBAND", "SAMPLE SUPPLIES LTD",
                    "DEMO FREIGHT LTD"], "ACCT {n4}", "", "INV {n5}", (60, 1800)),
    (6, "C", "DC", ["SAMPLE MOTORS", "EXAMPLE CAFE LTD", "DEMO PLUMBING", "TEST & CO LTD"],
     "INV {n4}", "", "PAYMENT", (150, 5200)),
    (3, "D", "DD", ["SOUTHERN INSURANCE", "HARBOUR LIFE COVER", "HARBOUR TELECOM"], "POLICY",
     "{n5}", "", (30, 420)),
    (2, "D", "AT", ["ANZ ATM QUEEN ST", "ANZ ATM THE MALL", "ANZ ATM LAMBTON QUAY"], "", "",
     "{time}", ("mult", 20, 400, 20)),
    (2, "D", "VT", ["WEBSHOP INTL", "CLOUDBOX STORAGE", "APPSTORE ONLINE"], "{card4}", "", "",
     (5, 320)),
    (1, "D", "CQ", ["CHEQUE"], "", "", "{chq}", (100, 2800)),
    (1, "C", "", ["DEPOSIT"], "", "", "", (100, 3000)),
]
WRAP_PARTIES = [("EXAMPLE HOLDINGS LTD", "TRUST ACCOUNT NO 2"),
                ("SAMPLE ENGINEERING", "PAYROLL CLEARING"),
                ("DEMO HEALTH SERVICES", "ACCOUNTS RECEIVABLE"),
                ("TEST LOGISTICS NZ", "FREIGHT RECOVERY")]


def fill(rng, s, d):
    out = s
    reps = {"{card4}": lambda: "****%04d" % rng.randint(0, 9999),
            "{time}": lambda: "%02d:%02d" % (rng.randint(6, 22), rng.randint(0, 59)),
            "{n1}": lambda: str(rng.randint(1, 9)),
            "{n4}": lambda: str(rng.randint(1000, 9999)),
            "{n5}": lambda: str(rng.randint(10000, 99999)),
            "{chq}": lambda: "%06d" % rng.randint(1, 999999),
            "{mon}": lambda: MON[d.month - 1].upper()}
    for k, f in reps.items():
        while k in out:
            out = out.replace(k, f(), 1)
    return out


def amount(rng, spec):
    if spec[0] == "mult":
        _, lo, hi, step = spec
        return rng.randrange(lo, hi + step, step) * 100
    return rng.randint(spec[0] * 100, spec[1] * 100)


def anz_entry(rng, want_dir=None, big=False):
    pool = [e for e in ANZ_TX if (want_dir is None or e[1] == want_dir)
            and (not big or e[2] in ("AP", "BP", "CQ"))]
    tot = sum(e[0] for e in pool)
    x = rng.uniform(0, tot)
    for e in pool:
        x -= e[0]
        if x <= 0:
            return e
    return pool[-1]


def anz_row(rng, d, e):
    _, dr, code, parties, part, code2, ref, spec = e
    cells = {"type": code, "party": rng.choice(parties), "part": fill(rng, part, d),
             "code": fill(rng, code2, d), "ref": fill(rng, ref, d)}
    return {"date": d, "dir": dr, "amt": amount(rng, spec), "cells": cells, "extra": [],
            "redact": None}


def special(d, dr, amt, party, extra=(), typ="", part="", code="", ref=""):
    return {"date": d, "dir": dr, "amt": amt, "redact": None, "extra": list(extra),
            "cells": {"type": typ, "party": party, "part": part, "code": code, "ref": ref}}


def gen_anz(rng, first_day, end, n, opening, limit=0, od_walk=False, fees=True,
            interest="credit", interest_split=False, wraps=0, money_ref=False,
            redact_dates=0, deposit_first=False, cash_fee=False):
    """One ANZ statement's rows (holder side, cents). Balances never go below
    -limit; an od_walk statement is pushed through zero both ways."""
    days = list(daterange(first_day, end))
    dates = sorted(rng.choice(days[:-1] if len(days) > 2 else days) for _ in range(n))
    rows = []
    bal = opening
    for k, d in enumerate(dates):
        if deposit_first and k == 0:
            r = special(d, "C", rng.randint(1500, 6000) * 100, "DEPOSIT")
        else:
            want = None
            if od_walk:
                # up through zero first, then down through it, then mixed
                if k < n * 0.3:
                    want = "C" if rng.random() < 0.7 else None
                elif k < n * 0.75:
                    want = "D" if rng.random() < 0.85 else None
                else:
                    want = "D" if rng.random() < 0.6 else None
            big = od_walk and want == "D" and rng.random() < 0.6
            r = anz_row(rng, d, anz_entry(rng, want, big))
        if r["dir"] == "D" and bal - r["amt"] < -limit + 6000:
            r = anz_row(rng, d, anz_entry(rng, "C"))
        if r["dir"] == "D":
            bal -= r["amt"]
        else:
            bal += r["amt"]
        rows.append(r)
    # wrapped other-party names (a second line in the details)
    cands = [r for r in rows if r["cells"]["type"] in ("DC", "AP", "BP")]
    for r in cands[:wraps]:
        a, b = rng.choice(WRAP_PARTIES)
        r["cells"]["party"] = a
        r["extra"].append(("party", b))
    if money_ref:
        r = next((x for x in rows if x["cells"]["type"] == "DC"), None)
        if r is None:
            raise GenError("no direct credit to carry a money-like reference")
        r["cells"]["part"] = "INV"
        r["cells"]["ref"] = "%d.%02d" % (rng.randint(1000, 4999), rng.randint(0, 99))
    if cash_fee:
        dep = [r for r in rows if r["cells"]["party"] == "DEPOSIT"]
        if dep:
            d = dep[0]["date"]
            rows.insert(rows.index(dep[0]) + 1, special(d, "D", 300, "CASH HANDLING FEE"))
    # month-end interest and fees
    last = end
    if interest == "credit":
        a = rng.randint(40, 1500)
        extra = []
        if interest_split:
            p = rng.randint(10, a - 10)
            extra = [("party", "Premium interest $%s" % mag(p)),
                     ("part+", "Standard interest $%s" % mag(a - p))]
        rows.append(special(last, "C", a, "CREDIT INTEREST PAID", extra=extra))
    elif interest == "debit":
        rows.append(special(last, "D", rng.randint(500, 4500), "DEBIT INTEREST"))
    if fees:
        rows.append(special(last, "D", 1250, "MONTHLY ACCOUNT FEE"))
        if rng.random() < 0.6:
            rows.append(special(last, "D", rng.randint(2, 12) * 50, "TRANSACTION FEE"))
    # balances
    bal = opening
    for r in rows:
        if r["dir"] == "D":
            r["debit"], r["credit"] = r["amt"], None
            bal -= r["amt"]
        else:
            r["debit"], r["credit"] = None, r["amt"]
            bal += r["amt"]
        r["bal"] = bal
        r["bal_printed"] = True
    if od_walk:
        sg = [r["bal"] < 0 for r in rows]
        flips = sum(1 for x, y in zip(sg, sg[1:]) if x != y)
        if flips < 2 or not sg[-1]:
            raise GenError("an OD statement must cross zero both ways and close OD "
                           "(flips %d, closes OD %s)" % (flips, sg[-1]))
    # date-only redactions (the QVF's "redacted transactions, dates not redacted")
    mids = [r for r in rows[2:-3] if r["cells"]["type"] in ("EP", "VT", "AT", "DD")]
    for r in mids[:redact_dates]:
        r["redact"] = "date_only"
    return rows, bal


def anz_desc(r):
    parts = [r["cells"][k] for k in TEXT_CELLS if r["cells"][k]]
    parts += [t for _, t in r["extra"]]
    return " ".join(parts)


def anz_row_h(r):
    return PITCH + CPITCH * sum(1 for k, _ in r["extra"] if not k.endswith("+"))


def anz_paginate(st):
    """Pages of items: ("open"|"bf"|"txn"|"none"|"ptot"|"cf", idx, y)."""
    rows = st["rows"]
    top1, top2 = 332.0, 108.0
    bottom = 762.0          # last baseline a row may use
    tail_last = 104.0       # period totals, available credit, legend
    pages, cur = [], []
    y = top1
    if st["opening_row"]:
        cur.append(("open", None, y))
        y += PITCH
    if not rows:
        cur.append(("none", None, y))
        pages.append(cur)
        st["tail_y"] = y + 18
        return pages
    for i, r in enumerate(rows):
        h = anz_row_h(r)
        last = i == len(rows) - 1
        need = h - PITCH + (tail_last if last else 16.0)
        if y + need > bottom and any(k == "txn" for k, _, _ in cur):
            cur.append(("ptot", None, y + 2))
            if st["cf_style"] == "carried":
                cur.append(("cf", None, y + 2 + PITCH))
            pages.append(cur)
            cur = [("bf", None, top2)]
            y = top2 + PITCH
        cur.append(("txn", i, y))
        y += h
    pages.append(cur)
    st["tail_y"] = y + 2
    return pages


def draw_anz_heading(sh, y):
    s = SZ
    sh.text(COLS["date"][0] + 2, y, "Date", s, bold=True)
    sh.text(COLS["type"][0] + 2, y, "Transaction type and details", s, bold=True)
    for k, h in (("debit", "Withdrawals"), ("credit", "Deposits"), ("balance", "Balance")):
        sh.text(COLS[k][1] - 3, y, h, s, bold=True, align="right")
    sh.line(36, y + 4, 559, y + 4, width=0.6)


def put_money(sh, k, y, c, bold=False):
    sh.text(COLS[k][1] - 3, y, mag(c), SZ, bold=bold, align="right")


def put_balance(sh, y, c, bold=False, size=SZ):
    tokw = sh.width("OD", size)
    xr = COLS["balance"][1] - 3
    xn = xr - tokw - 3
    num, tok = bal_parts(c, "OD")
    sh.text(xn, y, num, size, bold=bold, align="right")
    if tok:
        sh.text(xr, y, tok, size, bold=bold, align="right")
    return num, tok


def anz_first_header(sh, st):
    sh.text(36, 46, "ANZ", 22, bold=True)
    sh.text(559, 36, "Account statement", 11, bold=True, align="right")
    sh.text(559, 50, st["product"], 9, align="right")
    sh.text(297, 66, SYNTHETIC, 6.5, align="center")
    y = 92
    sh.text(36, y, st["holder"][0], 9, bold=True)
    for k, ln in enumerate(st["holder"][1]):
        sh.text(36, y + 11 * (k + 1), ln, 9)
    sh.text(330, 92, LEGAL, 7.5)
    sh.text(330, 102, "PO Box 9900, Sampleton 1142", 7.5)
    sh.text(330, 112, "anz.co.nz   0800 000 000", 7.5)
    # the glance box
    sh.rect(36, 160, 523, 136, stroke=(0.55, 0.55, 0.55), width=0.6)
    sh.text(42, 176, "Account at a glance", 10, bold=True)
    if not st.get("no_stmt_date"):
        sh.text(360, 176, "Statement date", 8.5)
        sh.text(440, 176, d_mon_y(st["end"]), 8.5)
    lab = [("Account name", st["name"]), ("Statement number", str(st["stno"])),
           ("Account number", st["acct"]), ("Statement period", st["period_text"])]
    for k, (a, b) in enumerate(lab):
        yy = 194 + 12 * k
        sh.text(42, yy, a, 8.5)
        sh.text(150, yy, b, 8.5, bold=(k == 0))
    summ = [("Opening balance", st["opening"]), ("Total withdrawals", st["tot_d"]),
            ("Total deposits", st["tot_c"]), ("Closing balance", st["closing"])]
    for k, (a, v) in enumerate(summ):
        yy = 248 + 12 * k
        sh.text(42, yy, a, 8.5)
        num, tok = bal_parts(v, "OD") if a in ("Opening balance", "Closing balance") else (mag(v), "")
        sh.text(262, yy, num, 8.5, align="right")
        if tok:
            sh.text(268, yy, tok, 8.5)


def anz_cont_header(sh, st, pno, npg):
    sh.text(36, 40, "ANZ", 16, bold=True)
    sh.text(559, 36, "Account number %s   Statement number %d" % (st["acct"], st["stno"]), 7.5,
            align="right")
    sh.text(297, 56, SYNTHETIC, 6.5, align="center")


def anz_footer(sh, st, pno, npg):
    sh.text(36, 802, LEGAL, 7)
    sh.text(559, 802, "Page %d of %d" % (pno, npg), 7, align="right")
    sh.text(36, 812, "Please check this statement and tell us about anything that looks wrong "
                     "within 30 days.", 6.5)
    sh.text(297, 826, SYNTHETIC, 6.5, align="center")


def draw_anz_txn(sh, r, y):
    rec = {"date": d_mon(r["date"]), "texts": [], "money": {}}
    sh.text(COLS["date"][0] + 2, y, rec["date"], SZ)
    if r["redact"] == "date_only":
        # text removed, a black box where the rest of the line was
        sh.rect(COLS["type"][0] + 1, y - 0.85 * SZ, COLS["balance"][1] - COLS["type"][0] - 2,
                1.15 * SZ, fill=ML.BLACK)
        r["rec"] = rec
        return y
    for k in TEXT_CELLS:
        s = r["cells"][k]
        if not s:
            continue
        x0, x1 = COLS[k]
        if sh.width(s, SZ) > (x1 - x0) - 4:
            raise GenError("%r does not fit the %s column" % (s, k))
        sh.text(x0 + 2, y, s, SZ)
        rec["texts"].append(s)
    if r["debit"] is not None:
        put_money(sh, "debit", y, r["debit"])
    if r["credit"] is not None:
        put_money(sh, "credit", y, r["credit"])
    put_balance(sh, y, r["bal"])
    yy = y
    prev_x1 = None
    for k, t in r["extra"]:
        if k.endswith("+"):            # same line, after the previous continuation text
            sh.text(prev_x1 + 12, yy, t, SZ)
        else:
            yy += CPITCH
            _, prev_x1 = sh.text(COLS[k][0] + 2, yy, t, SZ)
        rec["texts"].append(t)
    r["rec"] = rec
    return yy


def render_anz(sh, st):
    pages = st["pages"]
    npg = len(pages)
    rows = st["rows"]
    last_bal = st["opening"]
    for pi, items in enumerate(pages):
        sh.begin_page()
        if pi == 0:
            anz_first_header(sh, st)
            draw_anz_heading(sh, 316)
        else:
            anz_cont_header(sh, st, pi + 1, npg)
            draw_anz_heading(sh, 92)
        on_page = []
        for kind, idx, y in items:
            if kind == "open":
                sh.text(COLS["date"][0] + 2, y, d_mon(st["start"]), SZ)
                sh.text(COLS["party"][0] + 2, y, "Opening balance", SZ)
                put_balance(sh, y, st["opening"])
            elif kind == "none":
                sh.text(COLS["party"][0] + 2, y, "No transactions for this period", SZ)
            elif kind == "txn":
                draw_anz_txn(sh, rows[idx], y)
                on_page.append(rows[idx])
                last_bal = rows[idx]["bal"]
            elif kind == "ptot":
                sh.text(COLS["party"][0] + 2, y, "Totals at end of page", SZ, bold=True)
                put_money(sh, "debit", y, sum(r["debit"] or 0 for r in on_page), bold=True)
                put_money(sh, "credit", y, sum(r["credit"] or 0 for r in on_page), bold=True)
            elif kind == "cf":
                sh.text(COLS["party"][0] + 2, y, "Balance Carried Forward", SZ)
                put_balance(sh, y, last_bal)
            elif kind == "bf":
                sh.text(COLS["party"][0] + 2, y, "Balance brought forward from previous page", SZ)
                put_balance(sh, y, last_bal)
        if pi == npg - 1:
            y = st["tail_y"]
            if rows:
                y += 12
                sh.text(COLS["party"][0] + 2, y, "Totals at end of period", SZ, bold=True)
                put_money(sh, "debit", y, st["tot_d"], bold=True)
                put_money(sh, "credit", y, st["tot_c"], bold=True)
            if st["limit"]:
                y += 18
                sh.text(36, y, "Your available credit is $%s as at the closing date of this "
                               "statement." % mag(max(0, st["closing"] + st["limit"])), 8)
            y += 18
            sh.text(36, y, "Transaction codes", 7.5, bold=True)
            for k, (c, w) in enumerate(LEGEND):
                col, rowk = k % 3, k // 3
                sh.text(36 + 175 * col, y + 10 * (rowk + 1), "%s: %s" % (c, w), 6.5)
        anz_footer(sh, st, pi + 1, npg)
        sh.end_page()


def anz_statement(rng, spec):
    """spec: start (date or None for START), end, n, opening, stno, acct, name, holder,
    product, limit, plus gen_anz knobs."""
    start, end = spec["start"], spec["end"]
    first_day = spec.get("first_day") or start
    knobs = {k: spec[k] for k in ("limit", "od_walk", "fees", "interest", "interest_split",
                                   "wraps", "money_ref", "redact_dates", "deposit_first",
                                   "cash_fee") if k in spec}
    if spec.get("empty"):
        rows, closing = [], spec["opening"]
    else:
        rows, closing = gen_anz(rng, first_day, end, spec["n"], spec["opening"], **knobs)
    st = dict(spec)
    st.update(rows=rows, closing=closing, kind="anz",
              tot_d=sum(r["debit"] or 0 for r in rows), tot_c=sum(r["credit"] or 0 for r in rows),
              opening_row=start is not None and not spec.get("empty"),
              cf_style=spec.get("cf_style", "brought"), limit=spec.get("limit", 0))
    st["period_text"] = ("START - %s" % d_mon_y(end) if start is None
                         else "%s to %s" % (d_mon_y(start), d_mon_y(end)))
    st["pages"] = anz_paginate(st)
    return st


def anz_truth_rows(st, stmt_index=None):
    out = []
    for r in st["rows"]:
        t = {"date": r["date"].isoformat(), "description": anz_desc(r),
             "debit": None if r["debit"] is None else r["debit"] / 100.0,
             "credit": None if r["credit"] is None else r["credit"] / 100.0,
             "balance": r["bal"] / 100.0}
        if r["redact"] == "date_only":
            t.update(description=None, debit=None, credit=None, balance=None,
                     redacted=["description", "amount", "balance"])
        if stmt_index is not None:
            t["statement_index"] = stmt_index
        out.append(t)
    return out


# ---------------------------------------------------------------------------
# ANZ Loan
# ---------------------------------------------------------------------------

LSZ = 8.5
LPITCH = 13.0
LCPITCH = 10.0
LCOLS = dict(date=(40, 100), desc=(100, 320), debit=(320, 395), credit=(395, 470),
             balance=(470, 556))


def loan_number(rng):
    return "%04d-%010d-%04d" % (rng.randint(100, 999), rng.randint(10 ** 8, 10 ** 9),
                                rng.randint(1001, 1009))


def next_weekday(d):
    while d.weekday() >= 5:
        d += dt.timedelta(days=1)
    return d


def gen_loan(rng, spec):
    """Rows of one loan statement, holder side: interest, fees, drawdowns and
    reversals are debits; payments are credits; the balance owed is negative."""
    start, end = spec["start"], spec["end"]
    owed = spec["owed"]              # positive cents owed at the start
    rate = spec["rate"]              # percent p.a.
    pay = spec["pay"]
    ev = []                           # (date, order, row)

    def txn(d, dr, amt, desc, pdate=None):
        return {"date": d, "dir": dr, "amt": amt, "desc": desc, "pdate": pdate, "event": None}

    if spec.get("drawdown"):
        ev.append((start, 1, txn(start, "D", spec["drawdown"], "LOAN DRAWDOWN")))
    # payments
    d = spec["first_pay"]
    step = spec.get("freq", "month")
    k = 0
    while d <= end:
        pd_ = None
        if d.weekday() >= 5:
            pd_ = next_weekday(d)
        elif rng.random() < 0.2:
            pd_ = next_weekday(d + dt.timedelta(days=1))
        desc = "LOAN PAYMENT - INTEREST" if spec.get("interest_only") else "LOAN PAYMENT"
        ev.append((d, 3, txn(d, "C", pay, desc, pd_)))
        k += 1
        if step == "fortnight":
            d = d + dt.timedelta(days=14)
        else:
            m = d.month % 12 + 1
            y = d.year + (1 if d.month == 12 else 0)
            dd = min(spec["first_pay"].day, calendar.monthrange(y, m)[1])
            d = dt.date(y, m, dd)
    # month-end interest
    for y in range(start.year, end.year + 1):
        for m in range(1, 13):
            me = month_end(y, m)
            if start <= me <= end:
                ev.append((me, 2, txn(me, "D", None, "LOAN INTEREST")))
    for item in spec.get("extras", []):
        ev.append(item(rng))
    for rc in spec.get("rate_changes", []):
        d_, txt = rc[0], rc[1]
        ev.append((d_, rc[2] if len(rc) > 2 else 0, {"date": d_, "event": txt}))
    ev.sort(key=lambda e: (e[0], e[1]))
    rows = []
    bal = -owed
    cur_rate = rate
    for d_, order, r in ev:
        if r["event"]:
            rows.append(r)
            if "%" in r["event"]:
                cur_rate = float(r["event"].split()[-2].rstrip("%"))
            continue
        if r["amt"] is None:          # interest: on the balance owed, a month's worth
            r["amt"] = max(100, int(round(-bal * cur_rate / 100.0 / 12.0)))
            if spec.get("interest_only"):
                pass
        if r["dir"] == "D":
            r["debit"], r["credit"] = r["amt"], None
            bal -= r["amt"]
        else:
            r["debit"], r["credit"] = None, r["amt"]
            bal += r["amt"]
        r["bal"] = bal
        rows.append(r)
    # the balance is printed on the last money row of each day only
    money_rows = [r for r in rows if not r["event"]]
    for i, r in enumerate(money_rows):
        nxt = money_rows[i + 1] if i + 1 < len(money_rows) else None
        r["bal_printed"] = not (nxt is not None and nxt["date"] == r["date"])
    return rows, bal


def loan_paginate(st):
    top1, top2 = 332.0, 108.0
    bottom = 742.0
    tail_last = 120.0          # closing balance + fee summary
    pages, cur = [], []
    y = top1
    cur.append(("open", None, y))
    y += LPITCH
    rows = st["rows"]
    for i, r in enumerate(rows):
        h = LPITCH + (LCPITCH if r.get("pdate") else 0)
        last = i == len(rows) - 1
        need = h - LPITCH + (tail_last if last else 18.0)
        if y + need > bottom and any(k == "txn" for k, _, _ in cur):
            cur.append(("cf", None, y + 2))
            pages.append(cur)
            cur = [("bf", None, top2)]
            y = top2 + LPITCH
        cur.append(("txn", i, y))
        y += h
    cur.append(("close", None, y + 2))
    pages.append(cur)
    st["tail_y"] = y + 2
    return pages


def lput(sh, k, y, s, bold=False, align=None):
    x0, x1 = LCOLS[k]
    if align == "right" or (align is None and k in ("debit", "credit", "balance")):
        return sh.text(x1 - 3, y, s, LSZ, bold=bold, align="right")
    if sh.width(s, LSZ, bold) > (x1 - x0) - 4:
        raise GenError("%r does not fit the loan %s column" % (s, k))
    return sh.text(x0 + 2, y, s, LSZ, bold=bold)


def lbal(sh, y, c, bold=False):
    s = mag(c) + (" DR" if c < 0 else (" CR" if c > 0 else ""))
    lput(sh, "balance", y, s, bold=bold)


def loan_letterhead(sh, title):
    sh.text(40, 46, "ANZ", 22, bold=True)
    sh.text(556, 38, title, 11, bold=True, align="right")
    sh.text(40, 62, "%s   PO Box 9900   Sampleton 1142   Helpline 0800 000 000" % LEGAL, 7)
    sh.text(297, 74, SYNTHETIC, 6.5, align="center")


def loan_first_header(sh, st):
    loan_letterhead(sh, st["title"])
    y = 100
    sh.text(40, y, st["holder"][0], 9, bold=True)
    for k, ln in enumerate(st["holder"][1]):
        sh.text(40, y + 11 * (k + 1), ln, 9)
    if st.get("name_block"):
        sh.text(330, 100, "Account Name", 8.5)
        sh.text(410, 100, st["name"], 8.5, bold=True)
        sh.text(330, 112, "Account Number", 8.5)
        sh.text(410, 112, st["acct"], 8.5)
    sh.text(40, 178, "Summary of %s Number %s" % (st["loan_kind"], st["acct"]), 10, bold=True)
    sh.text(40, 194, "The following is a summary of your loan for the period %s to %s"
            % (st["pfmt"](st["start"]), st["pfmt"](st["end"])), 8.5)
    left = [("Principal Paid", mag(st["principal_paid"])),
            ("Interest Paid", mag(st["interest_paid"])),
            ("Interest Owing as at %s" % d_mon_y(st["end"]), mag(st["interest_owing"]))]
    right = [("Next Loan Payment Amount", mag(st["pay"])),
             ("Next Loan Payment Date Due", d_mon_y(st["next_due"])),
             ("Fixed Rate Review Date", d_mon_y(st["review"])),
             ("Maturity Date", d_mon_y(st["maturity"]))]
    for k, (a, b) in enumerate(left):
        sh.text(40, 214 + 12 * k, a, 8.5)
        sh.text(286, 214 + 12 * k, b, 8.5, align="right")
    for k, (a, b) in enumerate(right):
        sh.text(318, 214 + 12 * k, a, 8.5)
        sh.text(556, 214 + 12 * k, b, 8.5, align="right")


def loan_cont_header(sh, st):
    sh.text(40, 40, "ANZ", 16, bold=True)
    sh.text(556, 36, "Loan %s" % st["acct"], 7.5, align="right")
    sh.text(297, 56, SYNTHETIC, 6.5, align="center")


def loan_heading(sh, y):
    lput(sh, "date", y, "Date", bold=True)
    lput(sh, "desc", y, "Description", bold=True)
    lput(sh, "debit", y, "Withdrawals", bold=True)
    lput(sh, "credit", y, "Deposits", bold=True)
    lput(sh, "balance", y, "Principal Balance", bold=True)
    sh.line(40, y + 4, 556, y + 4, width=0.6)


def loan_footer(sh, pno, npg):
    sh.text(40, 802, LEGAL, 7)
    sh.text(556, 802, "Page %d of %d" % (pno, npg), 7, align="right")
    sh.text(297, 826, SYNTHETIC, 6.5, align="center")


def render_loan(sh, st):
    pages = st["pages"]
    npg = len(pages)
    rows = st["rows"]
    last_bal = -st["owed"]
    for pi, items in enumerate(pages):
        sh.begin_page()
        if pi == 0:
            loan_first_header(sh, st)
            loan_heading(sh, 316)
        else:
            loan_cont_header(sh, st)
            loan_heading(sh, 92)
        for kind, idx, y in items:
            if kind == "open":
                lput(sh, "date", y, d_mon_yy(st["start"]))
                if st.get("open_rate_row"):
                    lput(sh, "desc", y, "Opening interest rate %.2f%% p.a." % st["rate"])
                else:
                    lput(sh, "desc", y, "Opening Balance")
                lbal(sh, y, -st["owed"])
            elif kind == "txn":
                r = rows[idx]
                lput(sh, "date", y, d_mon_yy(r["date"]))
                if r["event"]:
                    lput(sh, "desc", y, r["event"])
                    continue
                lput(sh, "desc", y, r["desc"])
                if r["debit"] is not None:
                    lput(sh, "debit", y, mag(r["debit"]))
                if r["credit"] is not None:
                    lput(sh, "credit", y, mag(r["credit"]))
                if r["bal_printed"]:
                    lbal(sh, y, r["bal"])
                    last_bal = r["bal"]
                else:
                    last_bal = r["bal"]
                if r["pdate"]:
                    sh.text(LCOLS["desc"][0] + 10, y + LCPITCH,
                            "- processed on: %s" % d_mon_yy(r["pdate"]), LSZ)
            elif kind == "cf":
                lput(sh, "desc", y, "Balance Carried Forward")
                lbal(sh, y, last_bal)
            elif kind == "bf":
                lput(sh, "desc", y, "Balance Carried Forward")
                lbal(sh, y, last_bal)
            elif kind == "close":
                lput(sh, "desc", y, "Closing Balance", bold=True)
                lbal(sh, y, st["closing"], bold=True)
        if pi == npg - 1:
            y = st["tail_y"] + 30
            sh.text(40, y, "Summary of total loan fees for the period %s to %s"
                    % (st["pfmt"](st["start"]), st["pfmt"](st["end"])), 9, bold=True)
            sh.text(40, y + 16, "Fee Description", 8.5, bold=True)
            sh.text(300, y + 16, "Amount", 8.5, bold=True, align="right")
            fees = [r for r in rows if not r["event"] and r["desc"].endswith("FEE")]
            yy = y + 16
            for r in fees:
                yy += 12
                sh.text(40, yy, r["desc"].title(), 8.5)
                sh.text(300, yy, mag(r["debit"]), 8.5, align="right")
            yy += 12
            sh.text(40, yy, "Total Loan Fees", 8.5, bold=True)
            sh.text(300, yy, mag(sum(r["debit"] for r in fees)), 8.5, bold=True, align="right")
        loan_footer(sh, pi + 1, npg)
        sh.end_page()


def render_notice(sh, nt):
    sh.begin_page()
    loan_letterhead(sh, nt["title"])
    sh.text(40, 100, nt["holder"][0], 9, bold=True)
    for k, ln in enumerate(nt["holder"][1]):
        sh.text(40, 100 + 11 * (k + 1), ln, 9)
    sh.text(556, 100, "Date: %s" % d_mon_y(nt["date"]), 8.5, align="right")
    y = 180
    for a, b in nt["fields"]:
        sh.text(40, y, a, 9)
        sh.text(330, y, b, 9, align="right")
        y += 14
    y += 10
    for ln in nt["text"]:
        sh.text(40, y, ln, 8.5)
        y += 12
    sh.text(297, 826, SYNTHETIC, 6.5, align="center")
    sh.end_page()


def loan_statement(rng, spec):
    rows, closing = gen_loan(rng, spec)
    st = dict(spec)
    money_rows = [r for r in rows if not r["event"]]
    interest = sum(r["debit"] for r in money_rows if r["desc"] == "LOAN INTEREST")
    paid = sum(r["credit"] or 0 for r in money_rows)
    st.update(rows=rows, closing=closing, kind="loan", interest_paid=interest,
              principal_paid=max(0, paid - interest),
              interest_owing=int(round(-closing * spec["rate"] / 100.0 / 365.0 * 3)),
              title=spec.get("title", "Home Loan Statement"))
    st["pages"] = loan_paginate(st)
    return st


def loan_truth_rows(st, stmt_index=None):
    out = []
    for r in st["rows"]:
        if r["event"]:
            continue
        desc = r["desc"] + ((" - processed on: %s" % d_mon_yy(r["pdate"])) if r["pdate"] else "")
        t = {"date": r["date"].isoformat(), "description": desc,
             "debit": None if r["debit"] is None else r["debit"] / 100.0,
             "credit": None if r["credit"] is None else r["credit"] / 100.0,
             "balance": r["bal"] / 100.0 if r["bal_printed"] else None}
        if stmt_index is not None:
            t["statement_index"] = stmt_index
        out.append(t)
    return out


# ---------------------------------------------------------------------------
# Cases
# ---------------------------------------------------------------------------

HOLDERS = [("SAMPLE TRADING LIMITED", ["UNIT 4, 88 FICTION AVENUE", "SAMPLEVILLE 0610"]),
           ("EXAMPLE PLUMBING LTD", ["PO BOX 4321", "DEMOTOWN 7022"]),
           ("MS A EXAMPLE", ["FLAT 3", "45 SPECIMEN ROAD", "DEMOTOWN 7020"]),
           ("J SAMPLE & K SAMPLE", ["12 EXAMPLE STREET", "TESTBURY 9010"]),
           ("DEMO FARMS PARTNERSHIP", ["1450 MOCK VALLEY ROAD", "RD 5", "TESTBURY 9071"])]


def anz_acct(rng):
    return "01-%04d-%07d-%02d" % (rng.choice([1, 4, 8, 9, 10, 11, 13, 21, 25, 30]),
                                   rng.randint(0, 9999999), rng.randint(0, 9))


def anz_cases():
    """(name, note, [statement specs]) -- several specs make a bundle."""
    C = []

    def base(rng, h, product, **kw):
        d = dict(holder=HOLDERS[h], name=HOLDERS[h][0], product=product, acct=anz_acct(rng))
        d.update(kw)
        return d

    r = random.Random(seed_of("anz_qvf_1"))
    C.append(("anz_qvf_1", "Business current account, one month, two pages: totals at end of "
              "page, balance brought forward from previous page, totals at end of period, "
              "available credit line, code legend.",
              [base(r, 0, "Business Current Account", start=dt.date(2019, 9, 1),
                    end=dt.date(2019, 9, 30), n=40, opening=1834512, stno=14, limit=500000,
                    wraps=1)]))
    r = random.Random(seed_of("anz_qvf_2"))
    C.append(("anz_qvf_2", "Overdrawn account: opening and closing OD, balances cross zero "
              "both ways, every OD balance printed '1,234.56 OD', debit interest.",
              [base(r, 1, "Business Current Account", start=dt.date(2020, 3, 1),
                    end=dt.date(2020, 3, 31), n=26, opening=-215030, stno=27, limit=800000,
                    od_walk=True, interest="debit")]))
    r = random.Random(seed_of("anz_qvf_3"))
    C.append(("anz_qvf_3", "Period crossing a new year (15 Dec to 14 Jan), year-less dates.",
              [base(r, 2, "Everyday account", start=dt.date(2021, 12, 15),
                    end=dt.date(2022, 1, 14), n=24, opening=412095, stno=41, fees=False)]))
    r = random.Random(seed_of("anz_qvf_4"))
    acct = anz_acct(r)
    C.append(("anz_qvf_4", "Three statements in one file: a new account's first statement "
              "with a 'START - 30 Sep 2019' period and no opening row, then a month with "
              "'No transactions for this period', then a normal month.",
              [base(r, 0, "Business Current Account", start=None, first_day=dt.date(2019, 9, 12),
                    end=dt.date(2019, 9, 30), n=12, opening=0, stno=1, acct=acct, limit=0,
                    deposit_first=True, fees=False),
               base(r, 0, "Business Current Account", start=dt.date(2019, 10, 1),
                    end=dt.date(2019, 10, 31), n=0, opening=None, stno=2, acct=acct, empty=True),
               base(r, 0, "Business Current Account", start=dt.date(2019, 11, 1),
                    end=dt.date(2019, 11, 30), n=18, opening=None, stno=3, acct=acct)]))
    r = random.Random(seed_of("anz_qvf_5"))
    C.append(("anz_qvf_5", "Wrapped other-party names, a CREDIT INTEREST PAID row with a "
              "'Premium interest $x Standard interest $y' second line, a reference that "
              "looks like money (1043.20), a cash deposit with a cash handling fee, "
              "'Balance Carried Forward' at the foot of each page.",
              [base(r, 4, "Business Online Call Account", start=dt.date(2023, 5, 1),
                    end=dt.date(2023, 5, 31), n=46, opening=2290017, stno=8, wraps=4,
                    interest_split=True, money_ref=True, cash_fee=True, cf_style="carried")]))
    r = random.Random(seed_of("anz_qvf_6"))
    acct = anz_acct(r)
    C.append(("anz_qvf_6", "Two consecutive monthly statements in one file, each with its "
              "own glance box and page numbering.",
              [base(r, 3, "Everyday account", start=dt.date(2024, 2, 1), end=dt.date(2024, 2, 29),
                    n=22, opening=318840, stno=60, acct=acct),
               base(r, 3, "Everyday account", start=dt.date(2024, 3, 1), end=dt.date(2024, 3, 31),
                    n=30, opening=None, stno=61, acct=acct)]))
    r = random.Random(seed_of("anz_qvf_7"))
    C.append(("anz_qvf_7", "Two rows redacted except their dates (text removed, black box): "
              "the QVF's 'redacted transactions when the dates are not redacted'.",
              [base(r, 1, "Business Current Account", start=dt.date(2022, 7, 1),
                    end=dt.date(2022, 7, 31), n=24, opening=905511, stno=33, limit=300000,
                    redact_dates=2)]))
    r = random.Random(seed_of("anz_qvf_8"))
    C.append(("anz_qvf_8", "A single statement with 'No transactions for this period' "
              "(opening = closing, nothing in the table).",
              [base(r, 4, "Business Online Call Account", start=dt.date(2023, 1, 1),
                    end=dt.date(2023, 1, 31), n=0, opening=1250000, stno=12, empty=True,
                    limit=0)]))
    r = random.Random(seed_of("anz_qvf_9"))
    C.append(("anz_qvf_9", "A new account's first statement on its own: 'START - 31 Mar 2022' "
              "period, no opening-balance row, no statement date in the glance box, so the "
              "year of the year-less dates can only come from the START period.",
              [base(r, 2, "Everyday account", start=None, first_day=dt.date(2022, 3, 9),
                    end=dt.date(2022, 3, 31), n=16, opening=0, stno=1, limit=0,
                    deposit_first=True, fees=False, no_stmt_date=True)]))
    return C


def loan_cases():
    C = []

    def lbase(rng, h, **kw):
        d = dict(holder=HOLDERS[h], name=HOLDERS[h][0], acct=loan_number(rng),
                 loan_kind="Home Loan", pfmt=d_long, review=dt.date(2027, 3, 15),
                 maturity=dt.date(2045, 3, 15))
        d.update(kw)
        return d

    def fee(d, amt, desc):
        return lambda rng: (d, 4, {"date": d, "dir": "D", "amt": amt, "desc": desc,
                                   "pdate": None, "event": None})

    def credit(d, amt, desc):
        return lambda rng: (d, 4, {"date": d, "dir": "C", "amt": amt, "desc": desc,
                                   "pdate": None, "event": None})

    r = random.Random(seed_of("anz_loan_qvf_1"))
    C.append(("anz_loan_qvf_1", "Home loan, one month, Account Name block, fortnightly "
              "payments with '- processed on:' second lines, month-end LOAN INTEREST, a "
              "loan service fee and the fee summary.",
              [lbase(r, 3, name_block=True, start=dt.date(2015, 11, 1), end=dt.date(2015, 11, 30),
                     owed=24512088, rate=5.25, pay=92500, freq="fortnight",
                     first_pay=dt.date(2015, 11, 1), next_due=dt.date(2015, 12, 13),
                     extras=[fee(dt.date(2015, 11, 16), 1000, "LOAN SERVICE FEE")])]))
    r = random.Random(seed_of("anz_loan_qvf_2"))
    C.append(("anz_loan_qvf_2", "Business term loan, a whole year: 'Opening interest rate x% "
              "p.a.' first row carrying the opening balance, Rate change rows with no money, "
              "monthly payment on the last day so interest and payment share a date (balance "
              "on the last row of the day only), Balance Carried Forward across pages.",
              [lbase(r, 0, loan_kind="Term Loan", title="Term Loan Statement", open_rate_row=True,
                     start=dt.date(2017, 1, 1), end=dt.date(2017, 12, 31), owed=48000000,
                     rate=6.10, pay=520000, freq="month", first_pay=dt.date(2017, 1, 31),
                     next_due=dt.date(2018, 1, 31),
                     rate_changes=[(dt.date(2017, 3, 14), "Rate change 6.35% p.a."),
                                   (dt.date(2017, 5, 2), "Rate change 6.20% p.a.")],
                     extras=[fee(dt.date(2017, 2, 10), 5000, "LOAN VARIATION FEE"),
                             fee(dt.date(2017, 4, 3), 2500, "LOAN SERVICE FEE")])]))
    r = random.Random(seed_of("anz_loan_qvf_3"))
    acct = loan_number(r)
    q1 = lbase(r, 2, acct=acct, start=dt.date(2018, 7, 1), end=dt.date(2018, 9, 30),
               owed=31244013, rate=4.95, pay=85000, freq="fortnight",
               first_pay=dt.date(2018, 7, 6), next_due=dt.date(2018, 10, 12),
               review=dt.date(2018, 11, 6), pfmt=d_mon_y)
    C.append(("anz_loan_qvf_3", "Two quarterly statements in one file with a fixed-rate "
              "expiry NOTICE between them; no Account Name block (name on the letterhead "
              "only).", [q1, ("notice_expiry", dt.date(2018, 10, 9)),
                         lbase(r, 2, acct=acct, start=dt.date(2018, 10, 1),
                               end=dt.date(2018, 12, 31), owed=None, rate=5.85, pay=85000,
                               freq="fortnight", first_pay=dt.date(2018, 10, 12),
                               next_due=dt.date(2019, 1, 4), pfmt=d_mon_y)]))
    r = random.Random(seed_of("anz_loan_qvf_4"))
    C.append(("anz_loan_qvf_4", "A new loan: Opening Balance 0.00, LOAN DRAWDOWN on day one, "
              "six months crossing a new year (1 Oct 2016 to 31 Mar 2017), dd Mon yy dates.",
              [lbase(r, 4, name_block=True, start=dt.date(2016, 10, 1), end=dt.date(2017, 3, 31),
                     owed=0, drawdown=38500000, rate=5.49, pay=110000, freq="fortnight",
                     first_pay=dt.date(2016, 10, 14), next_due=dt.date(2017, 4, 7))]))
    r = random.Random(seed_of("anz_loan_qvf_5"))
    C.append(("anz_loan_qvf_5", "A dishonoured payment: REVERSAL - LOAN PAYMENT, DISHONOUR "
              "FEE, LOAN PAYMENT - ARREARS; a Rate change as the last table row; an "
              "interest-rate change NOTICE after the statement.",
              [lbase(r, 1, name_block=True, start=dt.date(2020, 8, 1), end=dt.date(2020, 8, 31),
                     owed=19876543, rate=4.45, pay=74000, freq="fortnight",
                     first_pay=dt.date(2020, 8, 7), next_due=dt.date(2020, 9, 4),
                     rate_changes=[(dt.date(2020, 8, 31), "Rate change 3.99% p.a.", 9)],
                     extras=[lambda rng: (dt.date(2020, 8, 10), 4,
                                          {"date": dt.date(2020, 8, 10), "dir": "D",
                                           "amt": 74000, "desc": "REVERSAL - LOAN PAYMENT",
                                           "pdate": None, "event": None}),
                             fee(dt.date(2020, 8, 10), 2000, "DISHONOUR FEE"),
                             credit(dt.date(2020, 8, 14), 74000, "LOAN PAYMENT - ARREARS")]),
               ("notice_rate", dt.date(2020, 9, 2))]))
    r = random.Random(seed_of("anz_loan_qvf_6"))
    acct = loan_number(r)
    specs = [("notice_expiry", dt.date(2021, 1, 20))]
    owed = 52200150
    for k, (s, e) in enumerate([(dt.date(2021, 2, 1), dt.date(2021, 2, 28)),
                                (dt.date(2021, 3, 1), dt.date(2021, 3, 31)),
                                (dt.date(2021, 4, 1), dt.date(2021, 4, 30))]):
        specs.append(lbase(r, 3, acct=acct, name_block=True, start=s, end=e,
                           owed=owed if k == 0 else None, rate=3.69, pay=245000, freq="month",
                           first_pay=dt.date(s.year, s.month, 15),
                           next_due=dt.date(e.year, e.month % 12 + 1, 15)))
    C.append(("anz_loan_qvf_6", "A notice first, then three monthly statements in one file "
              "(the QVF splits on 'The following').", specs))
    return C


def notice(kind, d, st, rng):
    if kind == "notice_expiry":
        return dict(title="Fixed interest rate expiry notice", date=d, holder=st["holder"],
                    fields=[("Loan Number", st["acct"]),
                            ("Current Balance", "%s DR" % mag(st["owed"] if st["owed"] else 0)),
                            ("Current Fixed Interest Rate", "%.2f%% p.a." % st["rate"]),
                            ("Fixed rate expiry date", d_mon_y(d + dt.timedelta(days=28))),
                            ("New Floating Interest Rate", "%.2f%% p.a." % (st["rate"] + 0.9)),
                            ("Next Payment Date", d_mon_y(d + dt.timedelta(days=10))),
                            ("Current Maturity Date", d_mon_y(st["maturity"]))],
                    text=["Your fixed interest rate period is about to end. If you do nothing,",
                          "your loan will move to the floating interest rate shown above.",
                          "Call us on 0800 000 000 to talk about fixing your rate again."])
    return dict(title="Interest rate change notice", date=d, holder=st["holder"],
                fields=[("Loan Number", st["acct"]),
                        ("Current Balance", "%s DR" % mag(st["owed"] if st["owed"] else 0)),
                        ("New interest rate", "%.2f%% p.a." % (st["rate"] - 0.46)),
                        ("Effective date", d_mon_y(d + dt.timedelta(days=14))),
                        ("New repayment amount", mag(int(st["pay"] * 0.96))),
                        ("Next Payment Date", d_mon_y(d + dt.timedelta(days=5)))],
                text=["We are changing the interest rate on your loan. Your new repayment",
                      "amount applies from the next payment date shown above."])


def features_for(kind, sts, npages):
    f = ["bank:anz", "font:Helvetica", "page:A4", "money:thousands", "heading_every_page",
         "pages:%d" % npages, "qvf:" + kind]
    if kind == "anz":
        f += ["cols:date|type|desc|part|code|ref|debit|credit|balance", "date:dd Mon",
              "date_yearless", "bal:od", "page_totals", "totals_line", "opening_line",
              "summary_box", "particulars_code_reference", "type_column"]
        rows = [r for st in sts for r in st["rows"]]
        if any(st["start"] is None for st in sts):
            f.append("qvf:start_period")
        if any(st.get("empty") for st in sts):
            f.append("qvf:no_transactions")
        if any(r["bal"] < 0 for r in rows) or any(st["opening"] < 0 for st in sts):
            f.append("negative_balance")
        if any(st["cf_style"] == "carried" for st in sts):
            f.append("carried_forward")
        if any(r["extra"] for r in rows):
            f.append("multiline_desc")
        if any(t.startswith("Premium interest") for r in rows for _, t in r["extra"]):
            f.append("qvf:premium_standard_interest")
        if any(r["redact"] for r in rows):
            f.append("redacted_removed")
    else:
        f += ["cols:date|desc|debit|credit|balance", "date:dd Mon yy", "bal:dr_only",
              "opening_line", "closing_line", "summary_box", "qvf:fee_summary"]
        rows = [r for st in sts for r in st["rows"]]
        if any(r.get("pdate") for r in rows):
            f.append("qvf:processed_on")
        if any(r["event"] for r in rows):
            f.append("qvf:rate_change_rows")
        if any(not r["event"] and not r["bal_printed"] for r in rows):
            f.append("balance_last_of_day")
        if any(len(st["pages"]) > 1 for st in sts):
            f.append("carried_forward")
        f.append("negative_balance")
    if any(st["start"] and st["start"].year != st["end"].year for st in sts):
        f.append("period_crosses_year")
    return f


# A case whose first draw fails its own self-check is re-drawn from a fixed, recorded
# offset, so the set stays byte-identical run to run.
SEED_BUMP = {"anz_qvf_2": 2}


def build(name, note, specs, out_dir, kind):
    pdf = os.path.join(out_dir, name + ".pdf")
    sh = Sheet(pdf, A4, "Helvetica")
    rng = random.Random(seed_of(name) + 1 + SEED_BUMP.get(name, 0))
    sts, rows, npages = [], [], 0
    prev_close = None
    pending_notices = []
    stmts_meta = []
    for sp in specs:
        if isinstance(sp, tuple):          # a notice
            pending_notices.append(sp)
            continue
        sp = dict(sp)
        if kind == "anz":
            if sp["opening"] is None:
                sp["opening"] = prev_close
            st = anz_statement(rng, sp)
        else:
            if sp["owed"] is None:
                sp["owed"] = -prev_close
            st = loan_statement(rng, sp)
        sts.append(st)
        prev_close = st["closing"]
    # draw in file order (notices where they were listed)
    si = 0
    for sp in specs:
        if isinstance(sp, tuple):
            ref = sts[si - 1] if si > 0 else sts[0]
            nt = notice(sp[0], sp[1], dict(ref, owed=(-ref["closing"] if si > 0 else ref["owed"])),
                        rng)
            render_notice(sh, nt)
            npages += 1
            continue
        st = sts[si]
        if kind == "anz":
            render_anz(sh, st)
        else:
            render_loan(sh, st)
        npages += len(st["pages"])
        si += 1
    sh.save()
    multi = len(sts) > 1
    for j, st in enumerate(sts):
        tr = anz_truth_rows(st, j if multi else None) if kind == "anz" \
            else loan_truth_rows(st, j if multi else None)
        rows += tr
        op = st["opening"] if kind == "anz" else -st["owed"]
        chain_check(name, tr, op / 100.0, st["closing"] / 100.0, False,
                    [False] * len(tr))
        stmts_meta.append({"statement_index": j,
                           "period_start": st["start"].isoformat() if st["start"] else None,
                           "period_end": st["end"].isoformat(),
                           "opening_balance": op / 100.0,
                           "closing_balance": st["closing"] / 100.0})
    st0 = sts[0]
    op0 = st0["opening"] if kind == "anz" else -st0["owed"]
    feats = features_for(kind, sts, npages)
    if multi:
        feats += ["bundle", "bundle:%d" % len(sts)]
    if any(isinstance(sp, tuple) for sp in specs):
        feats.append("qvf:notice_pages")
    acct = st0["acct"]
    truth = {
        "case": name,
        "generator": GENERATOR,
        "note": "QVF lookalike. ANZ, %s (synthetic). %s" % (
            st0.get("product") or st0.get("title"), note),
        "bank": "ANZ",
        "layout": "anz_qvf" if kind == "anz" else "anz_loan_qvf",
        "product": st0.get("product") or ("Term loan" if st0.get("loan_kind") == "Term Loan"
                                          else "Home loan"),
        "source_format": "pdf",
        "account_bank_code": acct[:2] if kind == "anz" else None,
        "account_number": acct,
        "account_redaction": None,
        "features": feats,
        "row_order": "oldest_first",
        "opening_balance": op0 / 100.0,
        "closing_balance": sts[-1]["closing"] / 100.0,
        "removed_rows": 0,
        "row_count": len(rows),
        "rows": rows,
    }
    if multi:
        truth["statements"] = stmts_meta
    write_json(os.path.join(out_dir, name + ".truth.json"), truth)
    return truth, npages


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=OUT_DEFAULT)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    index = []
    for kind, cases in (("anz", anz_cases()), ("anz_loan", loan_cases())):
        for name, note, specs in cases:
            try:
                truth, npg = build(name, note, specs, a.out, "anz" if kind == "anz" else "loan")
            except GenError as e:
                sys.exit("GENERATOR SELF-CHECK FAILED in %s: %s" % (name, e))
            print("%-18s %3d rows %d pg  %s" % (name, truth["row_count"], npg,
                                               " ".join(x for x in truth["features"]
                                                        if x.startswith(("qvf:", "bundle")))))
            index.append({"case": name, "file": name + ".pdf", "bank": "ANZ",
                          "layout": truth["layout"], "product": truth["product"],
                          "source_format": "pdf", "rows": truth["row_count"], "pages": npg,
                          "features": truth["features"], "type_key": kind})
    with open(os.path.join(a.out, "index.json"), "w") as f:
        json.dump(index, f, indent=1)
    return 0


if __name__ == "__main__":
    sys.exit(main())
