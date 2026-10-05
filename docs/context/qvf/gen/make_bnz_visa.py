#!/usr/bin/env python3
"""make_bnz_visa.py -- lookalike BNZ Visa statements, laid out the way the QVF
"Statement Converter" BNZ Visa section (script.qvs lines 5472-5994) expects them,
each written with an answer key in tools/synth/make_layouts.py's truth format.

What the script walks, and so what is printed here (see cards/bnz_visa.md):
  * "Statement" exactly once per statement (every word "Statement" starts one);
  * "Statement for the period <d Month yyyy> to <d Month yyyy> for ..." -- the word
    "for" is the 8th word after "period"; the period gives the year of the
    year-less "dd Mon" row dates (Oct/Nov/Dec rows take the START year when the
    period starts in Oct/Nov/Dec, everything else the END year);
  * the terms URL www.bnz.co.nz/cardterms, three words, then "<name> <19-character
    card number>" and the card type ("Lite", "Y" or "BNZ Advantage") within 14 words;
  * "Previous Balance" + 4 words + the opening figure, closed by the next $ figure
    or "Credit Limit"; "Current Balance <figure>" closed by "Current Minimum",
    "Please note" or "Over Limit/Overdue"; CR on a balance = in credit;
  * the transactions: from each column heading ending "Credit Amount $" to the next
    "Page", "Total", "Our" or a "BNZ" that is not right after a month: one heading
    per card section and per page, one "Total for card" line per section;
  * a row = "dd Mon", details, then ONE figure starting "$" (the debit or the credit
    column; the script cannot see which) and nothing after it before the next row's
    date -- so a wrapped row prints its figure on its LAST line;
  * "Continued over..." and "Page n of N" at the foot of every page.

Run:  python3 make_bnz_visa.py [--out DIR]       (default ../sets/visa2)
"""

import argparse
import datetime as dt
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import visa2_common as C  # noqa: E402
from visa2_common import GenError, ML, MON, PH  # noqa: E402

GENERATOR = "scratchpad/qvf/gen/make_bnz_visa.py"
VARIANT = "strip"        # "inline" only for experiments outside the set (--variant)
LEFT, RIGHT = 40.0, 553.0
X_DATE, X_DESC, X_DEB, X_CRE = 42.0, 82.0, 470.0, 553.0
SIZE = 8.0
PITCH, CPITCH = 12.5, 9.5
TOP1, TOP2, BOTTOM = 374.0, 98.0, PH - 74.0
DESC_W = X_DEB - 62.0 - X_DESC

PRODUCTS = {
    "lite": dict(name="BNZ Lite Visa", detail="Lite Visa credit card", limit=300000),
    "advantage": dict(name="BNZ Advantage Visa Platinum", detail="BNZ Advantage Visa Platinum",
                      limit=1000000),
}
HOLDERS = [("J SAMPLE", ["12 EXAMPLE STREET", "SAMPLEVILLE", "TESTBURY 9010"]),
           ("MS A EXAMPLE", ["FLAT 3", "45 SPECIMEN ROAD", "DEMOTOWN 7020"]),
           ("MR P TESTER", ["7 PLACEHOLDER LANE", "RD 2", "MOCKBURN 9310"]),
           ("R DEMO", ["101 TEMPLATE TERRACE", "TESTBURY 9011"])]
SECOND = ["K SAMPLE", "B EXAMPLE", "L TESTER", "M DEMO"]

# What a BNZ Visa row says. Purchases are plain merchant names; the bank's own
# lines are the words the QVF's deposit list looks for (PAYMENT THANK YOU,
# BNZ Cash Reward, Purchase Return/Refund, ACCOUNT FEE CREDIT).
BUY = [
    (6, ("r", 12, 320), ["{grocer} {city}"]),
    (3, ("r", 35, 160), ["{fuel} {city}"]),
    (3, ("r", 4, 40), ["{cafe} {city}"]),
    (2, ("r", 6, 120), ["{online} {city}"]),
    (1, ("r", 180, 1450), ["{airline} {city} TKT {invno}"]),
]
WRAP_BUY = [
    (1, ("r", 40, 400), ["{airline}", "{city} TKT {invno} 2 x ADULT"]),
    (2, ("r", 12, 320), ["{grocer}", "{city} NZ"]),
]
FX = [["{online} SAN FRANCISCO", "USD {fx} @ {rate}"], ["{online} SYDNEY", "AUD {fx} @ {arate}"]]


def row(rng, d, lines, spec, cat, vals=None):
    vals = {} if vals is None else vals
    text = [ML.fit(lambda s: ML.stringWidth(s, "Helvetica", SIZE), C.fill(rng, t, vals), DESC_W)
            for t in lines]
    amt = C.amount_for(rng, spec, vals) if spec else None
    return {"dir": d, "lines": text, "amt": amt, "cat": cat}


def pick(rng, table):
    return rng.choices(table, weights=[w for w, _, _ in table])[0]


def section_rows(rng, st, sec, n, o):
    """One cardholder's rows in printed (date) order."""
    start, end = st["start"], st["end"]
    days = (end - start).days
    rows = []
    primary = sec == 0
    extras = []
    if primary and o.get("payment", True) and st["opening_owed"] > 0:
        extras.append(("pay", row(rng, "C", ["PAYMENT THANK YOU"], None, "payment")))
    if primary and o.get("reward"):
        extras.append(("any", row(rng, "C", ["BNZ Cash Reward"], ("r", 5, 40), "reward")))
    if o.get("ret") and sec == o.get("ret_sec", 0):
        extras.append(("any", row(rng, "C", ["Purchase Return/Refund {online}"], ("r", 8, 90), "return")))
    if primary and o.get("feecredit"):
        extras.append(("any", row(rng, "C", ["ACCOUNT FEE CREDIT"], ("fixed", 45.0), "feecredit")))
    for _ in range(o.get("refund_nokw", 0) if primary else 0):
        # A merchant refund printed as the merchant's name alone, in the credit
        # column: no word the QVF's deposit list knows.
        extras.append(("any", row(rng, "C", ["{grocer} {city}"], ("r", 20, 140), "refund_nokw")))
    for _ in range(o.get("fx", 0) if primary or o.get("fx_all") else 0):
        vals = {}
        r = row(rng, "D", rng.choice(FX), ("fx",), "fx", vals)
        fee = row(rng, "D", ["OVERSEAS TRANSACTION FEE"], ("fixed", round(r["amt"] * 0.025) / 100.0),
                  "fxfee")
        extras.append(("pair", (r, fee)))
    for _ in range(o.get("atm", 0) if primary else 0):
        r = row(rng, "D", ["BNZ ATM SDM {atm}"], ("mult", 20, 400, 20), "atm")
        fee = row(rng, "D", ["CASH ADVANCE FEE"], ("fixed", 2.50), "cafee")
        extras.append(("pair", (r, fee)))
    k_plain = max(0, n - sum(2 if k == "pair" else 1 for k, _ in extras)
                  - (2 if primary and o.get("interest") else 0) - (1 if primary and o.get("insurance") else 0))
    for _ in range(k_plain):
        if rng.random() < o.get("wrap", 0.0):
            _, spec, lines = pick(rng, WRAP_BUY)
        else:
            _, spec, lines = pick(rng, BUY)
        extras.append(("any", row(rng, "D", lines, spec, "buy")))
    rng.shuffle(extras)
    # Dates: a payment early, everything else spread over the period.
    dated = []
    for kind, item in extras:
        if kind == "pay":
            d = start + dt.timedelta(days=rng.randint(2, min(9, days)))
        else:
            d = start + dt.timedelta(days=rng.randint(0, days))
        dated.append((d, kind, item))
    dated.sort(key=lambda x: x[0])
    for d, kind, item in dated:
        for r in (item if kind == "pair" else (item,)):
            r["date"] = d
            rows.append(r)
    # The month's charges, on the last day, in the primary section.
    if primary and o.get("interest"):
        rows.append(dict(row(rng, "D", ["INTEREST CHARGED ON PURCHASES"], ("r", 5, 60), "interest"), date=end))
        rows.append(dict(row(rng, "D", ["INTEREST CHARGED ON CASH ADVANCES"], ("r", 1, 15), "interest"),
                         date=end))
    if primary and o.get("insurance"):
        # A DEBIT whose words contain "CREDIT": the QVF's keyword list calls it a deposit.
        rows.append(dict(row(rng, "D", ["CREDIT CARD REPAYMENT INSURANCE"], ("r", 8, 25), "insurance"),
                         date=end))
    if primary and o.get("info_line"):
        # A dated line with no figure: the QVF drops it as a "missing amount" row
        # (script.qvs 5832-5841). Not a money movement, so not in the key.
        k = rng.randint(2, max(2, len(rows) - 3))
        rows.insert(k, {"dir": None, "lines": ["CARD REISSUED - REPLACEMENT CARD SENT"], "amt": None,
                        "cat": "info", "info": True, "date": rows[k]["date"]})
    for r in rows:
        r["desc"] = " ".join(r["lines"])
        r["sec"] = sec
    return rows


def build_statement(rng, spec, j, ident, opening_owed):
    y, m, d = spec["start"]
    start = C.add_months(dt.date(y, m, d), j)
    end = C.add_months(start, 1) - dt.timedelta(days=1)
    st = {"start": start, "end": end, "opening_owed": opening_owed, "ident": ident}
    o = dict(spec.get("opts", {}))
    o.update(spec.get("per", {}).get(j, {}))
    ns = spec["n"][j] if isinstance(spec["n"][0], list) else spec["n"]
    rows = []
    for sec, n in enumerate(ns):
        rows += section_rows(rng, st, sec, n, o)
    st["draw_rows"] = rows
    st["rows"] = [r for r in rows if not r.get("info")]
    rows = st["rows"]
    # Payment: the opening amount owed in full, part of it, or (mode "credit") more
    # than everything owed -- the overpaid card that closes in credit.
    pay = [r for r in rows if r["cat"] == "payment"]
    if pay:
        owed = opening_owed
        if o.get("closing") == "credit":
            debits = sum(r["amt"] for r in rows if r["dir"] == "D")
            other_c = sum(r["amt"] for r in rows if r["dir"] == "C" and r["cat"] != "payment")
            pay[0]["amt"] = owed + debits - other_c + rng.randint(2500, 8000)
        elif o.get("pay_full", True):
            pay[0]["amt"] = owed
        else:
            pay[0]["amt"] = max(2000, (owed // 3 // 100) * 100)
    C.settle(st)
    if o.get("closing") == "credit" and st["closing_owed"] >= 0:
        raise GenError("a statement meant to close in credit does not")
    limit = PRODUCTS[spec["product"]]["limit"]
    if o.get("closing") == "over":
        limit = ((st["closing_owed"] - rng.randint(5000, 40000)) // 50000) * 50000
        if limit <= 0 or st["closing_owed"] <= limit:
            raise GenError("cannot make an over-limit statement")
    else:
        while max(st["opening_owed"], st["closing_owed"]) > 0.8 * limit:
            limit += 100000
    st["limit"] = limit
    st["mode"] = o.get("closing", "min")
    return st


# ---------------------------------------------------------------------------
# Drawing.
# ---------------------------------------------------------------------------

def paginate(st):
    """The table as items placed on pages: (page, kind, y, payload)."""
    items, page, y = [], 0, TOP1
    allrows = st.get("draw_rows", st["rows"])
    secs = sorted({r["sec"] for r in allrows}) or [0]

    def new_page(sec, cont):
        nonlocal page, y
        page += 1
        y = TOP2
        if cont:
            items.append((page, "sechead", y, (sec, True)))
            y += 16
            items.append((page, "colhead", y, None))
            y += 16

    for sec in secs:
        rs = [r for r in allrows if r["sec"] == sec]
        first_h = (PITCH + CPITCH) if rs and len(rs[0]["lines"]) > 1 else PITCH
        if y + 32 + first_h > BOTTOM:
            new_page(sec, False)
        if sec:
            y += 8
        items.append((page, "sechead", y, (sec, False)))
        y += 16
        items.append((page, "colhead", y, None))
        y += 16
        if not rs:
            items.append((page, "none", y, None))
            y += PITCH
            continue
        for r in rs:
            h = PITCH + CPITCH * (len(r["lines"]) - 1)
            if y + h > BOTTOM:
                new_page(sec, True)
            items.append((page, "row", y, r))
            y += h
        if y + 16 > BOTTOM:
            new_page(sec, True)
        items.append((page, "total", y + 2, sec))
        y += 18
    return items, page + 1


def card_of(st, sec):
    return st["ident"]["cards"][sec]


def draw_first_header(sh, st, spec):
    idn = st["ident"]
    prod = PRODUCTS[spec["product"]]
    sh.text(LEFT, 46, "BNZ", 22, bold=True)
    sh.text(RIGHT, 44, prod["name"], 11, bold=True, align="right")
    y = 92
    sh.text(LEFT, y, idn["holder"], 9, bold=True)
    for a in idn["addr"]:
        y += 11
        sh.text(LEFT, y, a, 8.5)
    period = "Statement for the period %s to %s for card number %s" % (
        C.long_date(st["start"]), C.long_date(st["end"]), card_of(st, 0)[1])
    sh.text(LEFT, 152, period, 9, bold=True)
    sh.text(LEFT, 168, "Card terms and conditions apply, see www.bnz.co.nz/cardterms", 7.5)
    sh.text(LEFT, 182, "Name", 8, bold=True)
    sh.text(200, 182, "Card number", 8, bold=True)
    sh.text(LEFT, 194, idn["holder"], 8)
    sh.text(200, 194, card_of(st, 0)[1], 8)
    sh.text(LEFT, 206, prod["detail"], 8)
    sh.text(LEFT, 228, "Account summary", 9, bold=True)
    if VARIANT == "inline":
        draw_inline_summary(sh, st)
        return
    # The strip: four labels on one line, their figures on the next. The QVF reads
    # the opening as the 5th word after "Previous Balance" and stops at the NEXT $
    # figure, so the opening figure must be followed directly by another figure.
    strip = [("Previous Balance", C.card_balance(st["opening_owed"])),
             ("Credits", C.dollars(st["tot_c"])),
             ("Debits", C.dollars(st["tot_d"])),
             ("Current Balance", C.card_balance(st["closing_owed"]))]
    for k, (lab, val) in enumerate(strip):
        x = 160 + 110 * k
        sh.text(x, 244, lab, 8.5, bold=True, align="right")
        sh.text(x, 257, val, 8.5, align="right")
    sh.line(LEFT, 262, 490, 262, width=0.4)
    lines = [("Current Balance", C.card_balance(st["closing_owed"]), True)]
    owed = st["closing_owed"]
    due = C.add_months(st["end"], 1) - dt.timedelta(days=5)
    if st["mode"] == "credit" or owed <= 0:
        lines.append(("Please note there is no payment due this month.", "", False))
    else:
        if st["mode"] == "over":
            lines.append(("Over Limit/Overdue", C.dollars(owed - st["limit"]), False))
        mp = max(1000, min(owed, (owed * 3 // 100 // 100) * 100)) if owed > 0 else 0
        if st["mode"] == "over":
            mp += owed - st["limit"]
        lines.append(("Current Minimum Payment", C.dollars(mp), False))
        lines.append(("Payment Due Date", C.long_date(due), False))
    lines.append(("Credit Limit", C.dollars(st["limit"]), False))
    avail = max(0, st["limit"] - owed)
    lines.append(("Available Credit", C.dollars(avail), False))
    y = 278
    for lab, val, bold in lines:
        sh.text(LEFT, y, lab, 8.5, bold=bold)
        if val:
            sh.text(270, y, val, 8.5, bold=bold, align="right")
        y += 12
    if y > TOP1 - 10:
        raise GenError("summary runs into the table")


def draw_inline_summary(sh, st):
    """EXPERIMENT ONLY (--variant inline): every summary figure on its label's own line."""
    lines = [("Previous Balance", C.card_balance(st["opening_owed"])), ("Credits", C.dollars(st["tot_c"])),
             ("Debits", C.dollars(st["tot_d"])), ("Current Balance", C.card_balance(st["closing_owed"])),
             ("Credit Limit", C.dollars(st["limit"]))]
    y = 244
    for lab, val in lines:
        sh.text(LEFT, y, lab, 8.5)
        sh.text(270, y, val, 8.5, align="right")
        y += 12


def draw_cont_header(sh, st, spec):
    sh.text(LEFT, 44, "BNZ", 14, bold=True)
    sh.text(RIGHT, 44, PRODUCTS[spec["product"]]["name"], 9, bold=True, align="right")
    sh.text(RIGHT, 60, "Card number %s" % card_of(st, 0)[1], 8, align="right")


def draw_footer(sh, pno, npg, more):
    y = PH - 58
    sh.text(LEFT, y, "Page %d of %d" % (pno, npg), 8)
    if more:
        sh.text(RIGHT, y, "Continued over...", 8, align="right")
    sh.text(LEFT, PH - 46, "Please check this account carefully and tell us about anything that looks "
            "wrong within 30 days.", 6.5)
    sh.text(LEFT, PH - 37, "Synthetic test data for software testing. Not issued by Bank of New Zealand.", 6.5)
    C.draw_banner_text(sh, PH - 12)


def draw_item(sh, st, kind, y, payload):
    if kind == "sechead":
        sec, cont = payload
        name, num = card_of(st, sec)
        s = "%s - card number %s%s" % (name, num, " (continued)" if cont else "")
        sh.text(LEFT, y, s, 8.5, bold=True)
    elif kind == "colhead":
        sh.text(X_DATE, y, "Date", SIZE, bold=True)
        sh.text(X_DESC, y, "Transaction details", SIZE, bold=True)
        sh.text(X_DEB, y, "Debit Amount $", SIZE, bold=True, align="right")
        sh.text(X_CRE, y, "Credit Amount $", SIZE, bold=True, align="right")
        sh.line(LEFT, y + 4, RIGHT, y + 4, width=0.6)
    elif kind == "none":
        sh.text(X_DESC, y, "There are no transactions for this card this period.", SIZE)
    elif kind == "row":
        r = payload
        sh.text(X_DATE, y, "%d %s" % (r["date"].day, MON[r["date"].month - 1]), SIZE)
        for k, t in enumerate(r["lines"]):
            sh.text(X_DESC, y + k * CPITCH, t, SIZE)
        ym = y + CPITCH * (len(r["lines"]) - 1)
        if r["amt"] is not None:
            sh.text(X_DEB if r["dir"] == "D" else X_CRE, ym, C.dollars(r["amt"]), SIZE, align="right")
        r["drawn"] = True
    elif kind == "total":
        sec = payload
        rs = [r for r in st["rows"] if r["sec"] == sec]
        sh.text(X_DESC, y, "Total for card ending %s" % card_of(st, sec)[1][-4:], SIZE, bold=True)
        sh.text(X_DEB, y, C.dollars(sum(r["amt"] for r in rs if r["dir"] == "D")), SIZE, bold=True,
                align="right")
        sh.text(X_CRE, y, C.dollars(sum(r["amt"] for r in rs if r["dir"] == "C")), SIZE, bold=True,
                align="right")
        sh.line(X_DESC, y - 9, RIGHT, y - 9, width=0.4)


def render(sh, st, spec):
    items, npg = paginate(st)
    for p in range(npg):
        sh.begin_page()
        if p == 0:
            draw_first_header(sh, st, spec)
        else:
            draw_cont_header(sh, st, spec)
        for (pg, kind, y, payload) in items:
            if pg == p:
                draw_item(sh, st, kind, y, payload)
        draw_footer(sh, p + 1, npg, p + 1 < npg)
        sh.end_page()
    if any(not r.get("drawn") for r in st["rows"]):
        raise GenError("a row was never drawn")
    return npg


# ---------------------------------------------------------------------------
# The QVF replay (script.qvs lines 5472-5994, sBalances, sFindDeposits).
# ---------------------------------------------------------------------------

TIDY = dict(zip(C.MONTH, MON))
TIDY.update({"%d%s" % (k, "st" if k in (1, 21, 31) else "nd" if k in (2, 22) else "rd" if k in (3, 23)
             else "th"): str(k) for k in range(1, 32)})
DEPOSIT_WORDS = ["BNZ Cash Reward", "PAYMENT THANK YOU", "Purchase Return/Refund", "ACCOUNT FEE CREDIT",
                 "Credit", "CREDIT", "PAYMENT CHQ THANK YOU"]


def qvf_replay(words):
    ws = [TIDY.get(w, w) for w in words if w not in ("Continued", "over...")]
    n = len(ws)
    rep = {"statements_counted": ws.count("Statement"), "problems": []}
    # Periods: "the period" ... "for" 8 words on, the first after each "Statement".
    periods = []
    st_pos = [i for i, w in enumerate(ws) if w == "Statement"]
    for k, sp in enumerate(st_pos):
        nxt = st_pos[k + 1] if k + 1 < len(st_pos) else n
        hit = [i for i in range(sp, nxt) if ws[i] == "period" and ws[i - 1] == "the"
               and i + 8 < n and ws[i + 8] == "for"]
        periods.append(dict(sm=ws[hit[0] + 2], sy=ws[hit[0] + 3], em=ws[hit[0] + 6], ey=ws[hit[0] + 7])
                       if hit else None)
    # Opening (Opening_Balances_Step1/2 + sBalances): a start 4 words after each
    # "Previous Balance" (not "the Previous Balance"), paired with the next
    # "Credit Limit"; the figure is the word after the start up to the next $ word.
    ev = []
    for i in range(2, n):
        if ws[i].lower() == "balance" and ws[i - 1] == "Previous" and ws[i - 2] != "the":
            ev.append((i + 4, "S"))
        if ws[i].lower() == "limit" and ws[i - 1] == "Credit":
            ev.append((i, "E"))
    ev.sort()
    opens = []
    for k, (p0, kind) in enumerate(ev):
        if kind != "S" or k + 1 >= len(ev) or ev[k + 1][1] != "E":
            continue
        toks = []
        for j in range(p0 + 1, ev[k + 1][0]):
            if "$" in ws[j] and j > p0 + 1:
                break
            toks.append(ws[j])
        opens.append(C.qvf_balance(toks))
    # Closing (Closing_Balances_Step1-4): each "Current Balance" whose NEXT event is
    # an end ("Current Minimum", "Please note" after a $ word, "Over
    # Limit/Overdue"); the figure is everything between. The script's missing
    # brackets also make any word two after a "CR" an event (not an end), which
    # breaks a pair if it falls between.
    ev = []
    for i in range(2, n):
        w = ws[i]
        if w.lower() == "balance" and ws[i - 1] == "Current":
            ev.append((i, "B"))
        elif (w.lower() == "minimum" and ws[i - 1] == "Current") or \
                (w.lower() == "limit/overdue" and ws[i - 1] == "Over") or \
                (w.lower() == "note" and ((ws[i - 1] == "Please" and "$" in ws[i - 2]) or ws[i - 2] == "CR")):
            ev.append((i - 1, "E"))
        elif ws[i - 2] == "CR":
            ev.append((i, "X"))
    ev.sort()
    closes = []
    for k, (p0, kind) in enumerate(ev):
        if kind == "B" and k + 1 < len(ev) and ev[k + 1][1] == "E":
            closes.append(C.qvf_balance(ws[p0 + 1:ev[k + 1][0]]))
    rep["opening"], rep["closing"] = opens, closes
    # Account details (Account_Details_Step1-4): from 4 words after the terms URL to the
    # card type ("Y", "Lite", or "Advantage" after "BNZ") within 14 words; the number
    # is the last 19 characters, the name the rest.
    accts = []
    for i, w in enumerate(ws):
        if w == "www.bnz.co.nz/cardterms":
            ends = [j for j in range(i + 1, min(n, i + 14))
                    if ws[j] in ("Y", "Lite") or (ws[j] == "Advantage" and ws[j - 1] == "BNZ")]
            if ends:
                sub = " ".join(ws[i + 4:ends[0]])
                accts.append({"account_number": sub[-19:].strip(), "account_name": sub[:-19].strip()})
    rep["accounts"] = accts
    # Transaction data: from "Credit Amount $" to Page / Total / Our / BNZ-not-after-a-month.
    T, starts = [], []
    for i in range(2, n):
        if ws[i] == "$" and ws[i - 1].lower() == "amount" and ws[i - 2] == "Credit":
            j = i + 1
            while j < n and not (ws[j] in ("Page", "Total", "Our") or (ws[j] == "BNZ" and ws[j - 1] not in C.MONTHS)):
                j += 1
            starts.append((i, ws[i + 1:j]))
    if not starts:
        rep["problems"].append("no transaction heading found")
        return rep, []
    last = starts[-1][0]
    marks = [p for p in st_pos if p < last + len(starts[-1][1])]
    seq = sorted([(p, ["NS"]) for p in marks] + [(p, toks) for p, toks in starts])
    for _, toks in seq:
        T += toks
    rows = []
    n2 = len(T)
    stmt = 1

    def to_date(p):
        nonlocal stmt
        while True:
            p += 1
            if p >= n2:
                return None
            if T[p] == "NS":
                stmt += 1
            if T[p] in C.MONTHS:
                return p - 1

    def at(p):
        return T[p] if 0 <= p < n2 else None

    p = to_date(0)
    while p is not None and p < n2:
        if T[p] == "NS":
            stmt += 1
            p = to_date(p)
            continue
        s = []
        while True:                               # column 1: up to the month
            s.append(T[p])
            end = T[p] in C.MONTHS or p == n2 - 1
            p += 1
            if end:
                break
        date_s = " ".join(s)
        s, missing, rowend = [], False, False
        while p < n2:                             # column 2: up to a "$" word
            p1, p2 = at(p + 1), at(p + 2)
            end = False
            if p1 is not None and p1.startswith("$"):
                end = True
            elif p2 in C.MONTHS or p1 == "NS":
                missing = end = rowend = True
            if p == n2 - 1:
                end = rowend = True
            s.append(T[p])
            p += 1
            if end:
                break
        details = " ".join(s)
        amt_s = None
        if not rowend:
            s = []
            while p < n2:                         # column 3: up to the next date
                p1, p2 = at(p + 1), at(p + 2)
                end = p2 in C.MONTHS or p1 == "NS" or p == n2 - 1
                s.append(T[p])
                p += 1
                if end:
                    break
            amt_s = ",".join(s)
        if missing:
            rep["problems"].append("row dropped as 'missing amount': %s" % details)
            continue
        per = periods[stmt - 1] if stmt - 1 < len(periods) else None
        date = None
        if per:
            yr = per["sy"] if (per["sm"] in ("Oct", "Nov", "Dec") and
                               any(m in date_s[1:] for m in ("Oct", "Nov", "Dec"))) else per["ey"]
            try:
                dd, mm = date_s.split()
                date = dt.date(int(yr), MON.index(mm) + 1, int(dd)).isoformat()
            except (ValueError, IndexError):
                rep["problems"].append("date unreadable: %r" % date_s)
        amt = C.qlik_money(amt_s)
        if amt_s is not None and amt is None:
            rep["problems"].append("amount unreadable: %r" % amt_s)
        dep = any(w in details for w in DEPOSIT_WORDS)
        rows.append({"date": date, "details": details, "amt": amt, "deposit": dep, "stmt": stmt})
    verdicts = []
    for k in sorted({r["stmt"] for r in rows}):
        rs = [r for r in rows if r["stmt"] == k]
        o = opens[k - 1] if k - 1 < len(opens) else None
        c = closes[k - 1] if k - 1 < len(closes) else None
        verdicts.append(C.qvf_find_deposits(rs, o, c))
    rep["verdicts"] = verdicts
    return rep, rows


# ---------------------------------------------------------------------------
# The set.
# ---------------------------------------------------------------------------

SPECS = [
    dict(k=1, start=(2025, 2, 13), product="lite", n=[14], opening=(30000, 90000),
         opts=dict(interest=True),
         note="One cardholder, one page; full payment (PAYMENT THANK YOU), interest lines on the "
              "last day; separate Debit Amount $ and Credit Amount $ columns, every figure with a $."),
    dict(k=2, start=(2025, 4, 13), product="advantage", n=[22, 12], opening=(120000, 260000),
         opts=dict(fx=2, reward=True, ret=True, ret_sec=1, wrap=0.15, pay_full=False),
         note="Two cardholders, each section with its own heading and a 'Total for card ending' "
              "subtotal; foreign purchases with a USD/AUD line under the merchant and the NZ$ figure "
              "on that last line, each followed by an overseas transaction fee; BNZ Cash Reward and "
              "Purchase Return/Refund credits; part payment."),
    dict(k=3, start=(2024, 12, 13), product="lite", n=[20], opening=(60000, 150000),
         opts=dict(wrap=0.35, atm=2, interest=True, pay_full=False),
         note="Period crosses the new year (13 Dec 2024 to 12 Jan 2025) with year-less dd Mon dates; "
              "wrapped descriptions with the figure on the last line; BNZ ATM cash advances in round "
              "amounts, each with a cash advance fee; interest on purchases and on cash advances."),
    dict(k=4, start=(2025, 6, 13), product="lite", n=[12], opening=(40000, 90000),
         opts=dict(closing="credit", refund_nokw=1, insurance=True),
         note="Overpaid: the card closes IN CREDIT (Current Balance ... CR, then 'Please note'); a "
              "merchant refund printed with the merchant's name only (no deposit word); a DEBIT "
              "'CREDIT CARD REPAYMENT INSURANCE' whose words contain CREDIT."),
    dict(k=5, start=(2025, 7, 13), product="advantage", n=[[10], [9], [12]], bundle=3,
         opening=(50000, 120000), opts=dict(interest=False),
         note="Three consecutive monthly statements of one card in one PDF, each with its own "
              "'Statement for the period' header, summary and page numbering; balances chain."),
    dict(k=6, start=(2025, 9, 13), product="lite", n=[0], opening=(0, 0), opts=dict(payment=False),
         note="No transactions: previous and current balance $0.00, the table says there are none."),
    dict(k=7, start=(2025, 10, 13), product="advantage", n=[46, 26], opening=(250000, 400000),
         opts=dict(closing="over", fx=3, fx_all=True, wrap=0.2, interest=True, pay_full=False, atm=1),
         note="Over the credit limit: 'Over Limit/Overdue' follows Current Balance; two cardholders, "
              "three pages, the column heading repeated on every page; foreign purchases on both "
              "cards; a part payment."),
    dict(k=9, start=(2025, 11, 13), product="lite", n=[13], opening=(40000, 90000),
         opts=dict(info_line=True, interest=True),
         note="A dated line with no figure ('CARD REISSUED - REPLACEMENT CARD SENT'): not a money "
              "movement, so not in the key; the QVF drops such a line as a 'missing amount' row."),
    dict(k=8, start=(2026, 1, 13), product="lite", n=[11], opening=(-6000, -2500),
         opts=dict(payment=False, reward=True),
         note="Opens IN CREDIT (Previous Balance ... CR) and closes owing; no payment; a BNZ Cash "
              "Reward credit."),
]


def features_for(spec, sts, npages):
    f = ["bank:bnz", "cols:date|desc|debit|credit", "date:d Mon", "date_yearless", "font:Helvetica",
         "size:8", "page:A4", "pages:%d" % npages, "money:thousands", "money:dollar",
         "no_balance_column", "heading_every_page", "summary_box", "totals_line",
         "qvf:bnz_visa"]
    rows = [r for st in sts for r in st["rows"]]
    if any(st["start"].year != st["end"].year for st in sts):
        f.append("period_crosses_year")
    if len({r["sec"] for r in rows}) > 1:
        f.append("card_sections")
    if any(len(r["lines"]) > 1 for r in rows):
        f += ["multiline_desc", "staggered_amounts"]
    if any(r["cat"] == "fx" for r in rows):
        f.append("fx_in_description")
    if len(sts) > 1:
        f += ["bundle", "bundle:%d" % len(sts)]
    if not rows:
        f.append("no_transactions")
    if any(st["closing_owed"] < 0 for st in sts):
        f.append("closing_in_credit")
    if any(st["opening_owed"] < 0 for st in sts):
        f.append("opening_in_credit")
    if any(st["mode"] == "over" for st in sts):
        f.append("over_limit")
    if any(r.get("info") for st in sts for r in st.get("draw_rows", [])):
        f.append("dated_line_without_figure")
    if any(r["cat"] in ("refund_nokw", "insurance") for r in rows):
        f.append("qvf_keyword_trap")
    return f


def build(spec, out_dir):
    name = "bnz_visa_%d" % spec["k"]
    rng = C.seeded(name)
    holder = HOLDERS[(spec["k"] - 1) % len(HOLDERS)]
    ncards = len(spec["n"][0]) if isinstance(spec["n"][0], list) else len(spec["n"])
    base = "4%03d XXXX XXXX " % rng.randint(0, 999)
    cards = [(holder[0], base + "%04d" % rng.randint(0, 9999))]
    for s in range(1, ncards):
        cards.append((SECOND[(spec["k"] + s) % len(SECOND)], base + "%04d" % rng.randint(0, 9999)))
    ident = {"holder": holder[0], "addr": holder[1], "cards": cards}
    lo, hi = spec["opening"]
    owed = rng.randint(lo, hi) if hi != lo else lo
    sts = []
    for j in range(spec.get("bundle", 1)):
        st = build_statement(rng, spec, j, ident, owed)
        owed = st["closing_owed"]
        sts.append(st)
    pdf = os.path.join(out_dir, name + ".pdf")
    sh = C.Sheet(pdf, C.A4, "Helvetica")
    npages = 0
    for st in sts:
        npages += render(sh, st, spec)
    sh.save()
    truth = C.write_truth(
        out_dir, name, generator=GENERATOR,
        note="QVF lookalike (area visa2). BNZ, Visa credit card (synthetic). " + spec["note"],
        bank="BNZ", layout="bnz_visa", product="Visa credit card",
        features=features_for(spec, sts, npages), newest=False,
        statements=[{"rows": st["rows"], "opening_owed": st["opening_owed"],
                     "closing_owed": st["closing_owed"], "start": st["start"], "end": st["end"]}
                    for st in sts])
    if VARIANT != "strip":
        return name, truth, npages, {"right": 0, "wrong_sign": 0, "unidentified": 0}, {}
    # Read it back the way the Qlik script does.
    rep, qrows = qvf_replay(C.words_in_order(pdf))
    want_o = [-st["opening_owed"] for st in sts]
    want_c = [-st["closing_owed"] for st in sts]
    if rep["statements_counted"] != len(sts):
        raise GenError("%s: the QVF would count %d statements, not %d" % (name, rep["statements_counted"], len(sts)))
    if rep.get("opening") != want_o or rep.get("closing") != want_c:
        raise GenError("%s: QVF balances %s/%s, truth %s/%s" % (name, rep.get("opening"), rep.get("closing"),
                                                                  want_o, want_c))
    cmp_ = C.compare_qvf(truth["rows"], qrows)
    if cmp_["missing"] or cmp_["unreadable"] or cmp_["extra"] or cmp_["date_wrong"]:
        raise GenError("%s: the QVF replay does not read every row: %s %s" % (name, cmp_, rep["problems"]))
    rep["compare"] = cmp_
    C.save_report(out_dir, name, rep)
    return name, truth, npages, cmp_, rep


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=C.DEFAULT_OUT)
    ap.add_argument("--variant", default="strip", choices=["strip", "inline"],
                    help="inline: an experiment layout (summary figures beside their labels); never the set")
    a = ap.parse_args()
    global VARIANT
    VARIANT = a.variant
    if VARIANT != "strip" and os.path.abspath(a.out) == os.path.abspath(C.DEFAULT_OUT):
        sys.exit("an experiment variant must not be written into the set")
    os.makedirs(a.out, exist_ok=True)
    for spec in SPECS:
        name, truth, npages, cmp_, rep = build(spec, a.out)
        print("%-12s %2d rows %d page(s)  QVF replay: right %d, wrong sign %d, Unidentified %d  [%s]"
              % (name, truth["row_count"], npages, cmp_["right"], cmp_["wrong_sign"], cmp_["unidentified"],
                 "; ".join(rep.get("verdicts", []))))
    return 0


if __name__ == "__main__":
    sys.exit(main())
