#!/usr/bin/env python3
"""make_kiwibank_cc.py -- lookalike Kiwibank credit card statements, laid out the
way the QVF "Statement Converter" Kiwibank Credit Card section (script.qvs lines
5995-6496) expects them, each with an answer key in make_layouts.py's format.

What the script walks, and so what is printed here (see cards/kiwibank_cc.md):
  * "Credit limit" exactly once per statement (every "credit limit" starts one);
  * "Your card number <number> Your account name <name>" then "Statement ..." (or
    "Closing ...") ends the name;
  * "Opening balance <figure> [CR]" closed by the next $ figure or by "Total",
    "Thank", "Kiwibank" or "Includes"; "Closing balance <figure> [CR]" (not
    straight after the word "Debits") closed by "If" or "Interest";
  * the table: ONE heading per statement ending "Details Credit Debit" (or
    "credit amount Debit amount"), Credit BEFORE Debit, read until the first
    "TOTALS", "Totals", "Air", "Low" or "Statement" printed straight after a figure;
  * a row = transaction date dd/mm/yy, date processed dd/mm/yy, the card's last four
    digits (dropped by the script), details, then ONE "$" figure in the Credit or the
    Debit column, followed directly by the next row's date -- so a wrapped row prints
    its figure on its last line;
  * a page break is either "Continued over Page n of N" as the last words of the
    page or "Page n of N" as the first words of the next: the next page carries no
    other text before its rows (no repeated heading);
  * foreign purchases are followed by their own "Currency Conv Assessment" and
    "Foreign Currency Txn Fee" rows.

Run:  python3 make_kiwibank_cc.py [--out DIR]     (default ../sets/visa2)
"""

import argparse
import datetime as dt
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import visa2_common as C  # noqa: E402
from visa2_common import GenError, ML, PH  # noqa: E402

GENERATOR = "scratchpad/qvf/gen/make_kiwibank_cc.py"
LEFT, RIGHT = 40.0, 553.0
X_TD, X_PD, X_CARD, X_DESC, X_CR, X_DR = 42.0, 90.0, 140.0, 170.0, 487.0, 553.0
SIZE = 7.5
PITCH, CPITCH = 11.0, 8.8
TOP1, TOP2, BOTTOM = 372.0, 62.0, PH - 58.0
DESC_W = X_CR - 70.0 - X_DESC

PRODUCTS = {
    "low": dict(name="Low Rate Mastercard", rates="Interest rates: purchases 13.95% p.a., cash advances 21.95% p.a.",
                limit=300000, after="Low Rate Mastercard: pay the closing balance in full by the due date "
                                    "to avoid interest on purchases."),
    "air": dict(name="Air New Zealand Airpoints Mastercard",
                rates="Interest rates: purchases 20.95% p.a., cash advances 22.95% p.a.", limit=800000,
                after="Air New Zealand Airpoints Dollars earned this period: {pts}"),
}
HOLDERS = [("J SAMPLE", ["12 EXAMPLE STREET", "SAMPLEVILLE", "TESTBURY 9010"]),
           ("MS A EXAMPLE", ["FLAT 3", "45 SPECIMEN ROAD", "DEMOTOWN 7020"]),
           ("MR P TESTER", ["7 PLACEHOLDER LANE", "RD 2", "MOCKBURN 9310"]),
           ("R DEMO", ["101 TEMPLATE TERRACE", "TESTBURY 9011"])]

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


def row(rng, d, lines, spec, cat, vals=None):
    vals = {} if vals is None else vals
    text = [ML.fit(lambda s: ML.stringWidth(s, "Helvetica", SIZE), C.fill(rng, t, vals), DESC_W)
            for t in lines]
    amt = C.amount_for(rng, spec, vals) if spec else None
    return {"dir": d, "lines": text, "amt": amt, "cat": cat}


def pick(rng, table):
    return rng.choices(table, weights=[w for w, _, _ in table])[0]


def build_statement(rng, spec, j, ident, opening_owed):
    y, m, d = spec["start"]
    start = C.add_months(dt.date(y, m, d), j)
    end = C.add_months(start, 1) - dt.timedelta(days=1)
    o = dict(spec.get("opts", {}))
    n = spec["n"][j] if isinstance(spec["n"], list) else spec["n"]
    cards = ident["cards"]
    items = []                                   # (kind, [rows]) -- a kind "pair" keeps its rows together
    if o.get("payment", True) and opening_owed > 0:
        items.append(("pay", [row(rng, "C", [o.get("pay_words", "Payment Thankyou")], None, "payment")]))
    for _ in range(o.get("fx", 0)):
        vals = {}
        r = row(rng, "D", ["{online} SAN FRANCISCO USD {fx}"], ("fx",), "fx", vals)
        cca = row(rng, "D", ["Currency Conv Assessment"], ("fixed", max(5, round(r["amt"] * 0.01)) / 100.0), "fxfee")
        fee = row(rng, "D", ["Foreign Currency Txn Fee"], ("fixed", max(5, round(r["amt"] * 0.015)) / 100.0),
                  "fxfee")
        items.append(("pair", [r, cca, fee]))
    if o.get("refund"):
        items.append(("any", [row(rng, "C", ["Refund {online}"], ("r", 8, 90), "refund")]))
    if o.get("refund_nokw"):
        items.append(("any", [row(rng, "C", ["{grocer} {city}"], ("r", 20, 140), "refund_nokw")]))
    if o.get("fee_rev"):
        items.append(("any", [row(rng, "C", ["Foreign Curr Txn Fee Rev"], ("r", 0.3, 1.5), "fxrev")]))
    if o.get("int_adj"):
        items.append(("any", [row(rng, "C", ["Interest Adjustment"], ("r", 0.5, 6), "intadj")]))
    k_plain = n - sum(len(rs) for _, rs in items) - (1 if o.get("interest") else 0)
    if k_plain < 0:
        raise GenError("too many forced rows")
    for _ in range(k_plain):
        if rng.random() < o.get("wrap", 0.0):
            _, sp, lines = pick(rng, WRAP_BUY)
        else:
            _, sp, lines = pick(rng, BUY)
        items.append(("any", [row(rng, "D", lines, sp, "buy")]))
    # Processed dates over the period; the transaction a day or so earlier (it can
    # fall before the period starts). Printed in processed-date order.
    days = (end - start).days
    dated = []
    for kind, rs in items:
        pd = start + dt.timedelta(days=rng.randint(1, min(8, days)) if kind == "pay" else rng.randint(0, days))
        td = pd - dt.timedelta(days=0 if kind == "pay" else rng.choice([0, 0, 1, 1, 2, 3]))
        card = cards[0][1][-4:] if kind == "pay" or len(cards) == 1 else rng.choice(cards)[1][-4:]
        for r in rs:
            r["pdate"], r["date"], r["card"] = pd, td, card
        dated.append((pd, kind, rs))
    dated.sort(key=lambda x: x[0])
    rows = [r for _, _, rs in dated for r in rs]
    if o.get("interest"):
        rows.append(dict(row(rng, "D", ["INTEREST CHARGED ON PURCHASES"], ("r", 4, 45), "interest"),
                         pdate=end, date=end, card=cards[0][1][-4:]))
    for r in rows:
        r["desc"] = " ".join(r["lines"])
    st = {"start": start, "end": end, "opening_owed": opening_owed, "rows": rows, "ident": ident}
    pay = [r for r in rows if r["cat"] == "payment"]
    if pay:
        if o.get("closing") == "credit":
            debits = sum(r["amt"] for r in rows if r["dir"] == "D")
            other_c = sum(r["amt"] for r in rows if r["dir"] == "C" and r["cat"] != "payment")
            pay[0]["amt"] = opening_owed + debits - other_c + rng.randint(1500, 6000)
        elif o.get("pay_full", True):
            pay[0]["amt"] = opening_owed
        else:
            pay[0]["amt"] = max(2000, (opening_owed // 3 // 100) * 100)
    C.settle(st)
    if o.get("closing") == "credit" and st["closing_owed"] >= 0:
        raise GenError("meant to close in credit")
    if o.get("newest"):
        st["rows"] = st["rows"][::-1]
    limit = PRODUCTS[spec["product"]]["limit"]
    while max(st["opening_owed"], st["closing_owed"]) > 0.8 * limit:
        limit += 100000
    st["limit"] = limit
    return st


# ---------------------------------------------------------------------------
# Drawing.
# ---------------------------------------------------------------------------

def paginate(st, pagebreak):
    items, page, y = [], 0, TOP1
    rows = st["rows"]
    if not rows:
        items.append((0, "none", y, None))
        items.append((0, "totals", y + PITCH + 3, None))
        return items, 1
    for r in rows:
        h = PITCH + CPITCH * (len(r["lines"]) - 1)
        if y + h > BOTTOM:
            page += 1
            y = TOP2 + (14 if pagebreak == "top" else 0)
        items.append((page, "row", y, r))
        y += h
    if st.get("totals", True):
        if y + PITCH + 3 > BOTTOM:
            page += 1
            y = TOP2
        items.append((page, "totals", y + 3, None))
        y += PITCH + 3
    items.append((page, "after", y + 14, None))
    return items, page + 1


def draw_first_header(sh, st, spec, head_variant):
    idn = st["ident"]
    prod = PRODUCTS[spec["product"]]
    C.draw_banner_text(sh, 14)
    sh.text(LEFT, 46, "Kiwibank", 20, bold=True)
    sh.text(RIGHT, 44, prod["name"], 11, bold=True, align="right")
    sh.text(RIGHT, 58, "Credit card statement", 8.5, align="right")
    y = 92
    sh.text(LEFT, y, idn["holder"], 9, bold=True)
    for a in idn["addr"]:
        y += 11
        sh.text(LEFT, y, a, 8.5)
    det = [("Your card number", idn["cards"][0][1]), ("Your account name", idn["holder"]),
           ("Statement date", C.long_date(st["end"])),
           ("Statement period", "%s to %s" % (C.long_date(st["start"]), C.long_date(st["end"]))),
           ("Credit limit", C.dollars(st["limit"])),
           ("Available credit", C.dollars(max(0, st["limit"] - st["closing_owed"])))]
    y = 152
    for lab, val in det:
        sh.text(LEFT, y, lab, 8.5)
        sh.text(170, y, val, 8.5)
        y += 11.5
    sh.text(LEFT, 232, "Account summary", 9, bold=True)
    owed = st["closing_owed"]
    summ = [("Opening balance", C.card_balance(st["opening_owed"]), False),
            ("Total credits", C.dollars(st["tot_c"]), False),
            ("Total debits", C.dollars(st["tot_d"]), False),
            ("Closing balance", C.card_balance(owed), True)]
    y = 246
    for lab, val, b in summ:
        sh.text(LEFT, y, lab, 8.5, bold=b)
        sh.text(300, y, val, 8.5, bold=b, align="right")
        y += 11.5
    sh.text(LEFT, y, prod["rates"], 7.5)
    y += 11.5
    mp = 0 if owed <= 0 else max(1000, min(owed, (owed * 3 // 100 // 100) * 100))
    due = C.add_months(st["end"], 1) - dt.timedelta(days=4)
    sh.text(LEFT, y, "Minimum payment", 8.5)
    sh.text(300, y, C.dollars(mp), 8.5, align="right")
    y += 11.5
    sh.text(LEFT, y, "Payment due date", 8.5)
    sh.text(300, y, C.long_date(due) if mp else "No payment due", 8.5, align="right")
    # The heading: two lines, the money columns' words on the second, Credit first.
    hy = TOP1 - 22
    sh.text(X_TD, hy, "Transaction", SIZE, bold=True)
    sh.text(X_PD, hy, "Date", SIZE, bold=True)
    hy2 = hy + 9
    sh.text(X_TD, hy2, "date", SIZE, bold=True)
    sh.text(X_PD, hy2, "processed", SIZE, bold=True)
    sh.text(X_CARD, hy2, "Card", SIZE, bold=True)
    sh.text(X_DESC, hy2, "Details", SIZE, bold=True)
    if head_variant == "amount":
        sh.text(X_CR, hy2, "Credit amount", SIZE, bold=True, align="right")
        sh.text(X_DR, hy2, "Debit amount", SIZE, bold=True, align="right")
    else:
        sh.text(X_CR, hy2, "Credit", SIZE, bold=True, align="right")
        sh.text(X_DR, hy2, "Debit", SIZE, bold=True, align="right")
    sh.line(LEFT, hy2 + 4, RIGHT, hy2 + 4, width=0.6)


def fmt(d):
    return "%02d/%02d/%02d" % (d.day, d.month, d.year % 100)


def draw_item(sh, st, spec, kind, y, payload):
    if kind == "row":
        r = payload
        sh.text(X_TD, y, fmt(r["date"]), SIZE)
        sh.text(X_PD, y, fmt(r["pdate"]), SIZE)
        sh.text(X_CARD, y, r["card"], SIZE)
        for k, t in enumerate(r["lines"]):
            sh.text(X_DESC, y + k * CPITCH, t, SIZE)
        ym = y + CPITCH * (len(r["lines"]) - 1)
        sh.text(X_CR if r["dir"] == "C" else X_DR, ym, C.dollars(r["amt"]), SIZE, align="right")
        r["drawn"] = True
    elif kind == "none":
        sh.text(X_DESC, y, "There are no transactions for this period.", SIZE)
    elif kind == "totals":
        sh.line(X_DESC, y - 8, RIGHT, y - 8, width=0.4)
        sh.text(X_DESC, y, "TOTALS", SIZE, bold=True)
        sh.text(X_CR, y, C.dollars(st["tot_c"]), SIZE, bold=True, align="right")
        sh.text(X_DR, y, C.dollars(st["tot_d"]), SIZE, bold=True, align="right")
    elif kind == "after":
        s = PRODUCTS[spec["product"]]["after"].format(pts=max(0, st["tot_d"] // 10000))
        sh.text(LEFT, y, s, 7.5)


def render(sh, st, spec):
    o = spec.get("opts", {})
    pagebreak = o.get("pagebreak", "bottom")
    st["totals"] = o.get("totals", True)
    items, npg = paginate(st, pagebreak)
    for p in range(npg):
        sh.begin_page()
        last = p == npg - 1
        if p == 0:
            draw_first_header(sh, st, spec, o.get("head", "plain"))
        else:
            C.draw_banner_image(sh, 8)
            if pagebreak == "top":
                sh.text(RIGHT, TOP2 - 6, "Page %d of %d" % (p + 1, npg), 7.5, align="right")
        for (pg, kind, y, payload) in items:
            if pg == p:
                draw_item(sh, st, spec, kind, y, payload)
        if last:
            if npg > 1 and pagebreak == "top":
                pass
            else:
                sh.text(RIGHT, PH - 40, "Page %d of %d" % (p + 1, npg), 7.5, align="right")
            sh.text(LEFT, PH - 30, "Please check your transactions and tell us within 30 days if anything "
                    "looks wrong.", 6.5)
            sh.text(LEFT, PH - 22, "Synthetic test data for software testing. Not issued by Kiwibank Limited.",
                    6.5)
            C.draw_banner_text(sh, PH - 8)
        else:
            # The last words on this page; the next page's rows follow them directly.
            if pagebreak == "bottom":
                sh.text(RIGHT, PH - 40, "Continued over Page %d of %d" % (p + 1, npg), 7.5, align="right")
            C.draw_banner_image(sh, PH - 22)
        sh.end_page()
    if any(not r.get("drawn") for r in st["rows"]):
        raise GenError("a row was never drawn")
    return npg


# ---------------------------------------------------------------------------
# The QVF replay (script.qvs lines 5995-6496, sBalances, sLoopToDate 'full',
# sFindDeposits, sEndofData's date re-sort).
# ---------------------------------------------------------------------------

DEPOSIT_WORDS = ["Payment Thankyou", "Payment Received", "Assmnt Rev", "Fee Rev", "Interest Adjustment",
                 "Account Fee Credit", "Refund", "CC Pment", "Creditpment", "Creditcar",
                 "Currency Conv Assmnt Rev", "Foreign Curr Txn Fee Rev"]
NOT_CANDIDATES = ("Currency Conv Assessment", "Foreign Currency Txn Fee")


def isnum(s):
    return bool(s) and bool(re.match(r"^-?\$?[\d,]*\.?\d+$|^\d{1,2}/\d{1,2}/\d{2,4}$", s))


def is_deposit(details):
    if any(w in details for w in DEPOSIT_WORDS):
        return True
    k = details.rfind("-")
    if k >= 0:
        left = details[:k + 1]
        r3 = left[-3:]
        return len([c for c in r3 if c.isdigit()]) > 1 and left[-4:-3] == "."
    return False


def qvf_replay(ws):
    n = len(ws)
    rep = {"problems": []}
    ns = [i for i in range(1, n) if ws[i] == "limit" and ws[i - 1].lower() == "credit"]
    rep["statements_counted"] = len(ns)

    def pairs(ev):
        ev.sort()
        return [(p0, ev[k + 1][0]) for k, (p0, kind) in enumerate(ev) if kind == "S" and k + 1 < len(ev)]
    ev = []
    for i in range(1, n):
        if (ws[i] == "balance" and ws[i - 1] == "Opening") or (ws[i] == "Due" and ws[i - 1] == "Current"):
            ev.append((i, "S"))
        elif ws[i] in ("Thank", "Kiwibank", "Total", "Includes"):
            ev.append((i, "E"))
    opens = []
    for p0, p1 in pairs(ev):
        toks = []
        for j in range(p0 + 1, p1):
            if "$" in ws[j] and j > p0 + 1:
                break
            toks.append(ws[j])
        opens.append(C.qvf_balance(toks))
    ev = []
    for i in range(2, n):
        if ws[i] == "balance" and ws[i - 1] == "Closing" and ws[i - 2] != "Debits":
            ev.append((i, "S"))
        elif ws[i] in ("If", "Interest"):
            ev.append((i, "E"))
    closes = [C.qvf_balance(ws[p0 + 1:p1]) for p0, p1 in pairs(ev)]
    rep["opening"], rep["closing"] = opens, closes
    # Account name: after "account name" up to "Statement" or "Closing"; number: after
    # the last "card number"/"account number" before it, less "Your" and "account".
    accts = []
    for i in range(1, n):
        if ws[i] == "name" and ws[i - 1] == "account":
            j = i + 1
            while j < n and ws[j] not in ("Statement", "Closing"):
                j += 1
            nums = [k for k in range(1, i) if ws[k] == "number" and ws[k - 1] in ("account", "card")]
            num = " ".join(w for w in ws[nums[-1] + 1:i] if w not in ("Your", "account")) if nums else None
            accts.append({"account_name": " ".join(ws[i + 1:j]), "account_number": num})
    rep["accounts"] = accts
    # Transaction blocks: a heading start paired with the first end marker after it.
    marks = []
    for i in range(2, n):
        if ws[i] == "Debit" and ws[i - 1] in ("Credit", "amount") and ws[i - 2].lower() in ("details", "detail", "credit"):
            marks.append((i, "S"))
        elif ws[i] in ("Air", "TOTALS", "Statement", "Totals", "Low") and "." in ws[i - 1]:
            marks.append((i, "E"))
    marks.sort()
    seq = []
    for k, (p0, kind) in enumerate(marks):
        if kind == "S" and k + 1 < len(marks) and marks[k + 1][1] == "E":
            toks = list(range(p0 + 1, marks[k + 1][0]))
            if toks and ws[toks[0]] == "amount":
                toks = toks[1:]
            keep = []
            for j in toks:
                w, prev = ws[j], ws[j - 1]
                if len(w) == 4 and w.isdigit() and "/" in prev and "$" not in w:
                    continue
                keep.append((j, w))
            seq += keep
    if not seq:
        rep["problems"].append("no transaction block")
        return rep, []
    last = seq[-1][0]
    seq += [(p, "NS") for p in ns if p < last]
    seq.sort()
    T = [w for _, w in seq]
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
            if "/" in T[p]:
                return p

    def at(p):
        return T[p] if 0 <= p < n2 else ""

    rows = []
    p = to_date(0)
    while p is not None and p < n2:
        if T[p] == "NS":
            stmt += 1
            p = to_date(p)
            continue
        date_s = T[p]
        p += 2                                     # skip the date processed
        s, rowend = [], False
        while p < n2:
            p1, p2, p3 = at(p + 1), at(p + 2), at(p + 3)
            end = (p1.startswith("$") and T[p] != "of" and p3 != "Includes"
                   and ("/" in p2 or isnum(p2) or p2 in ("Continued", "Page", "NS"))) or p + 1 == n2 - 1
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
            while p < n2:
                p1 = at(p + 1)
                cur = T[p]
                end = "/" in p1 or p1 in ("NS", "Continued", "Page")
                if p1 == "Continued":
                    p += 6
                elif p1 == "Page":
                    p += 4
                if p >= n2 - 1:
                    end = True
                s.append(cur)
                p += 1
                if end:
                    break
            amt_s = ",".join(x.replace("$", "") for x in s)
        try:
            date = dt.datetime.strptime(date_s, "%d/%m/%y").date().isoformat()
        except ValueError:
            date = None
            rep["problems"].append("date unreadable: %r" % date_s)
        amt = C.qlik_money(amt_s)
        if amt_s is not None and amt is None:
            rep["problems"].append("amount unreadable: %r (%s)" % (amt_s, details))
        rows.append({"date": date, "details": details, "amt": amt, "deposit": is_deposit(details), "stmt": stmt})
    verdicts = []
    for k in sorted({r["stmt"] for r in rows}):
        rs = [r for r in rows if r["stmt"] == k]
        o = opens[k - 1] if k - 1 < len(opens) else None
        c = closes[k - 1] if k - 1 < len(closes) else None
        verdicts.append(C.qvf_find_deposits(rs, o, c, exclude_exact=NOT_CANDIDATES))
    rep["verdicts"] = verdicts
    return rep, rows


# ---------------------------------------------------------------------------
# The set.
# ---------------------------------------------------------------------------

SPECS = [
    dict(k=1, start=(2025, 2, 15), product="low", n=14, cards=1, opening=(30000, 90000),
         opts=dict(interest=False),
         note="One card, one page: transaction date, date processed, card digits, details, then "
              "Credit and Debit columns (Credit first), every figure with a $; a TOTALS row; full "
              "payment ('Payment Thankyou')."),
    dict(k=2, start=(2025, 4, 15), product="low", n=48, cards=2, opening=(150000, 280000),
         opts=dict(fx=2, refund=True, pay_full=False, interest=True),
         note="Two cards in one list (the Card column says whose), two pages: 'Continued over Page 1 "
              "of 2' ends page 1 and page 2 starts straight on its rows with no heading; foreign "
              "purchases each followed by Currency Conv Assessment and Foreign Currency Txn Fee rows; "
              "a Refund; part payment; interest."),
    dict(k=3, start=(2025, 6, 15), product="low", n=16, cards=1, opening=(40000, 120000),
         opts=dict(newest=True, pay_words="Payment Received"),
         note="Newest first (reverse date order), as some Kiwibank card statements print; the "
              "script re-sorts these by date."),
    dict(k=4, start=(2024, 12, 15), product="low", n=18, cards=1, opening=(50000, 110000),
         opts=dict(closing="credit", refund_nokw=True, fee_rev=True, int_adj=True, fx=1),
         note="Period crosses the new year (15 Dec 2024 to 14 Jan 2025), a transaction dated before the "
              "period started; overpaid, so it closes IN CREDIT (Closing balance ... CR); a merchant "
              "refund with no deposit word; Foreign Curr Txn Fee Rev and Interest Adjustment credits."),
    dict(k=5, start=(2025, 8, 15), product="low", n=[11, 9, 13], bundle=3, cards=1, opening=(40000, 90000),
         opts=dict(),
         note="Three consecutive monthly statements in one PDF, each with its own Credit limit, summary, "
              "heading and TOTALS; balances chain."),
    dict(k=6, start=(2025, 10, 15), product="low", n=0, cards=1, opening=(-1500, -1500),
         opts=dict(payment=False),
         note="No transactions: the card is $15.00 in credit at the start and at the end; TOTALS "
              "$0.00 $0.00."),
    dict(k=7, start=(2025, 11, 15), product="air", n=50, cards=2, opening=(180000, 350000),
         opts=dict(head="amount", totals=False, wrap=0.3, pagebreak="top", interest=True, pay_full=False,
                   pay_words="J SAMPLE CC Pment", fx=1),
         note="Airpoints card: heading 'Credit amount / Debit amount', no TOTALS row (the table ends at "
              "the Airpoints line), wrapped descriptions with the figure on the last line, two cards, two "
              "pages with 'Page 2 of 2' as the first words of page 2, a payment from another bank whose "
              "narrative is cut to 'CC Pment', interest."),
]


def features_for(spec, sts, npages):
    o = spec.get("opts", {})
    f = ["bank:kiwibank", "cols:date|pdate|code|desc|credit|debit", "date:dd/mm/yy", "font:Helvetica",
         "size:7.5", "page:A4", "pages:%d" % npages, "money:thousands", "money:dollar", "no_balance_column",
         "heading_page1_only", "two_line_heading", "summary_box", "card_digits_column", "qvf:kiwibank_cc"]
    rows = [r for st in sts for r in st["rows"]]
    if o.get("totals", True):
        f.append("totals_line")
    if any(st["start"].year != st["end"].year for st in sts):
        f.append("period_crosses_year")
    if any(r["date"] < st["start"] for st in sts for r in st["rows"]):
        f.append("date_before_period")
    if spec.get("cards", 1) > 1:
        f.append("several_cards_one_list")
    if any(len(r["lines"]) > 1 for r in rows):
        f += ["multiline_desc", "staggered_amounts"]
    if any(r["cat"] == "fx" for r in rows):
        f += ["fx_in_description", "fx_fee_rows"]
    if o.get("newest"):
        f.append("newest_first")
    if len(sts) > 1:
        f += ["bundle", "bundle:%d" % len(sts)]
    if not rows:
        f.append("no_transactions")
    if any(st["closing_owed"] < 0 for st in sts):
        f.append("closing_in_credit")
    if any(st["opening_owed"] < 0 for st in sts):
        f.append("opening_in_credit")
    if any(r["cat"] == "refund_nokw" for r in rows):
        f.append("qvf_keyword_trap")
    if npages > len(sts):
        f.append("pagebreak:" + o.get("pagebreak", "bottom"))
    return f


def build(spec, out_dir):
    name = "kiwibank_cc_%d" % spec["k"]
    rng = C.seeded(name)
    holder = HOLDERS[(spec["k"] - 1) % len(HOLDERS)]
    base = "5%03d XXXX XXXX " % rng.randint(100, 599)
    cards = [(holder[0], base + "%04d" % rng.randint(0, 9999)) for _ in range(spec.get("cards", 1))]
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
    npages = sum(render(sh, st, spec) for st in sts)
    sh.save()
    newest = bool(spec.get("opts", {}).get("newest"))
    truth = C.write_truth(
        out_dir, name, generator=GENERATOR,
        note="QVF lookalike (area visa2). Kiwibank, Mastercard credit card (synthetic). " + spec["note"],
        bank="Kiwibank", layout="kiwibank_cc", product="Mastercard",
        features=features_for(spec, sts, npages), newest=newest,
        statements=[{"rows": st["rows"], "opening_owed": st["opening_owed"],
                     "closing_owed": st["closing_owed"], "start": st["start"], "end": st["end"]}
                    for st in sts])
    rep, qrows = qvf_replay(C.words_in_order(pdf))
    want_o = [-st["opening_owed"] for st in sts]
    want_c = [-st["closing_owed"] for st in sts]
    if rep["statements_counted"] != len(sts):
        raise GenError("%s: QVF counts %d statements" % (name, rep["statements_counted"]))
    if rep["opening"] != want_o or rep["closing"] != want_c:
        raise GenError("%s: QVF balances %s/%s, truth %s/%s" % (name, rep["opening"], rep["closing"], want_o, want_c))
    cmp_ = C.compare_qvf(truth["rows"], qrows, sort=True)
    if cmp_["missing"] or cmp_["unreadable"] or cmp_["extra"] or cmp_["date_wrong"]:
        raise GenError("%s: the QVF replay does not read every row: %s %s" % (name, cmp_, rep["problems"][:5]))
    rep["compare"] = cmp_
    C.save_report(out_dir, name, rep)
    return name, truth, npages, cmp_, rep


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=C.DEFAULT_OUT)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    for spec in SPECS:
        name, truth, npages, cmp_, rep = build(spec, a.out)
        print("%-14s %2d rows %d page(s)  QVF replay: right %d, wrong sign %d, Unidentified %d  [%s]"
              % (name, truth["row_count"], npages, cmp_["right"], cmp_["wrong_sign"], cmp_["unidentified"],
                 "; ".join(rep.get("verdicts", []))))
    return 0


if __name__ == "__main__":
    sys.exit(main())
