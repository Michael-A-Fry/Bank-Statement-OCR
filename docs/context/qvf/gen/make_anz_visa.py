#!/usr/bin/env python3
"""make_anz_visa.py -- ANZ Visa (QVF type "ANZ - Visa", type key anz_visa)
lookalike statements, drawn to the shape the QVF load script expects
(script.qvs lines 4120-5048), each with an answer key in exactly the format
tools/synth/make_layouts.py writes.

    python3 make_anz_visa.py [--out DIR]      (default: ../sets/visa1)

What the QVF needs, and so what every page here carries (see cards/anz_visa.md):
  * "Statement Period dd Mon yy - dd Mon yy" followed by a "Credit" word
    (Credit Limit): the period, 2-digit years, one per statement (4179-4216);
  * "Account Number <card> Account Name <name>" ended by "Closing"
    (4265-4292, 4809-4815);
  * "Closing Balance <amount> Minimum Payment" in the page-1 panel (4218-4263);
  * a table "Date | Processed | Transaction details | Amount in NZ$", the
    heading followed by "Page k of N" (4376-4397), two dates per row of which
    the second (processed) is dropped (4139-4161), row amounts with NO thousands
    comma (the details-end test at 4947 only accepts digits and one point),
    credits marked CR (4979-4993);
  * "Opening Balance" as the first table line (skipped at 4878);
  * one "Card Number <card> <NAME>" heading per cardholder (stripped at 4685-4743),
    optionally a "Card Total" line, then a "Sundry Transactions" block;
  * foreign-currency purchases on two lines, the second "Incl Currency
    Conversion Charge <x>" (re-ordered at 4745-4795);
  * continuation pages headed "Credit Card Account Number <card>" (an end marker
    at 4353 / 4512, not counted as an account number at 4278).
"""

import argparse
import datetime as dt
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import visa1_common as C  # noqa: E402
from visa1_common import ML, GenError, ddmon, ddmonyy  # noqa: E402

GEN = "qvf/gen/make_anz_visa.py"
BANNER = ML.SYNTHETIC
SIZE = 8.0
PITCH = 12.5
FXP = 10.0
X_DATE, X_PDATE, X_DESC = 42.0, 84.0, 130.0
X_DOLLAR = 410.0
AMT_R = 487.0
RIGHT = 555.0
BOTTOM = 805.0


# ---------------------------------------------------------------------------
# The statements: what each one tests.
# ---------------------------------------------------------------------------

SPECS = [
    dict(name="anz_visa_1", start=(2025, 3, 8), sections=[14], sundry=["pay", "int"],
         note="Plain month: one cardholder, purchases and a refund under the card heading, "
              "a payment and interest under Sundry Transactions."),
    dict(name="anz_visa_2", start=(2025, 6, 14), sections=[20, 11], fx=[3, 1], fx_refund=1,
         card_totals=True, big=1, sundry=["pay", "int", "fee"],
         note="Two cardholders, each under its own Card Number heading with a Card Total line; "
              "foreign-currency purchases on two lines (Incl Currency Conversion Charge), one "
              "a refund marked CR; an amount over 1000 printed without a comma; two pages."),
    dict(name="anz_visa_3", start=(2024, 12, 15), sections=[16], early=2, sundry=["pay", "int"],
         note="Period crosses the new year (15 Dec 24 - 14 Jan 25); the first transactions are "
              "dated before the period starts, in December."),
    dict(name="anz_visa_4", start=(2025, 1, 3), sections=[13], early=1, early_days=(4, 4),
         fx=[1], sundry=["pay", "int"],
         note="January statement whose first transaction is dated 30 Dec (processed 3 Jan): "
              "the QVF's 'Dec on a Jan-Feb statement is last year' rule (5007)."),
    dict(name="anz_visa_5", start=(2025, 7, 21), bundle=3, sections=[10], sundry=["pay", "int"],
         note="Three consecutive statements of one card in one file, each with its own "
              "Statement Period, panel and table; balances chain from one into the next."),
    dict(name="anz_visa_6", start=(2025, 9, 2), sections=[11], in_credit=True, end_on_row=True,
         sundry=["int", "pay"],
         note="Card paid into credit: closing balance printed CR; the file ends on the last "
              "transaction (a CR payment) with no closing line after it (the QVF's "
              "DummyLastString case, 4163-4173)."),
    dict(name="anz_visa_7", start=(2025, 5, 11), sections=[0], no_txn=True, in_credit=True,
         note="No transactions this period: a dormant card in credit, opening = closing CR."),
    dict(name="anz_visa_8", start=(2025, 4, 19), sections=[15], redact=2, sundry=["pay", "int"],
         note="The first two transactions have their details and amount redacted (removed from "
              "the text layer, black box drawn); their dates are still visible (the QVF skips "
              "such leading rows, 4886-4910)."),
    dict(name="anz_visa_10", start=(2025, 8, 26), sections=[13, 9], fx=[1, 1], sundry=["pay", "int"],
         note="Two cardholders with no Card Total line: the second Card Number heading follows "
              "the first cardholder's last amount directly (the QVF's '****' rule, 4353)."),
    dict(name="anz_visa_9", start=(2013, 1, 12), bundle=2, sections=[9], cashback=True,
         sundry=["pay"],
         note="LOW REALISM (before March 2014): two statements, each with a Monthly CashBack "
              "reward credited outside the transaction table (only in an 'Enjoy Your CashBack "
              "Reward Of $x CR' box); the QVF adds it as a row for every statement but the last "
              "(4831-4850)."),
]


# ---------------------------------------------------------------------------
# Building the data.
# ---------------------------------------------------------------------------

def card_no(rng):
    return "4%03d %02d** **** %04d" % (rng.randint(0, 999), rng.randint(10, 99), rng.randint(0, 9999))


def make_rows(rng, spec, start, end, opening):
    """Rows per section (cardholders) and sundry, in printed order."""
    days = (end - start).days + 1
    secs = []
    fxn = list(spec.get("fx", [])) + [0] * 4
    for s, n in enumerate(spec["sections"]):
        rows = []
        for j in range(n):
            if j < fxn[s]:
                desc, fxt, nz, ch = C.fx_purchase(rng)
                rows.append(dict(desc1=desc + " " + fxt, desc2="Incl Currency Conversion Charge %s"
                                 % ML.mag(ch, thousands=False, dollar=True), amt=-nz, fx=True))
            else:
                big = s == 0 and (j - fxn[s]) < spec.get("big", 0)
                desc, c = C.purchase(rng, big=big)
                rows.append(dict(desc1=desc, desc2=None, amt=-c, fx=False))
        if s == 0 and n >= 6:
            if spec.get("fx_refund"):
                desc, fxt, nz, ch = C.fx_purchase(rng, "AUD")
                rows.append(dict(desc1="REFUND " + desc + " " + fxt,
                                 desc2="Incl Currency Conversion Charge %s" % ML.mag(ch, thousands=False, dollar=True),
                                 amt=nz, fx=True))
            desc, c = C.purchase(rng)
            rows.append(dict(desc1="REFUND " + desc, desc2=None, amt=min(c, 9000), fx=False))
        rng.shuffle(rows)
        # processed dates within the period, in order; transaction date 0-3 days before
        pds = sorted(start + dt.timedelta(days=rng.randrange(days)) for _ in rows)
        for k, (r, pd) in enumerate(zip(rows, pds)):
            r["pdate"] = pd
            r["date"] = max(start, pd - dt.timedelta(days=rng.choice([0, 0, 1, 1, 2, 3])))
        if s == 0 and spec.get("early"):
            lo, hi = spec.get("early_days", (1, 2))
            for k in range(spec["early"]):
                rows[k]["pdate"] = start + dt.timedelta(days=k)
                rows[k]["date"] = start - dt.timedelta(days=rng.randint(lo, hi) - k)
        for r in rows:
            r["section"] = s
        secs.append(rows)
    sundry = []
    owed = -opening if opening < 0 else 0
    purchases = -sum(r["amt"] for rows in secs for r in rows)
    for k in spec.get("sundry", []):
        pd = start + dt.timedelta(days=rng.randrange(3, days))
        if k == "pay":
            if spec.get("in_credit"):
                amt = owed + purchases + rng.randint(2000, 9000)
            else:
                amt = max(1000, int(owed * rng.uniform(0.35, 1.0)))
            sundry.append(dict(desc1="PAYMENT RECEIVED - THANK YOU", desc2=None, amt=amt, fx=False))
        elif k == "int":
            d, c = C.interest_line(rng)
            sundry.append(dict(desc1=d, desc2=None, amt=-c, fx=False))
            pd = end
        elif k == "fee":
            sundry.append(dict(desc1="ANNUAL ACCOUNT FEE", desc2=None, amt=-3000, fx=False))
        sundry[-1]["pdate"] = pd
        sundry[-1]["date"] = pd
        sundry[-1]["section"] = "sundry"
    sundry.sort(key=lambda r: r["pdate"])
    if spec.get("end_on_row") and sundry:
        # the file ends on a CR row: put the payment last
        pays = [r for r in sundry if r["amt"] > 0]
        rest = [r for r in sundry if r["amt"] <= 0]
        sundry = rest + pays
        sundry[-1]["pdate"] = sundry[-1]["date"] = end
    for rows in secs + [sundry]:
        for r in rows:
            r["desc"] = r["desc1"] + (" " + r["desc2"] if r["desc2"] else "")
    return secs, sundry


def build_one(rng, spec, start, ident, opening=None):
    end = C.add_months(start, 1) - dt.timedelta(days=1)
    if opening is None:
        if spec.get("in_credit") and spec.get("no_txn"):
            opening = rng.randint(1500, 9000)
        else:
            opening = -rng.randint(30000, 420000)
    secs, sundry = make_rows(rng, spec, start, end, opening)
    total = sum(r["amt"] for rows in secs + [sundry] for r in rows)
    cashback = rng.randint(180, 900) if spec.get("cashback") else 0
    closing = opening + total + cashback
    if spec.get("in_credit") and not spec.get("no_txn") and closing <= 0:
        raise GenError("%s: meant to close in credit" % spec["name"])
    limit = 800000 if max(-opening, -closing, 0) < 600000 else 1200000
    owed = max(0, -closing)
    minpay = 0 if owed == 0 else max(2500, int(round(owed * 0.03)))
    st = dict(start=start, end=end, opening=opening, closing=closing, secs=secs, sundry=sundry,
              cashback=cashback, limit=limit, minpay=minpay, due=end + dt.timedelta(days=25),
              ident=ident, spec=spec)
    if spec.get("redact"):
        for r in secs[0][:spec["redact"]]:
            r["redact"] = True
    return st


# ---------------------------------------------------------------------------
# Drawing.
# ---------------------------------------------------------------------------

def tokw(sh):
    return sh.width("CR", SIZE)


def amount_cell(sh, y, c, bold=False, dollar=False, mode=None):
    """A figure in the amount column: number right-aligned, CR in its own slot."""
    num, tok = C.card_money(c, thousands=False)
    xr = AMT_R
    xn = xr - tokw(sh) - 3
    a, b = sh.place(xn, num, SIZE, bold, "right")
    if mode == "remove":
        sh.blackout(a, xr, y, SIZE)
        return None
    sh.text(xn, y, num, SIZE, bold=bold, align="right")
    if tok:
        sh.text(xr, y, tok, SIZE, bold=bold, align="right")
    if dollar:
        sh.text(X_DOLLAR, y, "$", SIZE, bold=bold)
    return (num, tok)


def plan_items(st, first_page):
    """The table body as a list of (kind, payload, height)."""
    it = []
    it.append(("open", st["opening"], PITCH + 1))
    if st["spec"].get("no_txn"):
        it.append(("text", "There are no transactions this statement period", PITCH + 1))
    for s, rows in enumerate(st["secs"]):
        if not rows:
            continue
        it.append(("sec", s, PITCH + 2))
        for r in rows:
            it.append(("row", r, PITCH + (FXP if r["desc2"] else 0)))
        if st["spec"].get("card_totals"):
            it.append(("ctot", sum(r["amt"] for r in rows), PITCH + 2))
    if st["sundry"]:
        it.append(("sundry", None, PITCH + 2))
        for r in st["sundry"]:
            it.append(("row", r, PITCH + (FXP if r["desc2"] else 0)))
    if not st["spec"].get("end_on_row"):
        it.append(("close", st["closing"], PITCH + 2))
        it.append(("notice", None, 30))
    return it


def paginate(items, top1, top2):
    pages = [[]]
    y = top1
    for kind, payload, h in items:
        if y + h > BOTTOM and pages[-1]:
            pages.append([])
            y = top2
        pages[-1].append((kind, payload, y))
        y += h
    return pages


def draw_heading(sh, y, pno, npg):
    x0, x1 = 38.0, AMT_R + 3
    sh.rect(x0, y - SIZE - 3, x1 - x0, SIZE + 7, fill=ML.GREY)
    sh.text(X_DATE, y, "Date", SIZE, bold=True)
    sh.text(X_PDATE, y, "Processed", SIZE, bold=True)
    sh.text(X_DESC, y, "Transaction details", SIZE, bold=True)
    sh.text(AMT_R, y, "Amount in NZ$", SIZE, bold=True, align="right")
    sh.text(RIGHT, y, "Page %d of %d" % (pno, npg), 7.5, align="right")
    sh.line(x0, y + 4, x1, y + 4, width=0.6)


def draw_page1(sh, st, cust, card, extra_top):
    sh.text(W2(), 18, BANNER, 6.5, bold=True, align="center", color=ML.MID)
    sh.text(40, 52, "ANZ", 24, bold=True)
    sh.text(RIGHT, 50, "Credit Card Statement", 13, bold=True, align="right")
    sh.text(RIGHT, 78, "ANZ Visa Platinum", 9, align="right")
    name, addr = cust
    sh.text(40, 100, name, 9, bold=True)
    for k, a in enumerate(addr):
        sh.text(40, 112 + 12 * k, a, 8.5)
    rows = [("Statement Period", "%s - %s" % (ddmonyy(st["start"]), ddmonyy(st["end"])), False),
            ("Credit Limit", ML.mag(st["limit"], dollar=True), False),
            ("Account Number", card, False),
            ("Account Name", name, False),
            ("Closing Balance", C.joined(*C.card_money(st["closing"], dollar=True)), True),
            ("Minimum Payment", ML.mag(st["minpay"], dollar=True), False),
            ("Payment Due", ddmonyy(st["due"]), False)]
    for k, (lab, val, b) in enumerate(rows):
        y = 150 + 13 * k
        sh.text(300, y, lab, 8.5, bold=b)
        sh.text(400, y, val, 8.5, bold=b)
    # account summary box
    sh.rect(36, 240, 248, 74, stroke=ML.BLACK, width=0.5)
    sh.text(42, 253, "Account Summary", 9, bold=True)
    tot_d = -sum(r["amt"] for rows in st["secs"] + [st["sundry"]] for r in rows if r["amt"] < 0)
    tot_c = sum(r["amt"] for rows in st["secs"] + [st["sundry"]] for r in rows if r["amt"] > 0)
    tot_c += st["cashback"]
    srows = [("Opening Balance", C.joined(*C.card_money(st["opening"], dollar=True))),
             ("Purchases & Debits", ML.mag(tot_d, dollar=True)),
             ("Payments & Credits", ML.mag(tot_c, dollar=True)),
             ("Closing Balance", C.joined(*C.card_money(st["closing"], dollar=True)))]
    for k, (lab, val) in enumerate(srows):
        y = 267 + 13 * k
        sh.text(42, y, lab, 8.5, bold=(k == 3))
        sh.text(278, y, val, 8.5, bold=(k == 3), align="right")
    sh.text(300, 267, "Purchase rate 20.95% p.a.", 8)
    sh.text(300, 280, "Cash advance rate 22.95% p.a.", 8)
    if st["cashback"]:
        sh.rect(36, 324, 450, 32, stroke=ML.BLACK, width=0.5)
        sh.text(42, 337, "Enjoy Your CashBack Reward Of %s CR" % ML.mag(st["cashback"], dollar=True),
                9, bold=True)
        sh.text(42, 350, "This Monthly CashBack has been credited to your card account.", 8)


def W2():
    return C.W / 2.0


def draw_cont(sh, card):
    sh.text(40, 40, "Credit Card Account Number %s" % card, 9, bold=True)
    sh.text(RIGHT, 40, "ANZ", 14, bold=True, align="right")
    sh.text(W2(), 58, BANNER, 6.5, bold=True, align="center", color=ML.MID)


def render(sh, st, rec):
    """Draw one statement (its pages); fill rec with what each row printed."""
    ident = st["ident"]
    cards = ident["cards"]
    top1 = 372.0 + (38 if st["cashback"] else 0)
    head1 = top1 - 17
    items = plan_items(st, True)
    pages = paginate(items, top1, 104.0)
    npg = len(pages)
    for pno, its in enumerate(pages, 1):
        sh.begin_page()
        if pno == 1:
            draw_page1(sh, st, ident["cust"], cards[0], 0)
            draw_heading(sh, head1, pno, npg)
        else:
            draw_cont(sh, cards[0])
            draw_heading(sh, 87.0, pno, npg)
        for kind, payload, y in its:
            if kind == "open":
                sh.text(X_DESC, y, "Opening Balance", SIZE, bold=True)
                amount_cell(sh, y, payload, bold=True, dollar=True)
            elif kind == "text":
                sh.text(X_DESC, y, payload, SIZE)
            elif kind == "sec":
                nm = ident["cust"][0] if payload == 0 else ident["second"]
                sh.text(X_DESC, y, "Card Number %s %s" % (cards[payload], nm), SIZE, bold=True)
            elif kind == "ctot":
                sh.text(X_DESC, y, "Card Total", SIZE, bold=True)
                amount_cell(sh, y, payload, bold=True, dollar=True)
            elif kind == "sundry":
                sh.text(X_DESC, y, "Sundry Transactions", SIZE, bold=True)
            elif kind == "close":
                sh.text(X_DESC, y, "Closing Balance", SIZE, bold=True)
                amount_cell(sh, y, payload, bold=True, dollar=True)
            elif kind == "notice":
                sh.text(40, y + 6, "We have updated our credit card Conditions of Use - read them at "
                        "anz.co.nz/cards.", 7.5)
            elif kind == "row":
                r = payload
                sh.text(X_DATE, y, ddmon(r["date"]), SIZE)
                sh.text(X_PDATE, y, ddmon(r["pdate"]), SIZE)
                if r.get("redact"):
                    a, b = sh.place(X_DESC, r["desc1"], SIZE)
                    sh.blackout(a, b, y, SIZE)
                    amount_cell(sh, y, r["amt"], mode="remove")
                    rec.append(dict(r=r, date=ddmon(r["date"]), money=None, text=None))
                    continue
                sh.text(X_DESC, y, r["desc1"], SIZE)
                if r["desc2"]:
                    sh.text(X_DESC + 8, y + FXP, r["desc2"], SIZE)
                m = amount_cell(sh, y, r["amt"])
                rec.append(dict(r=r, date=ddmon(r["date"]), money=m,
                                text=r["desc1"] + (" " + r["desc2"] if r["desc2"] else "")))
        sh.end_page()
    return npg


# ---------------------------------------------------------------------------
# A simplified QVF (the row loop of 4797-5044) run on the PDF's reading order,
# as a check that the lookalike has the shape the script reads.
# ---------------------------------------------------------------------------

def qvf_lite(pdf):
    lines = C.page_lines(pdf)
    words = [w for _, _, ws in lines for (_, _, w) in ws]
    # the statements: every 'Period' word (4207-4216), dates at +1..+7
    periods = []
    for i, w in enumerate(words):
        if w == "Period":
            periods.append((words[i + 2], "20" + words[i + 3], words[i + 6], "20" + words[i + 7]))
    # the transaction stream the pairs are meant to yield: each statement's table
    # body lines (dated lines, the FX second lines, the Opening line), with a
    # 'New Statement' marker per statement.
    stream = []
    k = 0
    for pno, y, ws in lines:
        toks = [w for _, _, w in ws]
        if toks[:2] == ["Statement", "Period"]:
            stream.append("New Statement")
            k += 1
        dated = len(toks) >= 2 and toks[0].isdigit() and toks[1] in C.MON_SET \
            and ws[0][0] < X_PDATE - 4
        if dated or toks[:4] == ["Incl", "Currency", "Conversion", "Charge"] \
                or toks[:2] == ["Opening", "Balance"] and ws[0][0] > X_DESC - 3:
            stream += toks
    # 4139-4161: drop the processed date (a month two words after a month) and its day
    drop = set()
    for i in range(2, len(stream)):
        if stream[i] in C.MON_SET and stream[i - 2] in C.MON_SET:
            drop.update((i, i - 1))
    stream = [w for i, w in enumerate(stream) if i not in drop]
    stream = [w for w in stream if w != "$"]
    # 4745-4795: move the NZ$ amount (and CR) after "Incl Currency Conversion Charge x"
    i = 0
    while i < len(stream):
        if stream[i:i + 4] == ["Incl", "Currency", "Conversion", "Charge"]:
            j = i - 2 if stream[i - 1] == "CR" else i - 1
            moved = stream[j:i]
            stream = stream[:j] + stream[i:i + 5] + moved + stream[i + 5:]
            i = j + 5 + len(moved)
        else:
            i += 1
    return anz_loop(stream, periods)


def num_ok(s):
    return s is not None and s.replace(".", "").isdigit() and len(s.split(".")[-1]) == 2 and "." in s


def anz_year(dm, per):
    psm, psy, pem, pey = per
    mon = dm.split()[-1]
    if psm in ("Oct", "Nov", "Dec") and mon in ("Oct", "Nov", "Dec"):
        return int(psy)
    if mon == "Dec" and psm != "Dec" and pem != "Dec" and psy == pey:
        return int(psy) - 1
    return int(pey)


def anz_loop(s, periods):
    out = []
    last = len(s) - 1
    get = lambda p: s[p] if 0 <= p <= last else None  # noqa: E731
    stmt = 1
    pos = 0
    # sCreateVariables / sLoopToDate: step forward to the first month, back one
    while True:
        pos += 1
        if pos > last:
            return out
        if get(pos) == "New Statement":
            stmt += 1
        if get(pos) in C.MON_SET:
            break
    pos -= 1
    stmt = 1
    rows_seen = 0
    while pos <= last:
        row = {}
        col = 1
        row_end = False
        while not row_end:
            cur = get(pos)
            if cur == "New Statement":
                stmt += 1
                while True:
                    pos += 1
                    if pos > last:
                        return out
                    if get(pos) in C.MON_SET:
                        break
                pos -= 1
                cur = get(pos)
            string = []
            str_end = False
            while not str_end:
                if col == 1 and cur == "Opening":
                    while True:
                        pos += 1
                        if pos > last:
                            return out
                        if get(pos) in C.MON_SET:
                            break
                    pos -= 1
                    cur = get(pos)
                if col == 1 and stmt < 3 and rows_seen == 0:
                    # redacted leading rows: a date followed straight by another date
                    while get(pos + 2) and get(pos + 2).isdigit() and get(pos + 3) in C.MON_SET:
                        pos += 2
                    cur = get(pos)
                p1, p2, p3 = get(pos + 1), get(pos + 2), get(pos + 3)
                if col == 3 and (p2 in C.MON_SET or p1 == "New Statement"):
                    row_end = str_end = True
                if col == 1 and cur in C.MON_SET:
                    str_end = True
                if col == 2 and ((num_ok(p1) and (p2 in ("New Statement", "CR")
                                                  or p3 in ("New Statement", "CR") or p3 in C.MON_SET))
                                 or pos + 1 == last):
                    str_end = True
                if pos == last:
                    str_end = row_end = True
                string.append(cur)
                pos += 1
                cur = get(pos)
            txt = " ".join(w for w in string if w)
            if col == 3:
                if "CR" in txt:
                    txt = txt[:-3]
                    val = txt.replace(" ", "")
                else:
                    val = "-" + txt.replace(" ", "")
                try:
                    row["amount"] = int(round(float(val) * 100))
                except ValueError:
                    row["amount"] = None
            elif col == 1:
                try:
                    y = anz_year(txt, periods[stmt - 1])
                    row["date"] = dt.datetime.strptime("%s %d" % (txt, y), "%d %b %Y").date()
                except (ValueError, IndexError):
                    row["date"] = None
            else:
                row["details"] = txt
            col += 1
            if col > 3:
                row_end = True
        if row.get("details"):
            out.append(row)
            rows_seen += 1
    return out


# ---------------------------------------------------------------------------
# Checks and the truth.
# ---------------------------------------------------------------------------

def verify_printed(name, rec, st_rows):
    for x in rec:
        r = x["r"]
        if x["money"] is not None and C.parse_card(*x["money"]) != r["amt"]:
            raise GenError("%s: amount cell %r" % (name, x["money"]))
        if x["text"] is not None and x["text"] != r["desc"]:
            raise GenError("%s: description %r" % (name, x["text"]))
        if "," in (x["money"] or ("", ""))[0]:
            raise GenError("%s: a row amount with a thousands comma" % name)
    if len(rec) != len(st_rows):
        raise GenError("%s: %d rows drawn, %d planned" % (name, len(rec), len(st_rows)))


def verify_text_layer(name, pdf, truth_rows):
    """Every unredacted truth row's date and figure are on one line of the text layer."""
    lines = C.page_lines(pdf)
    seq = []
    for pno, y, ws in lines:
        toks = [w for _, _, w in ws]
        if len(toks) >= 4 and toks[0].isdigit() and toks[1] in C.MON_SET and toks[3] in C.MON_SET:
            seq.append(toks)
    if len(seq) != len(truth_rows):
        raise GenError("%s: %d dated lines in the text layer, %d truth rows" % (name, len(seq), len(truth_rows)))
    for toks, t in zip(seq, truth_rows):
        d = dt.date.fromisoformat(t["date"])
        if " ".join(toks[:2]) != ddmon(d):
            raise GenError("%s: date %r vs %s" % (name, toks[:2], t["date"]))
        if t["debit"] is None and t["credit"] is None:
            continue
        v = t["credit"] if t["credit"] is not None else t["debit"]
        num = ML.mag(int(round(v * 100)), thousands=False)
        if num not in toks:
            raise GenError("%s: figure %s not on its line %r" % (name, num, toks))
        if (t["credit"] is not None) != (toks[-1] == "CR"):
            raise GenError("%s: CR mark wrong on %r" % (name, toks))


def landmarks(name, pdf, sts):
    """The QVF's landmark sequences, in reading order."""
    words = [w for _, _, _, w in C.reading_words(pdf)]
    at = 0
    for st in sts:
        i = C.find_seq(words, ["Statement", "Period"], at)
        if i < 0:
            raise GenError("%s: no Statement Period" % name)
        per = words[i + 2:i + 9]
        if per[3] != "-" or per[:3] != ddmonyy(st["start"]).split() or per[4:7] != ddmonyy(st["end"]).split():
            raise GenError("%s: period words %r" % (name, per))
        if words[i + 9] != "Credit":
            raise GenError("%s: the period is not followed by Credit (%r)" % (name, words[i + 9]))
        j = C.find_seq(words, ["Account", "Number"], i)
        k = C.find_seq(words, ["Closing", "Balance"], j)
        between = [w for w in words[j + 2:k] if w not in ("Account", "Name", "Visa", "Platinum")]
        acct = " ".join(w for w in between if set(w) <= set("0123456789*"))
        if acct != st["ident"]["cards"][0]:
            raise GenError("%s: QVF account number would read %r" % (name, acct))
        mp = C.find_seq(words, ["Minimum", "Payment"], k)
        if mp < 0 or mp - k > 4:
            raise GenError("%s: Closing Balance not followed by Minimum Payment" % name)
        at = mp
    for i, w in enumerate(words):
        if w == "NZ$" and not (words[i - 1] == "in" and words[i + 1] == "Page"):
            raise GenError("%s: heading not followed by Page" % name)
        if w == "Account" and words[i - 2:i] == ["Credit", "Card"] and words[i - 3] == "ANZ":
            raise GenError("%s: 'ANZ Credit Card Account'" % name)


def features(st0, sts, npg):
    spec = st0["spec"]
    f = ["bank:anz", "cols:date|pdate|desc|amount", "date:dd Mon", "date_yearless", "font:Helvetica",
         "size:8", "page:A4", "pages:%d" % npg, "money:no_thousands", "sign:card_cr",
         "token_column", "no_balance_column", "heading_every_page", "opening_line",
         "summary_box", "sidebar", "period:dd Mon yy"]
    if not spec.get("end_on_row"):
        f.append("closing_line")
    if any(st["start"].year != st["end"].year for st in sts):
        f.append("period_crosses_year")
    if len(spec["sections"]) > 1:
        f.append("card_sections")
    if any(r["desc2"] for st in sts for rows in st["secs"] + [st["sundry"]] for r in rows):
        f += ["multiline_desc", "fx_in_description", "money_like_desc"]
    if spec.get("card_totals"):
        f.append("card_subtotals")
    if any(abs(r["amt"]) >= 100000 for st in sts for rows in st["secs"] for r in rows):
        f.append("amount_over_1000")
    if spec.get("redact"):
        f.append("redacted_removed")
    if spec.get("bundle"):
        f += ["bundle", "bundle:%d" % len(sts)]
    if spec.get("no_txn"):
        f.append("no_transactions")
    if any(st["closing"] > 0 for st in sts):
        f.append("card_in_credit")
    if any(r["date"] < st["start"] for st in sts for rows in st["secs"] for r in rows):
        f.append("txn_date_before_period")
    if spec.get("cashback"):
        f.append("off_table_credit")
    f.append("qvf:" + spec["name"])
    return f


def build(spec, out):
    name = spec["name"]
    rng = random.Random(C.seed_for(name))
    cust = rng.choice(C.PEOPLE)
    ident = dict(cust=cust, cards=[card_no(rng) for _ in range(max(1, len(spec["sections"])))],
                 second=rng.choice(C.SECOND))
    sts = []
    start = dt.date(*spec["start"])
    opening = None
    for j in range(spec.get("bundle", 1)):
        st = build_one(rng, spec, start, ident, opening)
        sts.append(st)
        opening = st["closing"]
        start = C.add_months(start, 1)
    pdf = os.path.join(out, name + ".pdf")
    sh = ML.Sheet(pdf, ML.A4, "Helvetica")
    rows_out, npg = [], 0
    for j, st in enumerate(sts):
        rec = []
        npg += render(sh, st, rec)
        prows = [r for rows in st["secs"] + [st["sundry"]] for r in rows]
        C.check_chain(name, st["opening"], prows, st["closing"], st["cashback"])
        verify_printed(name, rec, prows)
        rows_out += [C.truth_row(r, j if spec.get("bundle") else None) for r in prows]
    sh.save()
    verify_text_layer(name, pdf, rows_out)
    landmarks(name, pdf, sts)
    truth = {
        "case": name,
        "generator": GEN,
        "note": "QVF lookalike. ANZ, Visa credit card (synthetic). " + spec["note"],
        "bank": "ANZ",
        "layout": "qvf_anz_visa",
        "product": "Visa credit card",
        "source_format": "pdf",
        "account_bank_code": None,
        "account_number": None,
        "account_redaction": None,
        "features": features(sts[0], sts, npg),
        "row_order": "oldest_first",
        "opening_balance": sts[0]["opening"] / 100.0,
        "closing_balance": sts[-1]["closing"] / 100.0,
        "removed_rows": 0,
        "row_count": len(rows_out),
        "rows": rows_out,
    }
    if spec.get("bundle"):
        truth["statements"] = [
            {"statement_index": j, "period_start": st["start"].isoformat(),
             "period_end": st["end"].isoformat(), "opening_balance": st["opening"] / 100.0,
             "closing_balance": st["closing"] / 100.0} for j, st in enumerate(sts)]
    C.write_truth(os.path.join(out, name + ".truth.json"), truth)
    # the simplified QVF, for the record
    q = qvf_lite(pdf)
    want = [(t["date"], None if t["debit"] is None and t["credit"] is None
             else int(round(((t["credit"] or 0) - (t["debit"] or 0)) * 100))) for t in rows_out]
    got = [(r["date"].isoformat() if r.get("date") else None, r.get("amount")) for r in q]
    same = sum(1 for a, b in zip(want, got) if a == b)
    return name, truth, npg, (len(want), len(got), same, got, want)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=C.SET_DIR)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    log = []
    for spec in SPECS:
        name, truth, npg, (nw, ng, same, got, want) = build(spec, a.out)
        print("%-12s %2d rows %d pg  qvf-lite: %d rows, %d match the key in place"
              % (name, truth["row_count"], npg, ng, same))
        log.append("%s,%d,%d,%d,%d" % (name, truth["row_count"], npg, ng, same))
        if same != nw or ng != nw:
            for k in range(max(nw, ng)):
                a1 = want[k] if k < nw else None
                b1 = got[k] if k < ng else None
                if a1 != b1:
                    print("     key %s  qvf-lite %s" % (a1, b1))
    with open(os.path.join(a.out, "qvf_lite_anz_visa.csv"), "w") as f:
        f.write("case,rows,pages,qvf_lite_rows,qvf_lite_same\n" + "\n".join(log) + "\n")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except GenError as e:
        sys.exit("GENERATOR SELF-CHECK FAILED: %s" % e)
