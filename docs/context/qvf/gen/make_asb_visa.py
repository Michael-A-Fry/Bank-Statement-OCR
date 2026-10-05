#!/usr/bin/env python3
"""make_asb_visa.py -- ASB Visa (QVF type "ASB - Visa", type key asb_visa)
lookalike statements, drawn to the shape the QVF load script expects
(script.qvs lines 5049-5471), each with an answer key in exactly the format
tools/synth/make_layouts.py writes.

    python3 make_asb_visa.py [--out DIR]      (default: ../sets/visa1)

What the QVF needs, and so what every statement here carries (cards/asb_visa.md):
  * "Account Summary" followed by the period as two dates with 2-digit years and
    nothing between them, "dd Mon yy dd Mon yy" (5077-5098, 5201-5223), or
    "- dd Mon yy" when there is no start date (a card's first statement); the
    pair runs to the next "Payment" word (Minimum Payment);
  * a "Payment Advice" slip: the holder's name up to "Customer", and the card
    number as the last 19 characters before "Instructions" (5111-5133, 5225-5233);
  * a table whose heading ends with "Date Processed" (the start marker, 5135-5145),
    every row in the word order date | details | amount [CR] | processed date |
    balance [CR] | card used (5277-5359), running balance printed on every row;
  * the table ends on each page at "Carried forward" and finally at "Closing
    Balance" (5144); the words Balance and $ are dropped from the stream (5175);
  * an "Opening Balance" line before the first row (passed over by sLoopToDate).
"""

import argparse
import datetime as dt
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import visa1_common as C  # noqa: E402
from visa1_common import ML, GenError, ddmon, ddmonyy  # noqa: E402

GEN = "qvf/gen/make_asb_visa.py"
BANNER = ML.SYNTHETIC
SIZE = 8.0
PITCH = 12.5
X_DATE, X_DESC = 42.0, 78.0
AMT_R = 370.0
X_PDATE = 378.0
BAL_R = 506.0
X_CARD = 516.0
RIGHT = 555.0
BOTTOM1 = 668.0
BOTTOM = 800.0


SPECS = [
    dict(name="asb_visa_1", start=(2025, 3, 5), n=14, pays=1, interest=True,
         note="Plain month on one page: running balance on every row, a payment marked CR, "
              "interest charged."),
    dict(name="asb_visa_2", start=(2025, 6, 18), n=36, pays=1, refunds=1, fx=4, cards=2,
         interest=True,
         note="Two pages with a Carried forward line; two cardholders told apart only by the "
              "Card column; foreign-currency purchases with the currency amount at the end of "
              "the details; a refund marked CR."),
    dict(name="asb_visa_3", start=(2024, 12, 16), n=18, pays=1, interest=True, early=2,
         note="Period crosses the new year (16 Dec 24 - 15 Jan 25); the first transactions are "
              "dated before the period starts."),
    dict(name="asb_visa_4", start=(2024, 12, 10), n=12, pays=0, open_zero=True, no_start=True,
         note="A new card's first statement: the period prints no start date ('Account Summary "
              "- 9 Jan 25', the QVF's 'Unknown' start, 5203-5212), opening balance 0.00, "
              "December and January transactions."),
    dict(name="asb_visa_5", start=(2025, 8, 7), n=15, pays=2, refunds=1, into_credit=True,
         note="Overpaid: the running balance crosses from owed into credit (balances marked CR) "
              "and the statement closes in credit."),
    dict(name="asb_visa_6", start=(2025, 4, 22), n=11, pays=1, interest=True, bundle=2,
         note="Two consecutive statements of one card in one file, each with its own Account "
              "Summary, payment slip and table; balances chain from one into the next."),
    dict(name="asb_visa_7", start=(2025, 1, 3), n=13, pays=1, interest=True, early=1,
         early_days=(4, 4),
         note="January statement whose first transaction is dated 30 Dec (processed 3 Jan). The "
              "QVF dates it 30 Dec of the END year (its 'Dec is last year' rule needs an unknown "
              "start date, 5391), a year late."),
    dict(name="asb_visa_8", start=(2025, 10, 9), n=0, pays=0, no_txn=True,
         note="No transactions this period: a dormant card in credit, opening = closing CR."),
]


def card_no(rng):
    return "4%03d %02dXX XXXX %04d" % (rng.randint(0, 999), rng.randint(10, 99), rng.randint(0, 9999))


def build_one(rng, spec, start, ident, opening=None):
    end = C.add_months(start, 1) - dt.timedelta(days=1)
    days = (end - start).days + 1
    n = spec["n"]
    if opening is None:
        if spec.get("open_zero"):
            opening = 0
        elif spec.get("no_txn"):
            opening = rng.randint(800, 6000)
        else:
            opening = -rng.randint(40000, 380000)
    rows = []
    nfx = spec.get("fx", 0)
    for j in range(n):
        if j < nfx:
            desc, fxt, nz, ch = C.fx_purchase(rng)
            rows.append(dict(desc=desc + " " + fxt, amt=-nz))
        else:
            desc, c = C.purchase(rng)
            rows.append(dict(desc=desc, amt=-c))
    for _ in range(spec.get("refunds", 0)):
        desc, c = C.purchase(rng)
        rows.append(dict(desc="REFUND " + desc, amt=min(c, 9000)))
    rng.shuffle(rows)
    pds = sorted(start + dt.timedelta(days=rng.randrange(days)) for _ in rows)
    for r, pd in zip(rows, pds):
        r["pdate"] = pd
        r["date"] = max(start, pd - dt.timedelta(days=rng.choice([0, 0, 1, 1, 2, 3])))
    if spec.get("early"):
        lo, hi = spec.get("early_days", (1, 2))
        for k in range(spec["early"]):
            rows[k]["pdate"] = start + dt.timedelta(days=k)
            rows[k]["date"] = start - dt.timedelta(days=rng.randint(lo, hi) - k)
    cards = ident["card4s"][:spec.get("cards", 1)]
    for r in rows:
        r["card4"] = rng.choice(cards) if len(cards) > 1 and rng.random() < 0.4 else cards[0]
    # payments: placed after a few purchases
    owed = -opening
    purch = -sum(r["amt"] for r in rows if r["amt"] < 0)
    for k in range(spec.get("pays", 0)):
        if spec.get("into_credit"):
            amt = (owed + purch + rng.randint(3000, 12000)) if k == 0 else rng.randint(2000, 6000)
        else:
            amt = max(1500, int(owed * rng.uniform(0.4, 1.0))) if k == 0 else rng.randint(2000, 9000)
        pd = start + dt.timedelta(days=rng.randrange(4, max(5, days - 6)))
        pay = dict(desc="PAYMENT RECEIVED THANK YOU", amt=amt, pdate=pd, date=pd, card4=cards[0])
        idx = sum(1 for r in rows if r["pdate"] <= pd)
        rows.insert(idx, pay)
    if spec.get("interest"):
        d, c = C.interest_line(rng)
        rows.append(dict(desc=d, amt=-c, pdate=end, date=end, card4=cards[0]))
    bal = opening
    for r in rows:
        bal += r["amt"]
        r["bal"] = bal
        r["bal_printed"] = True
    closing = bal
    if spec.get("into_credit") and not (closing > 0 and any(r["bal"] < 0 for r in rows)):
        raise GenError("%s: meant to cross into credit" % spec["name"])
    limit = 500000 if max(-opening, -closing, 0) < 380000 else 1000000
    owed_c = max(0, -closing)
    minpay = 0 if owed_c == 0 else max(2000, int(round(owed_c * 0.03)))
    return dict(start=start, end=end, opening=opening, closing=closing, rows=rows, limit=limit,
                minpay=minpay, due=end + dt.timedelta(days=24), ident=ident, spec=spec)


# ---------------------------------------------------------------------------
# Drawing.
# ---------------------------------------------------------------------------

def tokw(sh):
    return sh.width("CR", SIZE)


def fig(sh, y, right, c, bold=False):
    num, tok = C.card_money(c, thousands=True)
    xn = right - tokw(sh) - 3
    sh.text(xn, y, num, SIZE, bold=bold, align="right")
    if tok:
        sh.text(right, y, tok, SIZE, bold=bold, align="right")
    return (num, tok)


def draw_heading(sh, y):
    x0, x1 = 38.0, RIGHT
    sh.rect(x0, y - 2 * SIZE - 5, x1 - x0, 2 * SIZE + 9, fill=ML.GREY)
    # "Card" sits on the line above: in reading order the heading then ENDS with
    # "Date Processed" (and Balance, which the QVF drops), as the script needs.
    sh.text(X_CARD, y - 10, "Card", SIZE, bold=True)
    sh.text(X_DATE, y, "Date", SIZE, bold=True)
    sh.text(X_DESC, y, "Transaction Details", SIZE, bold=True)
    sh.text(AMT_R, y, "Amount", SIZE, bold=True, align="right")
    sh.text(X_PDATE, y, "Date Processed", SIZE, bold=True)
    sh.text(BAL_R, y, "Balance", SIZE, bold=True, align="right")
    sh.line(x0, y + 4, x1, y + 4, width=0.6)


def W2():
    return C.W / 2.0


def draw_page1(sh, st):
    ident = st["ident"]
    name, addr = ident["cust"]
    sh.text(W2(), 18, BANNER, 6.5, bold=True, align="center", color=ML.MID)
    sh.text(40, 52, "ASB", 24, bold=True)
    sh.text(RIGHT, 50, "Visa Credit Card Statement", 13, bold=True, align="right")
    sh.text(RIGHT, 66, "ASB Visa Classic", 9, align="right")
    sh.text(40, 100, name, 9, bold=True)
    for k, a in enumerate(addr):
        sh.text(40, 112 + 12 * k, a, 8.5)
    sh.text(330, 100, "Card Number", 8.5)
    sh.text(420, 100, ident["card"], 8.5)
    sh.text(330, 113, "Statement Date", 8.5)
    sh.text(420, 113, ddmonyy(st["end"] + dt.timedelta(days=1)), 8.5)
    sh.text(330, 162, "Opening date", 7.5, color=ML.MID)
    sh.text(440, 162, "Closing date", 7.5, color=ML.MID)
    sh.text(40, 175, "Account Summary", 10, bold=True)
    sh.text(330, 175, "-" if st["spec"].get("no_start") else ddmonyy(st["start"]), 9)
    sh.text(440, 175, ddmonyy(st["end"]), 9)
    sh.line(38, 180, RIGHT, 180, width=0.5)
    tot_d = -sum(r["amt"] for r in st["rows"] if r["amt"] < 0)
    tot_c = sum(r["amt"] for r in st["rows"] if r["amt"] > 0)
    left = [("Opening Balance", C.joined(*C.card_money(st["opening"], dollar=True))),
            ("Purchases & Debits", ML.mag(tot_d, dollar=True)),
            ("Payments & Credits", ML.mag(tot_c, dollar=True)),
            ("Closing Balance", C.joined(*C.card_money(st["closing"], dollar=True)))]
    right = [("Minimum Payment", ML.mag(st["minpay"], dollar=True)),
             ("Payment Due Date", ddmonyy(st["due"])),
             ("Credit Limit", ML.mag(st["limit"], dollar=True)),
             ("Available Credit", ML.mag(max(0, st["limit"] + min(0, st["closing"])), dollar=True))]
    for k, ((l1, v1), (l2, v2)) in enumerate(zip(left, right)):
        y = 194 + 13 * k
        sh.text(42, y, l1, 8.5, bold=(k == 3))
        sh.text(285, y, v1, 8.5, bold=(k == 3), align="right")
        sh.text(330, y, l2, 8.5)
        sh.text(RIGHT, y, v2, 8.5, align="right")


def draw_slip(sh, st):
    ident = st["ident"]
    sh.line(38, 680, RIGHT, 680, width=0.4, color=ML.MID)
    sh.text(40, 698, "Payment Advice", 10, bold=True)
    sh.text(40, 712, ident["cust"][0], 8.5)
    sh.text(40, 725, "Customer Number %s" % ident["custno"], 8.5)
    sh.text(40, 738, "Card Number %s" % ident["card"], 8.5)
    sh.text(40, 754, "Instructions", 8.5, bold=True)
    sh.text(40, 767, "Detach this slip and pay at any ASB branch, or pay online quoting your "
            "card number.", 8)
    owed = max(0, -st["closing"])
    sh.text(40, 782, "Amount Due %s" % ML.mag(owed, dollar=True), 8.5, bold=True)
    sh.text(300, 782, "Due Date %s" % ddmonyy(st["due"]), 8.5, bold=True)


def draw_cont(sh, st):
    sh.text(W2(), 18, BANNER, 6.5, bold=True, align="center", color=ML.MID)
    sh.text(40, 46, "ASB", 14, bold=True)
    sh.text(RIGHT, 46, "Card Number %s" % st["ident"]["card"], 9, align="right")


def render(sh, st, rec):
    items = [("open", st["opening"])]
    if st["spec"].get("no_txn"):
        items.append(("text", "No transactions this statement period"))
    items += [("row", r) for r in st["rows"]]
    items.append(("close", st["closing"]))
    # paginate: a page that continues ends with Carried forward
    pages = [[]]
    y = 306.0
    last_bal = st["opening"]
    for kind, p in items:
        limit = BOTTOM1 if len(pages) == 1 else BOTTOM
        if y + 2 * PITCH > limit and kind == "row":
            pages[-1].append(("cf", last_bal, y))
            pages.append([])
            y = 102.0
        pages[-1].append((kind, p, y))
        y += PITCH + (1 if kind != "row" else 0)
        if kind == "row":
            last_bal = p["bal"]
    npg = len(pages)
    for pno, its in enumerate(pages, 1):
        sh.begin_page()
        if pno == 1:
            draw_page1(sh, st)
            draw_heading(sh, 290.0)
        else:
            draw_cont(sh, st)
            draw_heading(sh, 86.0)
        for kind, p, y in its:
            if kind == "open":
                sh.text(X_DESC, y, "Opening Balance", SIZE, bold=True)
                fig(sh, y, BAL_R, p, bold=True)
            elif kind == "text":
                sh.text(X_DESC, y, p, SIZE)
            elif kind == "cf":
                sh.text(X_DESC, y, "Carried forward", SIZE, bold=True)
                fig(sh, y, BAL_R, p, bold=True)
            elif kind == "close":
                sh.text(X_DESC, y, "Closing Balance", SIZE, bold=True)
                fig(sh, y, BAL_R, p, bold=True)
            elif kind == "row":
                r = p
                # drawn in the QVF's word order: date, details, amount, processed, balance, card
                sh.text(X_DATE, y, ddmon(r["date"]), SIZE)
                sh.text(X_DESC, y, r["desc"], SIZE)
                m = fig(sh, y, AMT_R, r["amt"])
                sh.text(X_PDATE, y, ddmon(r["pdate"]), SIZE)
                b = fig(sh, y, BAL_R, r["bal"])
                sh.text(X_CARD, y, r["card4"], SIZE)
                rec.append(dict(r=r, money=m, bal=b))
        if pno == 1:
            draw_slip(sh, st)
        sh.text(RIGHT, 822, "Page %d of %d" % (pno, npg), 7.5, align="right")
        sh.end_page()
    return npg


# ---------------------------------------------------------------------------
# A simplified QVF (5199-5467) on the PDF's reading order.
# ---------------------------------------------------------------------------

def qvf_lite(pdf):
    words = [w for _, _, _, w in C.reading_words(pdf)]
    periods = []
    summ = [i for i, w in enumerate(words) if w == "Summary" and words[i - 1] == "Account"]
    for k, i in enumerate(summ):
        if k == 0 and words[i + 1] == "-":
            periods.append(("Unknown", "Unknown", words[i + 3], "20" + words[i + 4]))
        else:
            periods.append((words[i + 2], "20" + words[i + 3], words[i + 5], "20" + words[i + 6]))
    # pairs: after "Date Processed" to the next "Carried forward" / "Closing Balance"
    stream = []
    i = 0
    marks = set(summ)
    while i < len(words):
        if i in marks:
            stream.append("New Statement")
        if words[i] == "Processed" and words[i - 1] == "Date":
            j = i + 1
            while j < len(words) and not ((words[j] == "Carried" and words[j + 1] == "forward")
                                          or (words[j] == "Closing" and words[j + 1] == "Balance")):
                if j in marks:
                    stream.append("New Statement")
                stream.append(words[j])
                j += 1
            i = j
            continue
        i += 1
    stream = [w for w in stream if w not in ("Balance", "$")]
    while stream and stream[-1] == "New Statement":
        stream.pop()
    return asb_loop(stream, periods)


def asb_year(dm, per):
    psm, psy, pem, pey = per
    mon = dm.split()[-1]
    if psm in ("Oct", "Nov", "Dec") and mon in ("Oct", "Nov", "Dec"):
        return int(psy)
    if psm == "Unknown" and mon == "Dec" and pem == "Jan":
        return int(pey) - 1
    return int(pey)


def two_dp(s):
    return s is not None and "." in s and len(s.split(".")[1]) == 2 and any(ch.isdigit() for ch in s)


def asb_loop(s, periods):
    out = []
    last = len(s) - 1
    get = lambda p: s[p] if 0 <= p <= last else None  # noqa: E731
    pos = 0
    stmt = 1

    def loop_to_date(p):
        nonlocal stmt
        while True:
            p += 1
            if p > last:
                return None
            if get(p) == "New Statement":
                stmt += 1
            if get(p) in C.MON_SET:
                return p - 1
    pos = loop_to_date(pos)
    if pos is None:
        return out
    while pos is not None and pos <= last:
        row = {}
        col = 1
        row_end = False
        while not row_end:
            cur = get(pos)
            if cur == "New Statement":
                pos = loop_to_date(pos)
                if pos is None:
                    return out
                stmt += 1
                cur = get(pos)
            string = []
            str_end = False
            while not str_end:
                p1 = p2 = p3 = None
                if col >= 2:
                    p1, p2, p3 = get(pos + 1), get(pos + 2), get(pos + 3)
                if col == 4 and (p3 in C.MON_SET or p1 == "New Statement"):
                    row_end = str_end = True
                    if p1 != "New Statement":
                        pos += 1
                if col == 1 and cur in C.MON_SET:
                    str_end = True
                if col == 2 and two_dp(p1) and (p2 == "CR" or p3 in C.MON_SET):
                    str_end = True
                if col == 3 and p2 in C.MON_SET:
                    str_end = True
                    pos += 2
                if pos >= last:
                    str_end = row_end = True
                string.append(cur)
                pos += 1
                cur = get(pos)
            txt = " ".join(w for w in string if w)
            if col > 2:
                if "CR" in txt:
                    val = txt[:-3].replace(" ", "").replace(",", "")
                else:
                    val = "-" + txt.replace(" ", "").replace(",", "")
                try:
                    v = int(round(float(val) * 100))
                except ValueError:
                    v = None
                row["amount" if col == 3 else "balance"] = v
            elif col == 1:
                try:
                    y = asb_year(txt, periods[stmt - 1])
                    row["date"] = dt.datetime.strptime("%s %d" % (txt, y), "%d %b %Y").date()
                except (ValueError, IndexError):
                    row["date"] = None
            else:
                row["details"] = txt
            col += 1
            if col > 4:
                row_end = True
        if row.get("details"):
            out.append(row)
    return out


# ---------------------------------------------------------------------------
# Checks and the truth.
# ---------------------------------------------------------------------------

def verify_printed(name, rec, rows):
    if len(rec) != len(rows):
        raise GenError("%s: %d rows drawn, %d planned" % (name, len(rec), len(rows)))
    for x in rec:
        if C.parse_card(*x["money"]) != x["r"]["amt"]:
            raise GenError("%s: amount %r" % (name, x["money"]))
        if C.parse_card(*x["bal"]) != x["r"]["bal"]:
            raise GenError("%s: balance %r" % (name, x["bal"]))


def verify_text_layer(name, pdf, truth_rows):
    lines = C.page_lines(pdf)
    seq = []
    for pno, y, ws in lines:
        toks = [w for _, _, w in ws]
        if len(toks) >= 4 and toks[0].isdigit() and toks[1] in C.MON_SET and ws[0][0] < X_DESC - 4:
            seq.append(toks)
    if len(seq) != len(truth_rows):
        raise GenError("%s: %d dated lines, %d truth rows" % (name, len(seq), len(truth_rows)))
    for toks, t in zip(seq, truth_rows):
        if " ".join(toks[:2]) != ddmon(dt.date.fromisoformat(t["date"])):
            raise GenError("%s: date %r vs %s" % (name, toks[:2], t["date"]))
        v = t["credit"] if t["credit"] is not None else t["debit"]
        if ML.mag(int(round(v * 100))) not in toks:
            raise GenError("%s: figure not on its line %r" % (name, toks))
        if ML.mag(int(round(abs(t["balance"]) * 100))) not in toks:
            raise GenError("%s: balance not on its line %r" % (name, toks))


def landmarks(name, pdf, sts):
    words = [w for _, _, _, w in C.reading_words(pdf)]
    at = 0
    for st in sts:
        i = C.find_seq(words, ["Account", "Summary"], at)
        if i < 0:
            raise GenError("%s: no Account Summary" % name)
        got = words[i + 2:i + 8]
        want = (["-"] if st["spec"].get("no_start") else ddmonyy(st["start"]).split()) \
            + ddmonyy(st["end"]).split()
        if got[:len(want)] != want:
            raise GenError("%s: period words %r" % (name, got))
        if C.find_seq(words, ["Payment"], i) < 0:
            raise GenError("%s: no Payment after the summary" % name)
        a = C.find_seq(words, ["Payment", "Advice"], i)
        e = C.find_seq(words, ["Instructions"], a)
        sub = " ".join(words[a + 2:e])
        if sub[-19:] != st["ident"]["card"]:
            raise GenError("%s: QVF card number would read %r" % (name, sub[-19:]))
        c = C.find_seq(words, ["Customer"], a)
        if " ".join(words[a + 2:c]) != st["ident"]["cust"][0]:
            raise GenError("%s: QVF name would read %r" % (name, " ".join(words[a + 2:c])))
        at = e
    for i, w in enumerate(words):
        if w == "Processed" and words[i - 1] == "Date":
            nxt = words[i + 1]
            if nxt not in ("Balance", "Opening") and not nxt.isdigit():
                raise GenError("%s: heading followed by %r" % (name, nxt))


def features(sts, npg):
    spec = sts[0]["spec"]
    f = ["bank:asb", "cols:date|desc|amount|pdate|balance|card", "date:dd Mon", "date_yearless",
         "font:Helvetica", "size:8", "page:A4", "pages:%d" % npg, "money:thousands",
         "sign:card_cr", "bal:card_cr", "token_column", "heading_every_page", "opening_line",
         "closing_line", "summary_box", "period:dd Mon yy", "pdate_after_amount",
         "two_line_heading"]
    if npg > len(sts):
        f.append("carried_forward")
    if any(st["start"].year != st["end"].year for st in sts) or \
            any(r["date"].year != st["end"].year for st in sts for r in st["rows"]):
        f.append("period_crosses_year")
    if spec.get("fx"):
        f += ["fx_in_description", "money_like_desc"]
    if spec.get("cards", 1) > 1:
        f.append("card_used_column")
    if spec.get("bundle"):
        f += ["bundle", "bundle:%d" % len(sts)]
    if spec.get("no_txn"):
        f.append("no_transactions")
    if spec.get("no_start"):
        f.append("period_no_start")
    if any(st["closing"] > 0 for st in sts):
        f.append("card_in_credit")
    if any(r["bal"] > 0 for st in sts for r in st["rows"]) and any(r["bal"] < 0 for st in sts for r in st["rows"]):
        f.append("balance_crosses_zero")
    if any(r["date"] < st["start"] for st in sts for r in st["rows"]):
        f.append("txn_date_before_period")
    f.append("qvf:" + spec["name"])
    return f


def build(spec, out):
    name = spec["name"]
    rng = random.Random(C.seed_for(name))
    cust = rng.choice(C.PEOPLE)
    card = card_no(rng)
    ident = dict(cust=cust, card=card, custno=str(rng.randint(1000000, 9999999)),
                 card4s=[card[-4:], "%04d" % rng.randint(0, 9999)])
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
        C.check_chain(name, st["opening"], st["rows"], st["closing"])
        verify_printed(name, rec, st["rows"])
        rows_out += [C.truth_row(r, j if spec.get("bundle") else None) for r in st["rows"]]
    sh.save()
    verify_text_layer(name, pdf, rows_out)
    landmarks(name, pdf, sts)
    truth = {
        "case": name,
        "generator": GEN,
        "note": "QVF lookalike. ASB, Visa credit card (synthetic). " + spec["note"],
        "bank": "ASB",
        "layout": "qvf_asb_visa",
        "product": "Visa credit card",
        "source_format": "pdf",
        "account_bank_code": None,
        "account_number": None,
        "account_redaction": None,
        "features": features(sts, npg),
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
    q = qvf_lite(pdf)
    want = [(t["date"], int(round(((t["credit"] or 0) - (t["debit"] or 0)) * 100))) for t in rows_out]
    got = [(r["date"].isoformat() if r.get("date") else None, r.get("amount")) for r in q]
    wantb = [int(round(t["balance"] * 100)) for t in rows_out]
    gotb = [r.get("balance") for r in q]
    same = sum(1 for a, b in zip(want, got) if a == b)
    sameb = sum(1 for a, b in zip(wantb, gotb) if a == b)
    return name, truth, npg, (len(want), len(got), same, sameb, got, want)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=C.SET_DIR)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    log = []
    for spec in SPECS:
        name, truth, npg, (nw, ng, same, sameb, got, want) = build(spec, a.out)
        print("%-12s %2d rows %d pg  qvf-lite: %d rows, %d match the key in place, %d balances"
              % (name, truth["row_count"], npg, ng, same, sameb))
        log.append("%s,%d,%d,%d,%d,%d" % (name, truth["row_count"], npg, ng, same, sameb))
        if same != nw or ng != nw:
            for k in range(max(nw, ng)):
                a1 = want[k] if k < nw else None
                b1 = got[k] if k < ng else None
                if a1 != b1:
                    print("     key %s  qvf-lite %s" % (a1, b1))
    with open(os.path.join(a.out, "qvf_lite_asb_visa.csv"), "w") as f:
        f.write("case,rows,pages,qvf_lite_rows,qvf_lite_same,qvf_lite_same_balance\n"
                + "\n".join(log) + "\n")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except GenError as e:
        sys.exit("GENERATOR SELF-CHECK FAILED: %s" % e)
