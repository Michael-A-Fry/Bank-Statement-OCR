#!/usr/bin/env python3
"""make_westpac_cc.py -- lookalikes of the "Westpac - Credit Card" statement type the
QVF reads (script.qvs lines 6966-7478).

What the QVF section expects, and so what every file here carries:
  * a title ending in the upper-case word "STATEMENT", then the cardholder's name,
    then the postal address (the name stops at the first word holding a digit, or
    at Flat / Unit / Apartment);
  * "Closing Balance $1,234.56" (or "$12.00 CR" for money owed TO the holder),
    followed in reading order by "Account Number <number>" and then
    "Statement Period dd/mm/yyyy - dd/mm/yyyy" and "Credit Limit ..." (the
    account number runs to the word "Statement"; the period runs to "Credit");
    no other "Account", "Current" or "Overdue" word in a statement, and the
    payment slip's own closing balance sits under "Payment Information" (the QVF
    ignores a "Closing Balance" after "Information" or "payment");
  * a table headed "... AMOUNT $" on every page that carries rows, ended by
    "Continued", "Westpac New Zealand Limited", "Interest rate", "Ways", "Cheque"
    or "Minimum";
  * rows: transaction date "dd Mon", processed date "dd Mon" (skipped), details,
    and ONE amount with no "$" and no thousands separator, followed by a separate
    "CR" for a payment or refund; a purchase has no mark;
  * a "General Payments & Charges" sub-heading at the top of the list, then one
    section per card headed by the cardholder's name and "**** **** **** 1234";
  * an overseas purchase carries a second line under its details,
    "USD 30.00 Foreign Currency Fee $0.92 included in amount" -- exactly two words
    before "Foreign" and four after "Fee", the window the QVF deletes;
  * the year is not printed on a row: it comes from the statement period.
Several statements in one file: each starts on a new page with its own header.

Truth: date = the transaction date (first date); the processed date is printed
beside it. A purchase is a debit and a payment/refund a credit (holder's side);
opening/closing are the amount owed NEGATED. No running balance is printed.

Run: python3 make_westpac_cc.py [--out DIR]
"""

import argparse
import datetime as dt
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from kbwp_common import (Sheet, GenError, MON, mag, write_json, OUT_DIR, BANNER,  # noqa: E402
                         rng_for, pick_dates, fill, chain_check, truth_doc, mole_words,
                         write_index, NAMES)

GEN = "scratchpad/qvf/gen/make_westpac_cc.py"
TYPE_KEY = "westpac_cc"
FONT = "Helvetica"
SIZE = 8.0
PITCH = 11.5
FX_PITCH = 10.0
X_TD, X_PD, X_DESC, DESC_MAX = 40.0, 92.0, 170.0, 470.0
R_NUM, R_CR = 536.0, 555.0
Y_LIMIT = 772.0

PURCHASES = [
    (8, "{grocer} {city}", (1200, 26000)),
    (4, "{cafe} {city}", (450, 3200)),
    (4, "{fuel} {city}", (4000, 15000)),
    (2, "{airline} {city}", (9000, 89000)),
    (2, "{telco} {city}", (3000, 12000)),
    (2, "{insurer} {city}", (4000, 21000)),
    (1, "ELECTRONICS WAREHOUSE {city}", (15000, 189000)),
]
REFUNDS = [
    "REFUND {grocer} {city}",
    "REFUND {airline} {city}",
]
FX_MERCH = [("WEBSHOP INTL SINGAPORE", "SGD"), ("STREAMFLIX.COM LOS ALTOS", "USD"),
            ("CLOUDBOX STORAGE DUBLIN", "EUR"), ("APPSTORE ONLINE SYDNEY", "AUD")]
CARDHOLDERS = ["MX ALEX SAMPLE", "MR JORDAN SAMPLE", "MS R DEMO", "DR P TESTER"]
ADDRS = [["14 EXAMPLE ROAD", "GREENVALE", "HAMILTON 3200"],
         ["FLAT 2, 9 SPECIMEN STREET", "DEMOTOWN 7020"],
         ["101 TEMPLATE TERRACE", "TESTBURY 9011"]]


def ddmon(d):
    return "%02d %s" % (d.day, MON[d.month - 1])


def dmy(d):
    return "%02d/%02d/%d" % (d.day, d.month, d.year)


def num(c):
    return mag(c, thousands=False)                 # 1250.00 -- no "$", no comma


def money_sum(c):
    """Summary figures: $1,234.56, and a credit balance as "$12.00 CR"."""
    return mag(c, thousands=True, dollar=True) + (" CR" if c < 0 else "")


def weighted(rng, items):
    tot = sum(i[0] for i in items)
    k = rng.uniform(0, tot)
    for it in items:
        k -= it[0]
        if k <= 0:
            return it
    return items[-1]


def gen_statement(rng, sp, opening_owed):
    start, end = sp["start"], sp["end"]
    sections = []
    gen_rows = []
    # General Payments & Charges: payments, interest, fees
    for _ in range(sp.get("payments", 1)):
        pd = rng.choice(list(range(3, 20)))
        p = start + dt.timedelta(days=pd)
        amt = sp.get("payment_amt") or max(2000, (opening_owed * rng.choice([40, 100, 100]) // 100))
        gen_rows.append(dict(date=p, pdate=p, desc="PAYMENT RECEIVED - THANK YOU", dir="C",
                             amt=amt))
    if sp.get("interest"):
        gen_rows.append(dict(date=end, pdate=end, desc="INTEREST CHARGED ON PURCHASES", dir="D",
                             amt=rng.randint(900, 4800), cat="interest"))
        gen_rows.append(dict(date=end, pdate=end, desc="INTEREST CHARGED ON CASH ADVANCES", dir="D",
                             amt=rng.randint(100, 900), cat="interest"))
    if sp.get("fee"):
        gen_rows.append(dict(date=start + dt.timedelta(days=1), pdate=start + dt.timedelta(days=1),
                             desc="ANNUAL CARD FEE", dir="D", amt=5000))
    gen_rows.sort(key=lambda r: (r["date"], r["pdate"]))
    sections.append(("General Payments & Charges", None, gen_rows))
    for k, holder in enumerate(sp["cards"]):
        n = sp["n"][k]
        pdates = pick_dates(rng, start, end, n)
        rows = []
        for j, p in enumerate(pdates):
            lag = rng.choice([0, 1, 1, 2, 3])
            d = p - dt.timedelta(days=lag)
            r = rng.random()
            if sp.get("fx") and r < 0.15:
                merch, cur = rng.choice(FX_MERCH)
                fxc = rng.randint(800, 9000)
                fee = max(10, fxc * 25 // 1000)
                nzd = fxc * 100 // rng.randint(55, 92) + fee
                rows.append(dict(date=d, pdate=p, desc=merch, dir="D", amt=nzd,
                                 fx="%s %s Foreign Currency Fee $%s included in amount"
                                    % (cur, mag(fxc, thousands=False), mag(fee))))
            elif r < 0.22:
                rows.append(dict(date=d, pdate=p, desc=fill(rng, rng.choice(REFUNDS)), dir="C",
                                 amt=rng.randint(1500, 22000)))
            else:
                _, tmpl, (lo, hi) = weighted(rng, PURCHASES)
                rows.append(dict(date=d, pdate=p, desc=fill(rng, tmpl), dir="D",
                                 amt=rng.randint(lo, hi)))
        for extra in sp.get("late", []) if k == 0 else []:
            rows.append(dict(extra))
        rows.sort(key=lambda r: (r["date"], r["pdate"]))
        sections.append((holder, sp["card_last4"][k], rows))
    return sections


def draw_page_top(sh, pno, total, first):
    sh.begin_page()
    sh.text(40, 18, BANNER, 6, color=(0.4, 0.4, 0.4))
    sh.text(40, 46, "Westpac", 20, bold=True)
    sh.text(555, 46, "Page %d of %d" % (pno, total), 8, align="right")


def draw_footer(sh, more):
    if more:
        sh.text(X_DESC, 788, "Continued over page", 7.5, color=(0.3, 0.3, 0.3))
    sh.text(40, 815, "Westpac New Zealand Limited", 7)
    sh.text(555, 815, "westpac.co.nz", 7, align="right")
    sh.end_page()


def draw_heading(sh, y):
    for x, s, al in ((X_TD, "TRANS DATE", "left"), (X_PD, "DATE PROCESSED", "left"),
                     (X_DESC, "TRANSACTION DETAILS", "left"), (R_CR, "AMOUNT $", "right")):
        sh.text(x, y, s, 7.5, bold=True, align=al)
    sh.line(40, y + 4, 555, y + 4, width=0.6)


def draw_header(sh, sp, st):
    sh.text(40, 76, "CREDIT CARD STATEMENT", 12, bold=True)
    y = 98
    for ln in [sp["cards"][0]] + sp["addr"]:
        sh.text(40, y, ln, 9)
        y += 11.5
    # "Westpac Cards" closes the QVF's name window (line 7120); one per statement
    y += 4
    sh.text(40, y, "Westpac Cards Customer Services 0800 000 000", 7.5, color=(0.3, 0.3, 0.3))
    y += 14
    box = [("Closing Balance", money_sum(st["closing_owed"])),
           ("Account Number", "**** **** **** %s" % sp["card_last4"][0]),
           ("Statement Period", "%s - %s" % (dmy(sp["start"]), dmy(sp["end"]))),
           ("Credit Limit", mag(sp["limit"], dollar=True)),
           ("Due Date", dmy(sp["end"] + dt.timedelta(days=25))),
           ("Minimum Payment", mag(st["minpay"], dollar=True))]
    for k, v in box:
        sh.text(40, y, k, 8.5, bold=(k == "Closing Balance"))
        sh.text(300, y, v, 8.5, bold=(k == "Closing Balance"), align="right")
        y += 12.5
    y += 8
    sh.text(40, y, "Statement Summary", 9, bold=True)
    y += 13
    summ = [("Opening Balance", money_sum(st["opening_owed"])),
            ("Payments & Credits", mag(st["tot_c"], dollar=True))]
    if sp.get("interest_line"):
        summ += [("Purchases & Debits", mag(st["tot_d"] - st["tot_i"], dollar=True)),
                 ("Interest Charged", mag(st["tot_i"], dollar=True))]
    else:
        summ += [("Purchases & Debits", mag(st["tot_d"], dollar=True))]
    for k, v in summ:
        sh.text(40, y, k, 8.5)
        sh.text(300, y, v, 8.5, align="right")
        y += 12.5
    return y + 16


def flatten(sections):
    """Printed lines: ("head", text, card4) or ("row", row)."""
    out = []
    for title, card4, rows in sections:
        if card4 is None:
            out.append(("general", title, None))
        else:
            out.append(("card", title, card4))
        for r in rows:
            out.append(("row", r, None))
    return out


def paginate(items, y1, yc, caps):
    pages, cur, y, k = [], [], y1, 0
    for it in items:
        h = PITCH + (FX_PITCH if it[0] == "row" and it[1].get("fx") else 0) + \
            (4 if it[0] in ("general", "card") else 0)
        cap = caps[min(k, len(caps) - 1)] if caps else 999
        nrows = sum(1 for x in cur if x[0] == "row")
        if cur and (y + h > Y_LIMIT or (it[0] == "row" and nrows >= cap)):
            pages.append(cur)
            cur, y, k = [], yc, k + 1
        cur.append(it)
        y += h
    pages.append(cur)
    # a section heading never ends a page
    for i in range(len(pages) - 1):
        while pages[i] and pages[i][-1][0] in ("general", "card"):
            pages[i + 1].insert(0, pages[i].pop())
    return [p for p in pages if p] or [[]]


def draw_statement(sh, sp, st, pages, pno0, total):
    for k, items in enumerate(pages):
        draw_page_top(sh, pno0 + k, total, k == 0)
        if k == 0:
            y = draw_header(sh, sp, st)
        else:
            y = 84
        draw_heading(sh, y)
        y += 16
        if not any(it[0] == "row" for p in pages for it in p):
            sh.text(X_DESC, y, "No transactions this statement period", SIZE)
            y += PITCH
        for it in items:
            if it[0] in ("general", "card"):
                y += 4
                sh.text(X_TD, y, it[1], SIZE, bold=True)
                if it[0] == "card":
                    sh.text(X_DESC, y, "**** **** **** %s" % it[2], SIZE, bold=True)
                y += PITCH
                continue
            r = it[1]
            sh.text(X_TD, y, ddmon(r["date"]), SIZE)
            sh.text(X_PD, y, ddmon(r["pdate"]), SIZE)
            a, b = sh.text(X_DESC, y, r["desc"], SIZE)
            sh.text(R_NUM, y, num(r["amt"]), SIZE, align="right")
            if r["dir"] == "C":
                sh.text(R_CR, y, "CR", SIZE, align="right")
            if b > R_NUM - 60:
                raise GenError("%s: details too long %r" % (sp["name"], r["desc"]))
            if r.get("fx"):
                y += FX_PITCH
                sh.text(X_DESC, y, r["fx"], 7.5)
            y += PITCH
        last = k == len(pages) - 1
        if last:
            y += 10
            sh.text(40, y, "Interest rate on purchases 20.95% p.a. Interest rate on cash "
                    "advances 22.95% p.a.", 7.5)
            y += 14
            sh.text(40, y, "Ways to pay: online banking, phone banking or at any branch.", 7.5)
            y += 20
            if y < 740:
                sh.text(40, y, "Payment Information", 8.5, bold=True)
                sh.text(40, y + 12, "Closing Balance", 8)
                sh.text(300, y + 12, money_sum(st["closing_owed"]), 8, align="right")
                sh.text(40, y + 24, "Card number", 8)
                sh.text(300, y + 24, "**** **** **** %s" % sp["card_last4"][0], 8, align="right")
        draw_footer(sh, not last)
    return len(pages)


# ---------------------------------------------------------------------------
# The QVF's token walk for this type, re-done as a self-check (script.qvs 6966-7478).
# ---------------------------------------------------------------------------

MONTHS = set(MON)


def _has_digit(s):
    return any(ch.isdigit() for ch in s)


def qvf_walk(W):
    # ASSUMPTION: the connector treats "&" as a delimiter (it is documented to split on
    # commas "among other characters"), so "General Payments & Charges" reaches the
    # script as three words -- which is what lines 7247-7249 remove.
    W = [w for w in W if w != "&"]
    n = len(W)
    prev = lambda i: W[i - 1] if i > 0 else None
    out = {"rows": [], "closing": [], "account": [], "name": [], "periods": [], "notes": []}
    ns = [i for i in range(n) if W[i] == "Period" and prev(i) == "Statement"]
    for p in ns:
        out["periods"].append((W[p + 1], W[p + 3]))
    # closing balance pairs (7024-7069)
    m = []
    for i in range(n):
        if W[i] == "Balance" and prev(i) == "Closing" and (W[i - 2] if i > 1 else None) not in ("Information", "payment"):
            m.append((i, "B"))
        elif W[i] in ("Account", "Current", "Overdue"):
            m.append((i, W[i]))
    keep = []
    for k, x in enumerate(m):
        nxt = m[k + 1][1] if k + 1 < len(m) else None
        if k == len(m) - 1 or x[1] in ("Account", "Current") or nxt in ("Account", "Current"):
            keep.append(x)
    for k in range(0, len(keep) - 1, 2):
        a, b = keep[k], keep[k + 1]
        if a[1] == "B":
            out["closing"].append(" ".join(W[a[0] + 1:b[0]]))
        else:
            out["notes"].append("closing-balance pair starts on %s" % a[1])
    # account numbers (7073-7110)
    m = [(i, "N") for i in range(n) if W[i] == "Number" and prev(i) == "Account"] + \
        [(i, "E") for i in range(n) if W[i] in ("Statement", "Payments", "Page")]
    m.sort()
    s2 = [x for k, x in enumerate(m) if x[1] == "E" or (k + 1 < len(m) and m[k + 1][1] == "E")]
    s3 = [x for k, x in enumerate(s2) if x[1] == "N" or (k > 0 and s2[k - 1][1] == "N")]
    for k in range(0, len(s3) - 1, 2):
        out["account"].append(" ".join(W[s3[k][0] + 1:s3[k + 1][0]]))
    # names (7112-7134)
    m = [(i, "S") for i in range(n) if W[i] == "STATEMENT"] + \
        [(i + 8, "S") for i in range(n) if W[i] == "ite"] + \
        [(i, "E") for i in range(n) if W[i] == "Cards" and prev(i) == "Westpac"]
    m.sort()
    s1 = [x for k, x in enumerate(m) if x[1] == "S" or (k > 0 and m[k - 1][1] == "S")]
    for k in range(0, len(s1), 2):
        a = s1[k][0]
        b = s1[k + 1][0] if k + 1 < len(s1) else n
        nm = []
        for w in W[a + 1:b]:
            if w in ("Flat", "Unit", "Apartment") or _has_digit(w):
                break
            nm.append(w)
        out["name"].append(" ".join(nm))
    # transaction words (7136-7255)
    m = []
    for i in range(n):
        w = W[i]
        if w == "$" and prev(i) == "AMOUNT":
            m.append((i, "S"))
        elif w in ("Ways", "Continued", "Cheque", "Minimum"):
            m.append((i, "E"))
        elif w.lower() == "rate" and prev(i) == "Interest":
            m.append((i - 1, "E"))
        elif w == "New" and prev(i) == "Westpac":
            m.append((i - 1, "E"))
        elif w == "number" and prev(i) == "Account":
            m.append((i - 1, "E"))
    m.sort()
    s1 = [x for k, x in enumerate(m) if x[1] == "S" or (k > 0 and m[k - 1][1] == "S")]
    body = []
    for k in range(0, len(s1) - 1, 2):
        a, b = s1[k][0], s1[k + 1][0]
        seg = list(range(a + 1, b))
        for j, p in enumerate(seg):
            pv = W[p - 1]
            if not (W[p] != "****" and pv == "****"):
                body.append((p, W[p]))
    if not body:
        out["notes"].append("no transaction words between the table landmarks")
        return out
    last = body[-1][0]
    body += [(p, "New Statement") for p in ns if p < last]
    body.sort()
    T = [s for _, s in body]

    def one_pass(T, rev):
        seq = T[::-1] if rev else T
        keep, pv = [], None
        for s in seq:
            drop = (not _has_digit(s) and pv == "****" and s != "CR" and (not rev or s != "New Statement")) \
                or (s.startswith("*") and sum(ch.isdigit() for ch in s) == 4)
            if not drop:
                keep.append(s)
            pv = s
        return keep[::-1] if rev else keep
    for _ in range(10):
        T = one_pass(T, False)
    for _ in range(10):
        T = one_pass(T, True)
    flag = [0] * len(T)
    for i in range(3, len(T)):
        if T[i - 1] == "Fee" and T[i - 2] == "Currency" and T[i - 3] == "Foreign":
            flag[i] = 1
    fx = [1 if (flag[i] or any(flag[i - j] for j in (1, 2, 3) if i - j >= 0)) else 0
          for i in range(len(T))]
    fy = [1 if (fx[i] or any(fx[i + j] for j in range(1, 6) if i + j < len(T))) else 0
          for i in range(len(T))]
    T2 = []
    for i, s in enumerate(T):
        pv = T[i - 1] if i > 0 else None
        if s == "****" or fy[i] == 1:
            continue
        if (s == "General" and pv == "New Statement") or (s == "Payments" and pv == "General") \
                or (s == "Charges" and pv == "Payments"):
            continue
        T2.append(s)
    T = T2

    def to_date(p, stmt_seen):
        p += 1
        while p < len(T) and T[p] not in MONTHS:
            if T[p] == "New Statement":
                stmt_seen += 1
            p += 1
        if p >= len(T):
            return None, stmt_seen
        return p - 1, stmt_seen

    stmt = 1
    pos, _ = to_date(0, 0)
    if pos is None:
        out["notes"].append("no date after the table start: the QVF's date search never ends")
        return out
    nsi = 1                                       # NS markers seen so far

    def period(stmt):
        a, b = out["periods"][stmt - 1]
        sd = dt.datetime.strptime(a, "%d/%m/%Y").date()
        ed = dt.datetime.strptime(b, "%d/%m/%Y").date()
        return sd, ed
    while pos < len(T):
        if T[pos] == "New Statement":
            q, _ = to_date(pos, 0)
            if q is None:
                break
            # line 7276: the statement number is read off the word just before the
            # first date, which must therefore be the New Statement marker itself
            if T[q - 1] != "New Statement":
                out["notes"].append("statement number lost: %r sits before the first date" % T[q - 1])
            stmt = sum(1 for x in T[:q] if x == "New Statement")
            pos = q
        cols, cur, col = [], [], 1
        while pos < len(T):
            s = T[pos]
            p1 = T[pos + 1] if pos + 1 < len(T) else None
            p2 = T[pos + 2] if pos + 2 < len(T) else None
            p3 = T[pos + 3] if pos + 3 < len(T) else None
            end_col = end_row = False
            if col == 3 and (p2 in MONTHS or p1 == "New Statement"):
                end_col = end_row = True
            skip2 = False
            if col == 1 and s in MONTHS:
                end_col = True
                skip2 = True
            if col == 2 and ((p1 is not None and p1.replace(".", "").isdigit() and p1.count(".") == 1
                              and len(p1.split(".")[1]) == 2)
                             and (p2 in ("New Statement", "CR") or p3 in MONTHS or p3 in ("New Statement", "CR"))
                             or pos + 1 == len(T) - 1):
                end_col = True
            if pos == len(T) - 1:
                end_col = end_row = True
            cur.append(s)
            pos += 3 if skip2 else 1
            if end_col:
                cols.append(" ".join(cur))
                cur = []
                col += 1
                if end_row:
                    break
        if len(cols) < 3:
            out["rows"].append(("?", " ".join(cols), None, stmt))
            continue
        d, det, a = cols
        if "Foreign" in a:
            a = a[:a.index(".") + 3]
        if "CR" in a:
            a = a[:-3]
        else:
            a = "-" + a
        a = a.replace(" ", "")
        sd, ed = period(stmt)
        mon = d.split(" ")[1]
        yr = sd.year if (MON[sd.month - 1] in ("Oct", "Nov", "Dec") and mon in ("Oct", "Nov", "Dec")) else ed.year
        try:
            dd = dt.datetime.strptime("%s %d" % (d, yr), "%d %b %Y").date().isoformat()
        except ValueError:
            dd = "?" + d
        out["rows"].append((dd, det, a, stmt))
    return out


def qvf_selfcheck(pdf, sts):
    got = qvf_walk(mole_words(pdf))
    bad, diverge = [], []
    want = []
    for j, (sp, st) in enumerate(sts):
        for title, card4, rows in st["sections"]:
            for r in rows:
                want.append((r["date"].isoformat(), r["desc"],
                             ("" if r["dir"] == "C" else "-") + num(r["amt"]), j + 1))
    if len(got["rows"]) != len(want):
        bad.append("rows %d, want %d" % (len(got["rows"]), len(want)))
    for g, w in zip(got["rows"], want):
        if g[1:] != w[1:]:
            bad.append("got %r want %r" % (g, w))
        elif g[0] != w[0]:
            diverge.append("QVF dates %s as %s" % (w[0], g[0]))
    for j, (sp, st) in enumerate(sts):
        if j < len(got["closing"]):
            if got["closing"][j] != money_sum(st["closing_owed"]):
                bad.append("closing %r want %r" % (got["closing"][j], money_sum(st["closing_owed"])))
        else:
            bad.append("no closing balance for statement %d" % (j + 1))
        acct = "**** **** **** %s" % sp["card_last4"][0]
        if j >= len(got["account"]) or got["account"][j] != acct:
            bad.append("account %r" % (got["account"][j:j + 1],))
        if j >= len(got["name"]) or got["name"][j] != sp["cards"][0]:
            nm = got["name"][j] if j < len(got["name"]) else None
            if nm and nm.startswith(sp["cards"][0]) and sp["addr"][0].startswith("FLAT"):
                diverge.append("QVF account name runs on into the address (%r): it stops at "
                               "'Flat' only in title case" % nm)
            else:
                bad.append("name %r" % (nm,))
    notes = got["notes"]
    if not want and notes == ["no date after the table start: the QVF's date search never ends"]:
        diverge += notes
        notes = []
    return bad + notes, diverge


# ---------------------------------------------------------------------------
# The statements
# ---------------------------------------------------------------------------

D = dt.date


def late_rows():
    return [dict(date=D(2023, 12, 30), pdate=D(2024, 1, 3), desc="KAURI SUPERSTORE AUCKLAND",
                 dir="D", amt=8743),
            dict(date=D(2023, 12, 31), pdate=D(2024, 1, 4), desc="TOTARA FUELS NAPIER",
                 dir="D", amt=11290)]


CASES = [
    dict(case="wpcc_1", note="One statement, one page, one card; a payment, purchases and a refund.",
         stmts=[dict(start=D(2024, 2, 15), end=D(2024, 3, 14), cards=[CARDHOLDERS[0]],
                     card_last4=["4821"], addr=ADDRS[0], n=[14], opening=103420, limit=800000)]),
    dict(case="wpcc_2", note="Three pages, a primary and an additional card, overseas purchases "
         "with a Foreign Currency Fee line, 'Continued over page', amounts over $1,000 printed "
         "without a thousands separator.",
         stmts=[dict(start=D(2024, 4, 9), end=D(2024, 5, 8), cards=[CARDHOLDERS[1], CARDHOLDERS[2]],
                     card_last4=["5310", "5328"], addr=ADDRS[2], n=[34, 22], opening=462015,
                     limit=1500000, fx=True, payments=2, fee=True, caps=[16, 28])]),
    dict(case="wpcc_3", note="The statement period crosses a new year (Dec-Jan rows dated by the "
         "QVF's Oct/Nov/Dec rule); interest charged; the summary prints interest apart from "
         "Purchases & Debits.",
         stmts=[dict(start=D(2023, 12, 16), end=D(2024, 1, 15), cards=[CARDHOLDERS[0]],
                     card_last4=["4821"], addr=ADDRS[0], n=[24], opening=238811, limit=800000,
                     interest=True, interest_line=True, payment_amt=60000)]),
    dict(case="wpcc_4", note="A credit balance: the payment overshoots, so the closing balance "
         "prints with CR; refunds; a payment over $1,000.",
         stmts=[dict(start=D(2024, 6, 1), end=D(2024, 6, 30), cards=[CARDHOLDERS[3]],
                     card_last4=["4407"], addr=ADDRS[1], n=[10], opening=118000, limit=500000,
                     payment_amt=260000)]),
    dict(case="wpcc_5", note="Three consecutive monthly statements concatenated in one file "
         "(each restarts 'Page 1 of N'); each closing balance is the next opening balance.",
         chain=True,
         stmts=[dict(start=D(2024, 7, 15), end=D(2024, 8, 14), cards=[CARDHOLDERS[0]],
                     card_last4=["4821"], addr=ADDRS[0], n=[12], opening=74550, limit=800000),
                dict(start=D(2024, 8, 15), end=D(2024, 9, 14), cards=[CARDHOLDERS[0]],
                     card_last4=["4821"], addr=ADDRS[0], n=[16], limit=800000, fx=True),
                dict(start=D(2024, 9, 15), end=D(2024, 10, 14), cards=[CARDHOLDERS[0]],
                     card_last4=["4821"], addr=ADDRS[0], n=[9], limit=800000, interest=True)]),
    dict(case="wpcc_6", note="No transactions in the period: a credit balance carried forward "
         "unchanged (the QVF's date search has nothing to find).",
         stmts=[dict(start=D(2024, 3, 1), end=D(2024, 3, 31), cards=[CARDHOLDERS[2]],
                     card_last4=["5328"], addr=ADDRS[2], n=[0], opening=-2500, limit=300000,
                     payments=0)]),
    dict(case="wpcc_7", note="A January statement with purchases made on 30 and 31 December "
         "and processed in January: the transaction date falls before the period and in "
         "the previous year (the QVF's year rule dates them a year late).",
         stmts=[dict(start=D(2024, 1, 3), end=D(2024, 2, 2), cards=[CARDHOLDERS[1]],
                     card_last4=["5310"], addr=ADDRS[2], n=[15], opening=152290, limit=1500000,
                     late=late_rows())]),
]


def build(case, out_dir):
    name = case["case"]
    rng = rng_for(name)
    sh = Sheet(os.path.join(out_dir, name + ".pdf"), (595.27, 841.89), FONT)
    sts = []
    owed = None
    for j, sp0 in enumerate(case["stmts"]):
        sp = dict(sp0, name="%s/%d" % (name, j))
        opening_owed = sp["opening"] if (not case.get("chain") or j == 0) else owed
        if sp.get("payments", 1) and opening_owed <= 0 and not sp.get("payment_amt"):
            sp["payments"] = 0
        sections = gen_statement(rng, sp, max(opening_owed, 5000))
        rows = [r for _, _, rs in sections for r in rs]
        tot_d = sum(r["amt"] for r in rows if r["dir"] == "D")
        tot_c = sum(r["amt"] for r in rows if r["dir"] == "C")
        tot_i = sum(r["amt"] for r in rows if r.get("cat") == "interest")
        closing_owed = opening_owed + tot_d - tot_c
        owed = closing_owed
        st = dict(sections=sections, opening_owed=opening_owed, closing_owed=closing_owed,
                  tot_d=tot_d, tot_c=tot_c, tot_i=tot_i,
                  minpay=max(0, min(closing_owed, max(1000, closing_owed * 3 // 100))))
        items = flatten(sections) if rows else []
        st["pages"] = paginate(items, 0, 0, sp.get("caps")) if items else [[]]
        sts.append((sp, st))
    # real pagination needs the header height of page 1: draw-independent estimate
    for sp, st in sts:
        items = [it for p in st["pages"] for it in p]
        y1 = 98 + 11.5 * (1 + len(sp["addr"])) + 18 + 12.5 * 6 + 8 + 13 + \
            12.5 * (4 if sp.get("interest_line") else 3) + 16 + 16
        st["pages"] = paginate(items, y1, 100, sp.get("caps")) if items else [[]]
    total = sum(len(st["pages"]) for _, st in sts)
    for sp, st in sts:
        # each statement is its own document: "Page 1 of N" restarts
        draw_statement(sh, sp, st, st["pages"], 1, len(st["pages"]))
    sh.save()

    bundle = len(sts) > 1
    rows_t, statements = [], []
    for j, (sp, st) in enumerate(sts):
        part = []
        for title, card4, rows in st["sections"]:
            for r in rows:
                desc = r["desc"] + (" " + r["fx"] if r.get("fx") else "")
                t = {"date": r["date"].isoformat(), "description": desc,
                     "debit": r["amt"] / 100.0 if r["dir"] == "D" else None,
                     "credit": r["amt"] / 100.0 if r["dir"] == "C" else None,
                     "balance": None}
                if bundle:
                    t["statement_index"] = j
                part.append(t)
        chain_check(sp["name"], part, -st["opening_owed"] / 100.0, -st["closing_owed"] / 100.0,
                    newest=False)
        rows_t += part
        statements.append({"statement_index": j, "period_start": sp["start"].isoformat(),
                           "period_end": sp["end"].isoformat(),
                           "opening_balance": -st["opening_owed"] / 100.0,
                           "closing_balance": -st["closing_owed"] / 100.0})
    sp0, st0 = sts[0]
    feats = ["bank:westpac", "qvf:westpac_cc", "cols:date|pdate|desc|amount", "date:dd Mon",
             "date_yearless", "sign:card_cr", "money:no_thousands", "no_balance_column",
             "card_sections", "cardholder_lines", "summary_box", "heading_every_page",
             "pages:%d" % total]
    rows_all = [r for _, st in sts for _, _, rs in st["sections"] for r in rs]
    if any(r.get("fx") for r in rows_all):
        feats += ["fx_second_line", "money_like_desc"]
    if any(sp["start"].year != sp["end"].year for sp, _ in sts):
        feats.append("period_crosses_year")
    if any(r["date"] < sp["start"] for sp, st in sts for _, _, rs in st["sections"] for r in rs):
        feats.append("date_before_period")
    if any(r["date"].year != sp["start"].year and r["date"] < sp["start"]
           for sp, st in sts for _, _, rs in st["sections"] for r in rs):
        feats.append("date_previous_year")
    if any(st["closing_owed"] < 0 or st["opening_owed"] < 0 for _, st in sts):
        feats.append("credit_balance_cr")
    if any(len(sp["cards"]) > 1 for sp, _ in sts):
        feats.append("two_cards")
    if any(sp.get("interest_line") for sp, _ in sts):
        feats.append("totals_exclude_interest")
    if any(r["amt"] >= 100000 for r in rows_all):
        feats.append("large_amounts")
    if not rows_all:
        feats.append("no_transactions")
    if bundle:
        feats += ["bundle", "bundle:%d" % len(sts), "page_numbers_restart"]
    truth = truth_doc(
        case=name, generator=GEN,
        note="QVF lookalike (Westpac - Credit Card). " + case["note"],
        bank="Westpac", layout=TYPE_KEY, product="Credit card", account_bank_code=None,
        account_number="**** **** **** %s" % sp0["card_last4"][0], features=feats,
        row_order="oldest_first", opening=-st0["opening_owed"] / 100.0,
        closing=-sts[-1][1]["closing_owed"] / 100.0, rows=rows_t,
        statements=statements if bundle else None)
    write_json(os.path.join(out_dir, name + ".truth.json"), truth)
    bad, diverge = qvf_selfcheck(os.path.join(out_dir, name + ".pdf"), sts)
    return truth, total, bad, diverge


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=OUT_DIR)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    index = []
    for case in CASES:
        truth, pages, bad, diverge = build(case, a.out)
        print("%-8s %3d rows %d pages  QVF walk: %s%s" % (
            truth["case"], truth["row_count"], pages, "ok" if not bad else "; ".join(bad[:4]),
            ("  [expected QVF divergence: %s]" % "; ".join(diverge)) if diverge else ""))
        index.append({"case": truth["case"], "file": truth["case"] + ".pdf", "bank": "Westpac",
                      "layout": TYPE_KEY, "rows": truth["row_count"], "pages": pages,
                      "features": truth["features"], "qvf_walk_ok": not bad,
                      "qvf_divergence": diverge})
    write_index(a.out, index, "index_%s.json" % TYPE_KEY)
    return 0


if __name__ == "__main__":
    sys.exit(main())
