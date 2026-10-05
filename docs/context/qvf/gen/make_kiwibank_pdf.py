#!/usr/bin/env python3
"""make_kiwibank_pdf.py -- lookalikes of the "Kiwibank - PDF" statement type the QVF
reads (script.qvs lines 6497-6965): a Kiwibank transaction-account ENQUIRY report.

What the QVF section expects, and so what every file here carries:
  * page furniture with a phone line ending "33 55" directly followed (in reading
    order) by "www.kiwibank.co.nz" at the top of EVERY page, and a footer that
    starts with "www.kiwibank.co.nz" (the end-of-table landmark on every page);
  * a header per statement: "Name <holder> Address <...> Account No <number>
    Enquiry Period <dd Mon yyyy> to <dd Mon yyyy>" ("Enquiry Period" splits
    statements; Name..Address is the account name; Account No..Enquiry is the
    account number);
  * a table headed "... Debit Amt Credit Amt Balance" (the word after "Credit Amt"
    is skipped), rows NEWEST FIRST, each row = Posted date (dd Mon yyyy, skipped by
    the QVF), transaction Date (dd Mon yyyy), Details, ONE "$" amount (in the Debit
    Amt or the Credit Amt column; the QVF cannot tell which, so it signs by keyword
    or by the balance difference), and a "$" balance ("-$" when overdrawn);
  * continuation pages either repeat the column heading or go straight from the
    "www.kiwibank.co.nz" header line into the rows;
  * each statement starts on a new page; a statement with no rows is allowed only
    inside a bundle (the QVF counts it while skipping to the next date).
Wrapped details put the money on the LAST line of the row, the only wrapping the
QVF's token walk survives.

Truth: date = the transaction Date column (the second date, which the QVF keeps);
the Posted date is printed beside it and is not the truth date.

Run: python3 make_kiwibank_pdf.py [--out DIR]
"""

import argparse
import datetime as dt
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from kbwp_common import (Sheet, GenError, MON, mag, write_json, OUT_DIR, BANNER,  # noqa: E402
                         rng_for, pick_dates, fill, chain_check, truth_doc, mole_words,
                         write_index)

GEN = "scratchpad/qvf/gen/make_kiwibank_pdf.py"
TYPE_KEY = "kiwibank_pdf"
FONT = "Helvetica"
SIZE = 8.0
PITCH = 12.0
WRAP_PITCH = 10.0

# Column anchors (points from the left edge).
X_POSTED, X_DATE, X_DESC, DESC_W = 40.0, 100.0, 160.0, 205.0
R_DEBIT, R_CREDIT, R_BAL = 432.0, 497.0, 555.0
Y_LIMIT = 778.0

HOLDERS = [
    ("ALEX J SAMPLE", ["12 EXAMPLE STREET, SAMPLEVILLE", "TESTBURY 9010"]),
    ("MS R DEMO", ["FLAT 3, 45 SPECIMEN ROAD", "DEMOTOWN 7020"]),
    ("J TESTER & K TESTER", ["7 PLACEHOLDER LANE, RD 2", "MOCKBURN 9310"]),
]

# (weight, template, amount range in cents or "atm", card-like: posted after the txn date)
DEBITS = [
    (8, "POS W/D {grocer}-{time}", (1200, 31000), True),
    (5, "POS W/D {cafe}-{time}", (400, 3800), True),
    (3, "POS W/D {fuel}-{time}", (3500, 16000), True),
    (2, "ATM W/D KIWIBANK {atm}-{time}", "atm", False),
    (2, "PAY {person} {ref}", (2000, 60000), False),
    (2, "TRF TO {own}", (5000, 150000), False),
    (2, "BILL PAYMENT {council} {inv}", (9000, 165000), False),
    (2, "AP DIRECT DEBIT {utility} {custno}", (6000, 34000), False),
    (2, "VISA DEBIT {online}", (500, 8000), True),
]
CREDITS = [
    (3, "Direct Credit {employer} SALARY", (180000, 420000), False),
    (2, "TRANSFER FROM {own}", (5000, 150000), False),
    (2, "FROM {person} {ref}", (2000, 50000), False),
    (1, "CASH DEPOSIT {branch} BRANCH", (2000, 90000), False),
    (1, "POS DEP {grocer} REFUND", (500, 12000), True),
    (1, "POS W/D {cafe}-{time} REVRSL", (400, 3800), True),
]
LONG = [
    ("D", "PAY {person} RENT FOR TWO WEEKS FLAT 2 REF {ref} PARTICULARS RENT CODE BOND", (38000, 76000)),
    ("D", "AP DIRECT DEBIT {insurer} POLICY {custno} MONTHLY PREMIUM HOUSE AND CONTENTS", (6000, 21000)),
    ("C", "Direct Credit {employer} SALARY FORTNIGHTLY PAY PERIOD ENDING WITH ALLOWANCES", (180000, 320000)),
    ("D", "BILL PAYMENT {council} {inv} QUARTERLY INSTALMENT PROPERTY RATES ACCOUNT", (40000, 90000)),
]


def fmt_date(d):
    return "%02d %s %d" % (d.day, MON[d.month - 1], d.year)


def fmt_amt(c):
    return mag(c, thousands=True, dollar=True)


def fmt_bal(c):
    return ("-" if c < 0 else "") + mag(c, thousands=True, dollar=True)


def weighted(rng, items):
    tot = sum(i[0] for i in items)
    k = rng.uniform(0, tot)
    for it in items:
        k -= it[0]
        if k <= 0:
            return it
    return items[-1]


def gen_rows(rng, sp):
    """Oldest first: posted date, txn date, desc, dir, amount (cents)."""
    n = sp["n"]
    if n == 0:
        return []
    start, end = sp["start"], sp["end"]
    posted = pick_dates(rng, start, end, n)
    rows = []
    extra = {"own": sp.get("own") or ["38-9012-0345678-01"]}
    for p in posted:
        if sp.get("wrap") and rng.random() < 0.25:
            d, tmpl, (lo, hi) = rng.choice(LONG)
            amt, card = rng.randint(lo, hi), False
        elif rng.random() < sp.get("credit_share", 0.25):
            _, tmpl, rngc, card = weighted(rng, CREDITS)
            d, amt = "C", rng.randint(*rngc)
        else:
            _, tmpl, rngc, card = weighted(rng, DEBITS)
            d = "D"
            amt = rng.randrange(20, 420, 20) * 100 if rngc == "atm" else rng.randint(*rngc)
        lag = rng.choice([0, 1, 1, 2, 3]) if card else 0
        rows.append(dict(posted=p, date=p - dt.timedelta(days=lag), desc=fill(rng, tmpl, extra),
                         dir=d, amt=amt))
    if sp.get("interest"):
        # interest and its withholding tax on the last day, as a bank posts them
        i = rng.randint(15, 1400)
        rows.append(dict(posted=end, date=end, desc="INTEREST CREDIT", dir="C", amt=i))
        rows.append(dict(posted=end, date=end, desc="WITHHOLDING TAX", dir="D",
                         amt=max(1, i * 33 // 100)))
    if sp.get("zero_oldest"):
        rows.insert(0, dict(posted=start, date=start, desc="MONTHLY ACCOUNT FEE WAIVED",
                            dir="D", amt=0))
    return rows


def set_balances(rng, rows, sp):
    """Choose the opening so the running balance does (or does not) go overdrawn."""
    run, lo = 0, 0
    for r in rows:
        run += r["amt"] if r["dir"] == "C" else -r["amt"]
        lo = min(lo, run)
    if sp.get("opening") is not None:
        opening = sp["opening"]
    elif sp.get("od"):
        opening = -lo - rng.randint(15000, 60000)       # dips 150-600 below zero
        if -lo < 30000:
            raise GenError("%s: not enough outgoings to go overdrawn" % sp["name"])
    else:
        opening = -lo + rng.randint(5000, 250000)
    bal = opening
    for r in rows:
        bal += r["amt"] if r["dir"] == "C" else -r["amt"]
        r["bal"] = bal
    if sp.get("od") and not any(r["bal"] < 0 for r in rows):
        raise GenError("%s: OD requested but never overdrawn" % sp["name"])
    if not sp.get("od") and sp.get("opening") is None and any(r["bal"] < 0 for r in rows):
        raise GenError("%s: unexpected overdraft" % sp["name"])
    return opening, bal


def wrap_desc(sh, s, width):
    words, lines, cur = s.split(" "), [], ""
    for w in words:
        t = (cur + " " + w).strip()
        if sh.width(t, SIZE) <= width:
            cur = t
        else:
            lines.append(cur)
            cur = w
    lines.append(cur)
    return [x for x in lines if x]


# ---------------------------------------------------------------------------
# Drawing
# ---------------------------------------------------------------------------

def page_top(sh, pno, npages_label):
    sh.begin_page()
    sh.text(40, 18, BANNER, 6, color=(0.4, 0.4, 0.4))
    sh.text(40, 44, "Kiwibank", 18, bold=True)
    sh.text(555, 50, "0800 11 33 55", 8, align="right")
    sh.text(555, 61, "www.kiwibank.co.nz", 8, align="right")


def page_bottom(sh, pno, total):
    sh.text(40, 815, "www.kiwibank.co.nz", 7)
    sh.text(297, 815, "Kiwibank Limited", 7, align="center")
    sh.text(555, 815, "Page %d of %d" % (pno, total), 7, align="right")
    sh.end_page()


def draw_heading(sh, y):
    for x, s, al in ((X_POSTED, "Posted", "left"), (X_DATE, "Date", "left"),
                     (X_DESC, "Details", "left"), (R_DEBIT, "Debit Amt", "right"),
                     (R_CREDIT, "Credit Amt", "right"), (R_BAL, "Balance", "right")):
        sh.text(x, y, s, SIZE, bold=True, align=al)
    sh.line(40, y + 4, 555, y + 4, width=0.6)


def draw_header(sh, sp, y0=86):
    sh.text(40, y0, "Transaction History", 12, bold=True)
    holder, addr = sp["holder"]
    lab = [("Name", [holder]), ("Address", addr), ("Account No", [sp["account"]]),
           ("Enquiry Period", ["%s to %s" % (fmt_date(sp["start"]), fmt_date(sp["end"]))]),
           ("Account Type", [sp["product"]])]
    y = y0 + 24
    for k, vals in lab:
        sh.text(40, y, k, 8.5, bold=True)
        for j, v in enumerate(vals):
            sh.text(130, y + 11 * j, v, 8.5)
        y += 11 * len(vals) + 3
    return y + 14


def layout_pages(sh, sp, printed):
    """Split the printed rows into pages; returns a list of lists of row indices."""
    pages, cur = [], []
    first_y = sp["_table_y1"]
    y = first_y
    cap = sp.get("cap1", 99)
    for i, r in enumerate(printed):
        h = PITCH + WRAP_PITCH * (len(r["lines"]) - 1)
        if cur and (y + h > Y_LIMIT or len(cur) >= cap):
            pages.append(cur)
            cur = []
            y = sp["_table_yc"]
            cap = sp.get("capc", 99)
        cur.append(i)
        y += h
    pages.append(cur)
    return pages


def draw_statement(sh, sp, printed, pno0, total):
    pages = sp["_pages"]
    for k, idx in enumerate(pages):
        page_top(sh, pno0 + k, total)
        if k == 0:
            yh = draw_header(sh, sp)
            draw_heading(sh, yh)
            y = yh + 16
        else:
            if sp.get("head_every_page"):
                draw_heading(sh, 84)
                y = 100
            else:
                y = 84
        if not idx and k == 0 and not printed:
            sh.text(X_DESC, y, "No transactions found.", SIZE)
        for i in idx:
            r = printed[i]
            nl = len(r["lines"])
            ylast = y + WRAP_PITCH * (nl - 1)
            sh.text(X_POSTED, y, fmt_date(r["posted"]), SIZE)
            sh.text(X_DATE, y, fmt_date(r["date"]), SIZE)
            for j, ln in enumerate(r["lines"]):
                a, b = sh.text(X_DESC, y + WRAP_PITCH * j, ln, SIZE)
                if b > X_DESC + DESC_W + 0.01:
                    raise GenError("%s: details spill: %r" % (sp["name"], ln))
            # money on the LAST line of the row (the QVF's token walk needs it there)
            col = R_DEBIT if r["dir"] == "D" else R_CREDIT
            sh.text(col, ylast, fmt_amt(r["amt"]), SIZE, align="right")
            sh.text(R_BAL, ylast, fmt_bal(r["bal"]), SIZE, align="right")
            y = ylast + PITCH
        page_bottom(sh, pno0 + k, total)
    return len(pages)


# ---------------------------------------------------------------------------
# The QVF's token walk, re-done in Python as a self-check (script.qvs 6497-6965).
# ---------------------------------------------------------------------------

MONTHS = set(MON)


def qvf_walk(words):
    """Return (statements: list of row lists, notes). Each row: (date str, details,
    amount str, balance str, statement number)."""
    W = list(words)
    n = len(W)
    prev = lambda i: W[i - 1] if i > 0 else None
    ns = [i for i in range(n) if W[i] == "Period" and prev(i) == "Enquiry"]
    marks = []                                     # (pos, kind)
    for i in range(n):
        if W[i] == "Amt" and prev(i) == "Credit":
            marks.append((i + 1, "S"))
        elif W[i] == "55" and prev(i) == "33":
            marks.append((i + 1, "S"))
        elif W[i] == "www.kiwibank.co.nz" and prev(i) != "55":
            marks.append((i, "E"))
    marks.sort()
    step1 = [m for k, m in enumerate(marks) if m[1] == "S" or (k > 0 and marks[k - 1][1] == "S")]
    step2 = [m for k, m in enumerate(step1)
             if m[1] == "E" or (k + 1 < len(step1) and step1[k + 1][1] == "E")]
    body = []
    for k in range(0, len(step2) - 1, 2):
        a, b = step2[k][0], step2[k + 1][0]
        body += [(p, W[p]) for p in range(a + 1, b)]
    last = body[-1][0] if body else -1
    body += [(p, "New Statement") for p in ns if p < last]   # the evident intent of line 6620
    body.sort()
    T = [s for _, s in body]
    rows, pos, stmt = [], 0, 1

    def to_date(p, stmt):
        p += 1
        while p < len(T) and T[p] not in MONTHS:
            if T[p] == "New Statement":
                stmt += 1
            p += 1
        return p - 1, stmt

    if not T:
        return rows
    pos, stmt = to_date(0, stmt)
    if T[0] == "New Statement" and pos >= 0:
        pass
    pos += 3                                       # skip the Posted date
    while pos < len(T):
        if T[pos] == "New Statement":
            pos, stmt = to_date(pos, stmt)
            pos += 3
            stmt += 1
        cols, col = [], 1
        cur = []
        while pos < len(T):
            s = T[pos]
            p1 = T[pos + 1] if pos + 1 < len(T) else None
            p2 = T[pos + 2] if pos + 2 < len(T) else None
            p3 = T[pos + 3] if pos + 3 < len(T) else None
            end_col = end_row = False
            if col == 4 and ((p2 in MONTHS) or p1 == "New Statement"):
                end_col = end_row = True
            if col == 1 and sum(ch.isdigit() for ch in s) == 4:
                end_col = True
            if col == 2 and p1 and p1.find("$") == 0 and p2 and p2.find("$") in (0, 1) \
                    and (p3 is None or "$" not in p3):
                end_col = True
            if col == 3 and p1 and p1.find("$") in (0, 1):
                end_col = True
            if pos == len(T) - 1:
                end_col = end_row = True
            cur.append(s)
            pos += 1
            if end_row and col == 4 and p1 != "New Statement" and pos < len(T):
                pos += 3                           # skip the next row's Posted date
            if end_col:
                cols.append(" ".join(cur))
                cur = []
                col += 1
                if end_row:
                    break
        if len(cols) >= 4:
            rows.append((cols[0], cols[1], cols[2].replace("$", ""), cols[3].replace("$", ""), stmt))
        else:
            rows.append(("?", " ".join(cols), None, None, stmt))
    return rows


def qvf_selfcheck(pdf, specs, printed_by_stmt):
    got = qvf_walk(mole_words(pdf))
    want = []
    for j, (sp, pr) in enumerate(zip(specs, printed_by_stmt)):
        for r in pr:
            want.append((fmt_date(r["date"]), r["desc"], mag(r["amt"]), fmt_bal(r["bal"]).replace("$", ""),
                         j + 1))
    bad = []
    if len(got) != len(want):
        bad.append("row count %d, want %d" % (len(got), len(want)))
    for g, w in zip(got, want):
        if g[0] != w[0] or g[1] != w[1] or g[2] != w[2] or g[3] != w[3] or g[4] != w[4]:
            bad.append("got %r want %r" % (g, w))
    return bad


# ---------------------------------------------------------------------------
# The statements
# ---------------------------------------------------------------------------

D = dt.date
ACCT_A, ACCT_B, ACCT_C = "38-9012-0345678-00", "38-9012-0345678-01", "38-9012-0345678-03"

CASES = [
    dict(case="kbpdf_1", note="One enquiry, one page, everyday account, newest first.",
         stmts=[dict(holder=HOLDERS[0], account=ACCT_A, product="Everyday account",
                     start=D(2024, 3, 1), end=D(2024, 3, 31), n=18, own=[ACCT_B])]),
    dict(case="kbpdf_2", note="Three pages; continuation pages go straight from the "
         "www.kiwibank.co.nz header line into the rows (no column heading); the enquiry "
         "period crosses a new year.",
         stmts=[dict(holder=HOLDERS[1], account="38-9104-0712345-00", product="Everyday account",
                     start=D(2023, 11, 1), end=D(2024, 1, 31), n=72, cap1=26, capc=30,
                     head_every_page=False, interest=True)]),
    dict(case="kbpdf_3", note="Overdrawn: the balance goes below zero and prints -$; two "
         "pages with the column heading repeated on page 2.",
         stmts=[dict(holder=HOLDERS[2], account="38-9150-0098765-00", product="Cheque account",
                     start=D(2024, 5, 1), end=D(2024, 6, 15), n=44, cap1=24,
                     head_every_page=True, od=True, credit_share=0.15)]),
    dict(case="kbpdf_4", note="Long details wrap onto a second line, with the money on the "
         "last line of the row; interest and withholding tax lines; the oldest row is a "
         "$0.00 waived fee (the QVF keeps a zero first amount).",
         stmts=[dict(holder=HOLDERS[0], account=ACCT_B, product="Online savings",
                     start=D(2024, 7, 1), end=D(2024, 8, 31), n=26, wrap=True,
                     interest=True, zero_oldest=True, own=[ACCT_A])]),
    dict(case="kbpdf_5", note="Two enquiries of the same account in one report, consecutive "
         "periods (the balance chains from one into the next); page numbers run on through "
         "the file (one report), so no page says 'Page 1 of' twice.",
         chain=True,
         stmts=[dict(holder=HOLDERS[0], account=ACCT_A, product="Everyday account",
                     start=D(2024, 1, 1), end=D(2024, 1, 31), n=22, own=[ACCT_B]),
                dict(holder=HOLDERS[0], account=ACCT_A, product="Everyday account",
                     start=D(2024, 2, 1), end=D(2024, 2, 29), n=20, own=[ACCT_B])]),
    dict(case="kbpdf_6", note="One report holding three enquiries for three accounts of one "
         "customer, same enquiry period, page numbers running on; the middle one has no "
         "transactions; transfers between the customer's own accounts (the QVF codes those "
         "as inter-account transfers).",
         stmts=[dict(holder=HOLDERS[0], account=ACCT_A, product="Everyday account",
                     start=D(2024, 4, 1), end=D(2024, 4, 30), n=16, own=[ACCT_C]),
                dict(holder=HOLDERS[0], account=ACCT_B, product="Online savings",
                     start=D(2024, 4, 1), end=D(2024, 4, 30), n=0, opening=1250000),
                dict(holder=HOLDERS[0], account=ACCT_C, product="Bills account",
                     start=D(2024, 4, 1), end=D(2024, 4, 30), n=12, own=[ACCT_A],
                     credit_share=0.35)]),
    dict(case="kbpdf_7", note="Four pages, heading repeated on every page, many card "
         "rows posted 1-3 days after the transaction date (posted and transaction dates "
         "differ, and a few transaction dates fall before the enquiry period).",
         stmts=[dict(holder=HOLDERS[1], account="38-9104-0712345-00", product="Everyday account",
                     start=D(2024, 9, 1), end=D(2024, 11, 30), n=110, head_every_page=True,
                     cap1=30, capc=36)]),
    dict(case="kbpdf_8", note="Three separately issued enquiry reports for one account, "
         "concatenated (each restarts 'Page 1 of N'), consecutive months across a new year; "
         "the balance chains from one into the next.", chain=True, page_reset=True,
         stmts=[dict(holder=HOLDERS[2], account="38-9150-0098765-00", product="Cheque account",
                     start=D(2023, 12, 1), end=D(2023, 12, 31), n=24, cap1=20,
                     head_every_page=True),
                dict(holder=HOLDERS[2], account="38-9150-0098765-00", product="Cheque account",
                     start=D(2024, 1, 1), end=D(2024, 1, 31), n=18, head_every_page=True),
                dict(holder=HOLDERS[2], account="38-9150-0098765-00", product="Cheque account",
                     start=D(2024, 2, 1), end=D(2024, 2, 29), n=21, head_every_page=True,
                     interest=True)]),
]


def build(case, out_dir):
    name = case["case"]
    rng = rng_for(name)
    specs = case["stmts"]
    sh = Sheet(os.path.join(out_dir, name + ".pdf"), (595.27, 841.89), FONT)
    printed_all, sts = [], []
    prev_close = None
    for j, sp in enumerate(specs):
        sp = dict(sp, name="%s/%d" % (name, j))
        rows = gen_rows(rng, sp)
        if case.get("chain") and prev_close is not None:
            sp["opening"] = prev_close
        opening, closing = set_balances(rng, rows, sp)
        if not rows:
            closing = opening
        prev_close = closing
        for r in rows:
            r["lines"] = wrap_desc(sh, r["desc"], DESC_W)
        printed = rows[::-1]                         # NEWEST FIRST
        sp["_table_y1"], sp["_table_yc"] = 230.0, (100.0 if sp.get("head_every_page") else 84.0)
        sp["_pages"] = layout_pages(sh, sp, printed) if printed else [[]]
        sts.append((sp, printed, opening, closing))
        printed_all.append(printed)
    total = sum(len(s[0]["_pages"]) for s in sts)
    pno = 1
    for sp, printed, _, _ in sts:
        if case.get("page_reset"):
            # separately issued enquiry reports, concatenated: each restarts "Page 1 of N"
            draw_statement(sh, sp, printed, 1, len(sp["_pages"]))
        else:
            pno += draw_statement(sh, sp, printed, pno, total)
    sh.save()

    bundle = len(sts) > 1
    rows_t, statements = [], []
    for j, (sp, printed, opening, closing) in enumerate(sts):
        part = []
        for r in printed:
            t = {"date": r["date"].isoformat(), "description": r["desc"],
                 "debit": r["amt"] / 100.0 if r["dir"] == "D" else None,
                 "credit": r["amt"] / 100.0 if r["dir"] == "C" else None,
                 "balance": r["bal"] / 100.0}
            if bundle:
                t["statement_index"] = j
            part.append(t)
        chain_check(sp["name"], part, opening / 100.0, closing / 100.0, newest=True)
        rows_t += part
        statements.append({"statement_index": j, "period_start": sp["start"].isoformat(),
                           "period_end": sp["end"].isoformat(), "opening_balance": opening / 100.0,
                           "closing_balance": closing / 100.0})
    sp0 = sts[0][0]
    feats = ["bank:kiwibank", "qvf:kiwibank_pdf", "cols:pdate|date|desc|debit|credit|balance",
             "date:dd Mon yyyy", "newest_first", "money:dollar", "money:thousands",
             "bal:lead_minus_dollar", "two_date_columns", "no_opening_closing_printed",
             "pages:%d" % total]
    feats.append("heading_every_page" if sp0.get("head_every_page") else "heading_page1_only")
    if any(r["bal"] < 0 for _, pr, _, _ in sts for r in pr):
        feats.append("negative_balance")
    if any(len(r["lines"]) > 1 for _, pr, _, _ in sts for r in pr):
        feats += ["multiline_desc", "staggered_amounts"]
    if any(r["amt"] == 0 for _, pr, _, _ in sts for r in pr):
        feats.append("zero_amounts")
    if any(sp["start"].year != sp["end"].year for sp, _, _, _ in sts):
        feats.append("period_crosses_year")
    if any(r["date"] != r["posted"] for _, pr, _, _ in sts for r in pr):
        feats.append("posted_differs_from_date")
    if any(r["date"] < sp["start"] for sp, pr, _, _ in sts for r in pr):
        feats.append("date_before_period")
    if bundle:
        feats += ["bundle", "bundle:%d" % len(sts),
                  "page_numbers_restart" if case.get("page_reset") else "page_numbers_run_on"]
        if len({sp["account"] for sp, _, _, _ in sts}) > 1:
            feats.append("bundle_several_accounts")
        if any(not pr for _, pr, _, _ in sts):
            feats.append("bundle_empty_statement")
    truth = truth_doc(
        case=name, generator=GEN,
        note="QVF lookalike (Kiwibank - PDF, transaction enquiry). " + case["note"],
        bank="Kiwibank", layout=TYPE_KEY, product=sp0["product"], account_bank_code="38",
        account_number=sp0["account"], features=feats, row_order="newest_first",
        opening=sts[0][2] / 100.0, closing=sts[-1][3] / 100.0, rows=rows_t,
        statements=statements if bundle else None)
    write_json(os.path.join(out_dir, name + ".truth.json"), truth)
    bad = qvf_selfcheck(os.path.join(out_dir, name + ".pdf"), [s[0] for s in sts], printed_all)
    return truth, total, bad


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=OUT_DIR)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    index = []
    for case in CASES:
        truth, pages, bad = build(case, a.out)
        print("%-10s %3d rows %d pages  QVF walk: %s" % (truth["case"], truth["row_count"], pages,
                                                       "ok" if not bad else "; ".join(bad[:3])))
        index.append({"case": truth["case"], "file": truth["case"] + ".pdf", "bank": "Kiwibank",
                      "layout": TYPE_KEY, "rows": truth["row_count"], "pages": pages,
                      "features": truth["features"], "qvf_walk_ok": not bad})
    write_index(a.out, index, "index_%s.json" % TYPE_KEY)
    return 0


if __name__ == "__main__":
    sys.exit(main())
