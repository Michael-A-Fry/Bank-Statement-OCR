#!/usr/bin/env python3
"""make_layouts.py -- a corpus of REALISTIC statement LAYOUTS, each PDF paired with
the ground truth it was drawn from.

WHAT THIS IS FOR, and how it differs from make_corpus.py.

make_corpus.py is adversarial: ONE layout (the shipped ANZ bands) pushed until it
breaks. It answers "does the reader survive a fault". It cannot answer "where are
the columns on a statement nobody has written a template for", because every case in
it has the same columns in the same place.

This corpus is the opposite: ~30 plausible bank DESIGNS per split -- different
column sets, heading wordings, fonts, sizes, margins, date and money formats, header
boxes, footers -- drawn the way real New Zealand statements are drawn (everyday,
credit-card, business, savings, loan), and NOT tuned against any column-finding
algorithm. It exists to MEASURE a column finder, so it was written without reading
one. The banks, people, addresses, account and card numbers are all fictional.

TWO SPLITS. `dev` is for looking at; `holdout` is for scoring once. They use
different seeds AND different layout parameters -- every shared design draws its
positions, widths, fonts, sizes, heading wordings and date formats from a disjoint
holdout menu, and each split has five designs the other has never seen -- so an
algorithm tuned on dev is tested on combinations it has not met.

THE TRUTH FILE is make_corpus.py's format exactly, plus two keys:
    case, generator, note, opening_balance, closing_balance, row_count,
    rows: [{date "YYYY-MM-DD", description, debit, credit, balance}],
    layout   -- the design id (shared by both splits; the parameters differ)
    features -- short tags: what this statement exercises
debit is money OUT and credit money IN, both positive, from the ACCOUNT HOLDER'S side
-- so on a credit card a purchase is a debit and a payment a credit, whatever sign the
card prints. balance is the running balance printed ON THAT ROW, else null (a layout
that prints the balance only on the last transaction of a day has nulls on the
others; a card has no balance column, so all null). A card's opening/closing balance
is the amount owed, NEGATED (the holder's side), so the arithmetic below holds for
every statement. Rows are in printed order; opening, brought/carried-forward, page
total, sub-total and closing lines are NOT rows.

    description = every text cell of the transaction that is not a date or a figure,
                  in reading order (line by line, left to right), joined by ONE space.
                  A two-line description is "line one" + " " + "line two"; a
                  Type column and a Particulars/Code/Reference set contribute in
                  column order.
    date        = the FIRST date column (the transaction date). A card's processed
                  date is printed second and is not the truth date.

EVERY STATEMENT IS CHECKED BEFORE ITS TRUTH IS WRITTEN, and the run aborts loudly on
the first failure rather than writing a truth that lies:
  * arithmetic: opening - debits + credits = every printed balance and the closing;
  * every printed date parses back to exactly the truth date, given the period;
  * every printed figure parses back (sign, CR/DR/OD token, parentheses, trailing
    minus, $ and thousands) to the truth amount or balance;
  * every printed description piece joins to the truth description;
  * every cell lies inside its column, no two pieces of text collide, and nothing
    is drawn off the page.

PYTHON 3.9+, reportlab. Byte-identical output across runs (crc32 seeds, not hash();
reportlab invariant mode so the PDF carries no timestamp).

Run:  python3 tools/synth/make_layouts.py --out /tmp/zoo/dev --split dev
      python3 tools/synth/make_layouts.py --out /tmp/zoo/holdout --split holdout
      python3 tools/synth/make_layouts.py --split dev --list
      python3 tools/synth/make_layouts.py --out /tmp/zoo/dev --split dev --only card

Dev-time only. Nothing here ships to the server.
"""

import argparse
import calendar
import datetime as dt
import json
import os
import random
import re
import sys
import zlib

MIN_PYTHON = (3, 9)
if sys.version_info < MIN_PYTHON:
    sys.exit("make_layouts.py needs Python %d.%d or newer (this is %s)"
             % (MIN_PYTHON[0], MIN_PYTHON[1], sys.version.split()[0]))

try:
    from reportlab.pdfgen import canvas
    from reportlab.lib.pagesizes import A4, letter, landscape
    from reportlab.pdfbase.pdfmetrics import stringWidth
except ImportError as e:            # noqa: BLE001 -- the message IS the handling
    sys.exit("make_layouts.py needs reportlab (%s).\n"
             "  python3 -m pip install reportlab\n"
             "Nothing in the app needs it: this is a dev-time tool." % e)

GENERATOR = "tools/synth/make_layouts.py"
SPLIT_SEED = {"dev": 20260117, "holdout": 77031}

PAGES = {"A4": A4, "A4L": landscape(A4), "Letter": letter}
FONTS = {"Helvetica": ("Helvetica", "Helvetica-Bold"),
         "Times": ("Times-Roman", "Times-Bold"),
         "Courier": ("Courier", "Courier-Bold")}
MONEY_KINDS = ("debit", "credit", "amount", "balance")
TEXT_KINDS = ("type", "desc", "payee", "part", "code", "ref")
PAD_T = 2.0          # a text cell starts this far inside its column
PAD_M = 3.0          # a right-aligned figure ends this far inside its column

MON = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
MONTH = ["January", "February", "March", "April", "May", "June", "July",
         "August", "September", "October", "November", "December"]
DOW = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

NAVY = (0.05, 0.22, 0.45)
TEAL = (0.0, 0.40, 0.44)
YELLOW = (1.0, 0.80, 0.10)
GREEN = (0.06, 0.38, 0.24)
MAROON = (0.42, 0.08, 0.12)
BLACK = (0.0, 0.0, 0.0)
WHITE = (1.0, 1.0, 1.0)
GREY = (0.90, 0.90, 0.90)
PALE = (0.95, 0.95, 0.95)


class GenError(Exception):
    """A self-check failed. Never caught quietly: a truth file that disagrees with
    its own PDF is worse than no corpus, because it accuses the reader."""


# ---------------------------------------------------------------------------
# Dates and money, printed the ways NZ statements print them.
# ---------------------------------------------------------------------------

DATE_FMTS = {
    "dd Mon":       lambda d: "%02d %s" % (d.day, MON[d.month - 1]),
    "d Mon":        lambda d: "%d %s" % (d.day, MON[d.month - 1]),
    "dd MON":       lambda d: "%02d %s" % (d.day, MON[d.month - 1].upper()),
    "Dow dd Mon":   lambda d: "%s %02d %s" % (DOW[d.weekday()], d.day, MON[d.month - 1]),
    "Dow d Mon":    lambda d: "%s %d %s" % (DOW[d.weekday()], d.day, MON[d.month - 1]),
    "d Mon yyyy":   lambda d: "%d %s %d" % (d.day, MON[d.month - 1], d.year),
    "dd Mon yyyy":  lambda d: "%02d %s %d" % (d.day, MON[d.month - 1], d.year),
    "dd Mon yy":    lambda d: "%02d %s %02d" % (d.day, MON[d.month - 1], d.year % 100),
    "dd MON yy":    lambda d: "%02d %s %02d" % (d.day, MON[d.month - 1].upper(), d.year % 100),
    "d Month yyyy": lambda d: "%d %s %d" % (d.day, MONTH[d.month - 1], d.year),
    "dd/mm/yyyy":   lambda d: "%02d/%02d/%d" % (d.day, d.month, d.year),
    "d/mm/yyyy":    lambda d: "%d/%02d/%d" % (d.day, d.month, d.year),
    "dd/mm/yy":     lambda d: "%02d/%02d/%02d" % (d.day, d.month, d.year % 100),
    "dd-mm-yy":     lambda d: "%02d-%02d-%02d" % (d.day, d.month, d.year % 100),
    "dd-mm-yyyy":   lambda d: "%02d-%02d-%d" % (d.day, d.month, d.year),
    "dd-Mon-yy":    lambda d: "%02d-%s-%02d" % (d.day, MON[d.month - 1], d.year % 100),
}
# A year-less date is only readable beside a printed statement period, which every
# layout here prints in its page-1 header.
YEARLESS = {"dd Mon", "d Mon", "dd MON", "Dow dd Mon", "Dow d Mon"}


def fmt_date(d, f):
    return DATE_FMTS[f](d)


def period_text(f, start, end):
    """The statement period, always WITH the year."""
    def one(d):
        if f == "long":
            return "%d %s %d" % (d.day, MONTH[d.month - 1], d.year)
        if f == "short":
            return "%d %s %d" % (d.day, MON[d.month - 1], d.year)
        if f == "num":
            return "%02d/%02d/%d" % (d.day, d.month, d.year)
        if f == "upper":
            return "%02d %s %d" % (d.day, MON[d.month - 1].upper(), d.year)
        raise GenError("unknown period format %r" % f)
    return one(start), one(end)


def add_months(d, k):
    m0 = d.month - 1 + k
    y, m = d.year + m0 // 12, m0 % 12 + 1
    return dt.date(y, m, min(d.day, calendar.monthrange(y, m)[1]))


def mag(c, thousands=True, dollar=False):
    """|c| cents as printed: 1,234.56. Integer arithmetic, never float formatting."""
    d, r = divmod(abs(int(c)), 100)
    s = ("{:,}".format(d) if thousands else str(d)) + ".%02d" % r
    return "$" + s if dollar else s


def fmt_money(v, style, L):
    """A signed figure (holder's side: negative = money out, or overdrawn) as the
    page prints it. Returns (number, token); token is "" or CR / DR / OD.

    THE SIGN MUST SURVIVE THIS FUNCTION. make_corpus.py's money() once printed
    abs(x) and blamed the reader for 221 wrong balances; every style here is
    parsed back by parse_money() before a truth file is written."""
    m = mag(v, L["thousands"], L["dollar"])
    neg = v < 0
    if style == "plain":
        if v < 0:
            raise GenError("plain style asked to print a negative figure")
        return m, ""
    if style == "lead":
        return ("-" + m if neg else m), ""
    if style == "plus":
        return (("-" if neg else "+") + m), ""
    if style == "trail":
        return (m + "-" if neg else m), ""
    if style == "paren":
        return ("(" + m + ")" if neg else m), ""
    if style == "drcr":
        return m, ("DR" if neg else "CR")
    if style == "od":
        return m, ("OD" if neg else "")
    if style == "dr_only":
        return m, ("DR" if neg else "")
    # Credit-card conventions: printed from the CARD's side, so a purchase (money
    # out for the holder) is a plain figure and a payment carries the mark.
    if style == "card_cr":
        return m, ("" if neg else "CR")
    if style == "card_minus":
        return (m if neg else "-" + m), ""
    raise GenError("unknown money style %r" % style)


def parse_money(num, tok, card=False):
    """Read a printed figure back, independently of how fmt_money built it."""
    s = num
    neg = False
    if s.startswith("(") and s.endswith(")"):
        neg, s = True, s[1:-1]
    if s.startswith("-"):
        neg, s = True, s[1:]
    elif s.startswith("+"):
        s = s[1:]
    if s.endswith("-"):
        neg, s = True, s[:-1]
    if s.startswith("$"):
        s = s[1:]
    if not re.fullmatch(r"\d{1,3}(,\d{3})*\.\d\d|\d+\.\d\d", s):
        raise GenError("printed figure %r does not parse" % num)
    c = int(s.replace(",", "").replace(".", ""))
    if tok not in ("", "CR", "DR", "OD"):
        raise GenError("unknown token %r" % tok)
    if card:
        return c if (neg or tok == "CR") else -c
    if tok in ("DR", "OD"):
        neg = True
    return -c if neg else c


# ---------------------------------------------------------------------------
# Drawing, in TOP-DOWN coordinates, with the collision check built in.
# ---------------------------------------------------------------------------

class Sheet:
    """One PDF being drawn. Every string drawn is recorded, and at the end of each
    page the strings are checked for collisions and for leaving the page: a corpus
    whose text overlaps measures the overlap, not the reader."""

    def __init__(self, path, pagesize, family):
        self.c = canvas.Canvas(path, pagesize=pagesize, invariant=1)
        self.w, self.h = pagesize
        self.reg, self.bold = FONTS[family]
        self.dx = 0.0
        self.items = []
        self.pageno = 0

    def width(self, s, size, bold=False):
        return stringWidth(s, self.bold if bold else self.reg, size)

    def begin_page(self, dx=0.0):
        self.pageno += 1
        self.items = []
        self.dx = dx
        if dx:
            self.c.translate(dx, 0)

    def text(self, x, y, s, size, bold=False, align="left", color=None):
        if s == "":
            return (x, x)
        fn = self.bold if bold else self.reg
        w = stringWidth(s, fn, size)
        x0 = x - w if align == "right" else (x - w / 2.0 if align == "center" else x)
        self.c.setFont(fn, size)
        if color is not None:
            self.c.setFillColorRGB(*color)
        self.c.drawString(x0, self.h - y, s)
        if color is not None:
            self.c.setFillColorRGB(0, 0, 0)
        self.items.append((y, x0 + self.dx, x0 + w + self.dx, size, s))
        return (x0, x0 + w)

    def rect(self, x, y_top, w, h, fill=None, stroke=None, width=0.6):
        if fill is not None:
            self.c.setFillColorRGB(*fill)
        if stroke is not None:
            self.c.setStrokeColorRGB(*stroke)
            self.c.setLineWidth(width)
        self.c.rect(x, self.h - y_top - h, w, h,
                    fill=1 if fill is not None else 0,
                    stroke=1 if stroke is not None else 0)
        self.c.setFillColorRGB(0, 0, 0)
        self.c.setStrokeColorRGB(0, 0, 0)

    def line(self, x0, y0, x1, y1, width=0.6, color=None):
        self.c.setLineWidth(width)
        if color is not None:
            self.c.setStrokeColorRGB(*color)
        self.c.line(x0, self.h - y0, x1, self.h - y1)
        self.c.setStrokeColorRGB(0, 0, 0)

    def end_page(self):
        self.check()
        self.c.showPage()

    def check(self):
        its = sorted(self.items)
        for a in its:
            if a[1] < 4 or a[2] > self.w - 4 or a[0] < 8 or a[0] > self.h - 4:
                raise GenError("page %d: %r drawn off the page (x %.1f-%.1f, y %.1f)"
                               % (self.pageno, a[4], a[1], a[2], a[0]))
        # Same baseline: at least 2.5pt apart, or a text extractor runs the two
        # strings together into one word. Different baselines: the glyph boxes
        # (cap height 0.75em above the baseline, descenders 0.22em below) must not
        # overlap where the strings overlap horizontally.
        for i, a in enumerate(its):
            for b in its[i + 1:]:
                dy = b[0] - a[0]
                if dy > 30:
                    break
                if dy < 0.01:
                    hit = b[1] < a[2] + 2.5 and a[1] < b[2] + 2.5
                else:
                    vert = (b[0] - 0.75 * b[3]) < (a[0] + 0.22 * a[3])
                    hit = vert and b[1] < a[2] - 0.5 and a[1] < b[2] - 0.5
                if hit:
                    raise GenError("page %d: %r and %r collide (y %.1f / %.1f)"
                                   % (self.pageno, a[4], b[4], a[0], b[0]))

    def save(self):
        self.c.save()


class Col:
    """One table column: a band [x0, x1] and how its cells sit in it."""

    def __init__(self, kind, x0, x1, align, head, head_align):
        self.kind, self.x0, self.x1 = kind, float(x0), float(x1)
        self.align, self.head, self.head_align = align, tuple(head), head_align

    def anchor(self, align=None):
        a = align or self.align
        if a == "right":
            return self.x1 - PAD_M
        if a == "center":
            return (self.x0 + self.x1) / 2.0
        return self.x0 + PAD_T

    def __repr__(self):
        return "%s[%.0f-%.0f]" % (self.kind, self.x0, self.x1)


def mkcols(left, right, spec):
    """Columns laid side by side from `left` to `right`. spec entries are
    (kind, width or None for the one flexible column, heading lines[, align[, head_align]])."""
    fixed = sum(e[1] for e in spec if e[1])
    flex = right - left - fixed
    if flex < 60:
        raise GenError("no room left for the flexible column (%.0f)" % flex)
    out, x = [], float(left)
    for e in spec:
        kind, w, head = e[0], (e[1] or flex), e[2]
        align = e[3] if len(e) > 3 and e[3] else ("right" if kind in MONEY_KINDS else "left")
        hal = e[4] if len(e) > 4 and e[4] else align
        out.append(Col(kind, x, x + w, align, head, hal))
        x += w
    return out


def fit(sh_w, s, maxw):
    """Shorten s (whole words first) until it fits in maxw points."""
    s = s.strip()
    while s and sh_w(s) > maxw:
        if " " in s:
            s = s.rsplit(" ", 1)[0].rstrip(" -,@/")
        else:
            s = s[:-1]
    return s


# ---------------------------------------------------------------------------
# What the transactions SAY. Invented merchants, people and numbers; the shapes
# (card numbers, times, foreign amounts, invoice numbers that look like money,
# dates inside descriptions) are the real ones.
# ---------------------------------------------------------------------------

NAMES = {
    "grocer": ["HARBOUR FOODMARKET", "VALLEY FRESH GROCER", "KAURI SUPERSTORE",
               "MAIN ST MARKET", "RIVERSIDE DAIRY", "CORNER FOODS 24/7"],
    "fuel": ["TOTARA FUELS", "SOUTHGATE FUEL STOP 114", "KEA GAS AND SERVICE"],
    "cafe": ["MAIN ST CAFE", "BEAN THERE ESPRESSO", "THE DAILY GRIND"],
    "online": ["STREAMFLIX.COM", "APPSTORE ONLINE", "CLOUDBOX STORAGE", "WEBSHOP INTL"],
    "city": ["AUCKLAND", "WELLINGTON", "CHRISTCHURCH", "HAMILTON", "DUNEDIN",
             "NELSON", "TAURANGA", "NAPIER"],
    "utility": ["KAURI ENERGY", "TASMAN POWER", "CITYGAS LTD", "FIBRENET BROADBAND"],
    "council": ["CITY COUNCIL RATES", "DISTRICT COUNCIL WATER", "REGIONAL TRANSPORT"],
    "landlord": ["EXAMPLE RENTALS LTD", "SAMPLE PROPERTY TRUST", "B SAMPLE"],
    "employer": ["EXAMPLE HOLDINGS LTD", "SAMPLE ENGINEERING", "TEST LOGISTICS NZ",
                 "DEMO HEALTH SERVICES"],
    "insurer": ["SOUTHERN INSURANCE", "HARBOUR LIFE COVER"],
    "telco": ["HARBOUR TELECOM", "TUI MOBILE"],
    "atm": ["QUEEN ST", "LAMBTON QUAY", "COLOMBO ST", "VICTORIA ST", "THE MALL"],
    "person": ["A EXAMPLE", "B SAMPLE", "C TESTER", "D DEMO"],
    "supplier": ["SAMPLE SUPPLIES LTD", "EXAMPLE TIMBER CO", "DEMO FREIGHT LTD",
                 "TEST PACKAGING"],
    "customer": ["EXAMPLE CAFE LTD", "SAMPLE MOTORS", "DEMO PLUMBING", "TEST & CO LTD"],
    "airline": ["SKYWAYS AIR", "TASMAN AIRLINES"],
    "branch": ["SAMPLETOWN", "EXAMPLE CENTRAL", "HILLSIDE"],
}


def T(d, w, amt, *lines, cat=""):
    """A catalogue entry printed as one or two description lines."""
    return {"dir": d, "w": w, "amt": amt, "lines": lines, "cat": cat}


def TY(d, w, amt, typ, det, cat=""):
    """An entry for a layout with a separate Type column."""
    return {"dir": d, "w": w, "amt": amt, "type": typ, "lines": (det,), "cat": cat}


def P(d, w, amt, part, code, ref, payee="", cat=""):
    """An entry for an NZ Particulars / Code / Reference layout."""
    return {"dir": d, "w": w, "amt": amt, "part": part, "code": code, "ref": ref,
            "payee": payee, "lines": ("",), "cat": cat}


BANK = [
    T("D", 6, ("r", 12, 310), "EFTPOS {grocer}"),
    T("D", 3, ("r", 35, 160), "EFTPOS {fuel} {card} {time}"),
    T("D", 3, ("r", 4, 38), "POS W/D {cafe}-{time}"),
    T("D", 1, ("r", 9, 14), "2 x COFFEE {cafe}"),
    T("D", 2, ("fx",), "VISA DEBIT {online} USD {fx}"),
    T("D", 2, ("r", 380, 760), "AP {landlord} RENT REF {ref}"),
    T("D", 2, ("r", 60, 340), "DD {utility} {custno}"),
    T("D", 2, ("r", 90, 1650), "BILL PAYMENT {council} INV {inv}"),
    T("D", 2, ("mult", 20, 400, 20), "ATM WITHDRAWAL {atm} {time}"),
    T("D", 1, ("fixed", 5.00), "MONTHLY ACCOUNT FEE", cat="fee"),
    T("D", 1, ("r", 0.25, 4.5), "OVERSEAS TXN FEE", cat="fee"),
    T("D", 2, ("r", 50, 2500), "TFR TO {acct}"),
    T("D", 1, ("r", 28, 260), "DD {insurer} POLICY {invno}"),
    T("D", 1, ("r", 3, 24), "PARKING {ddmm} {time} {city}"),
    T("D", 1, ("r", 150, 900), "LOAN REPAYMENT {n} OF 52"),
    T("D", 2, ("r", 35, 190), "ONLINE PAYMENT {telco} REF {ref}"),
    T("D", 4, ("r", 12, 310), "EFTPOS PURCHASE", "{grocer} {card}"),
    T("D", 2, ("r", 380, 1600), "AUTOMATIC PAYMENT", "{landlord} RENT {n} WKS"),
    T("D", 2, ("fx",), "VISA PURCHASE", "{online} USD {fx}"),
    T("D", 2, ("r", 60, 340), "DIRECT DEBIT", "{utility} {custno}"),
    T("D", 2, ("r", 90, 1650), "BILL PAYMENT", "{council} INV {inv}"),
    T("D", 1, ("mult", 20, 400, 20), "ATM WITHDRAWAL", "{atm} {ddmm} {time}"),
    T("C", 3, ("r", 1800, 6200), "SALARY {employer}", cat="salary"),
    T("C", 2, ("r", 850, 2400), "DIRECT CREDIT {employer} WAGES WK {wk}", cat="wages"),
    T("C", 1, ("r", 0.15, 24), "CREDIT INTEREST", cat="interest"),
    T("C", 2, ("r", 50, 1500), "TFR FROM {acct}"),
    T("C", 1, ("r", 20, 900), "DEPOSIT {branch} BRANCH"),
    T("C", 1, ("r", 5, 180), "REFUND {grocer} INV {inv}"),
    T("C", 1, ("r", 100, 2400), "IRD REFUND {person}", cat="ird"),
    T("C", 3, ("r", 1800, 6200), "DIRECT CREDIT", "{employer} SALARY", cat="salary"),
    T("C", 2, ("r", 50, 2500), "TRANSFER", "FROM {acct}"),
    T("C", 1, ("r", 20, 1800), "DEPOSIT", "MOBILE CHEQUE {chq}"),
]

CARD = [
    T("D", 5, ("r", 12, 320), "{grocer} {city}"),
    T("D", 3, ("r", 35, 160), "{fuel} {city}"),
    T("D", 3, ("r", 4, 40), "{cafe} {city}"),
    T("D", 2, ("r", 6, 120), "{online} {city}"),
    T("D", 1, ("r", 180, 1450), "{airline} {city} TKT {invno}"),
    T("D", 1, ("r", 0.2, 6), "OFFSHORE SERVICE MARGIN", cat="fee"),
    T("D", 3, ("fx",), "{online} SAN FRANCISCO", "USD {fx} @ {rate}"),
    T("D", 2, ("fx",), "{online} SYDNEY", "AUD {fx} @ {arate}"),
    T("D", 1, ("r", 40, 400), "{airline}", "{city} TKT {invno} 2 x ADULT"),
    T("C", 3, ("pay",), "PAYMENT RECEIVED - THANK YOU", cat="payment"),
    T("C", 1, ("pay",), "AUTOMATIC PAYMENT", "THANK YOU", cat="payment"),
    T("C", 1, ("r", 5, 120), "REFUND {online} {city}"),
    T("C", 1, ("r", 5, 120), "REFUND", "{grocer} {city}"),
]

BIZ = [
    T("D", 3, ("r", 120, 9800), "AP {supplier} INV {invno}"),
    T("D", 1, ("r", 800, 14000), "IRD GST PERIOD {ddmm}"),
    T("D", 1, ("r", 2500, 18000), "WAGES BATCH {chq}"),
    T("D", 1, ("r", 12, 85), "BANK FEES", cat="fee"),
    T("D", 2, ("r", 40, 180), "EFTPOS {fuel} {card}"),
    T("D", 1, ("r", 600, 4800), "DD {insurer} POLICY {invno}"),
    T("D", 2, ("r", 120, 9800), "AUTOMATIC PAYMENT", "{supplier} INV {inv}"),
    T("D", 1, ("r", 800, 14000), "DIRECT DEBIT", "IRD PAYE {ddmm}"),
    T("C", 4, ("r", 90, 12500), "BILL PAYMENT {customer} INV {invno}"),
    T("C", 2, ("r", 300, 8800), "MERCHANT SETTLEMENT {chq}"),
    T("C", 1, ("r", 100, 3000), "CASH DEPOSIT {branch}"),
    T("C", 2, ("r", 90, 12500), "DIRECT CREDIT", "{customer} INV {inv}"),
]

SAV = [
    T("C", 2, ("r", 0.5, 40), "CREDIT INTEREST", cat="interest"),
    T("C", 2, ("r", 100, 3000), "TFR FROM {acct}"),
    T("D", 1, ("r", 0.1, 9), "RWT ON INTEREST", cat="rwt"),
    T("D", 3, ("r", 100, 1400), "TFR TO {acct}"),
    T("D", 1, ("r", 20, 800), "WITHDRAWAL {branch} BRANCH"),
    T("D", 2, ("r", 100, 1400), "TRANSFER", "TO {acct} {person}"),
    T("C", 1, ("r", 100, 3000), "TRANSFER", "FROM {acct} {person}"),
]

LOAN = [
    T("C", 5, ("r", 1100, 2600), "REPAYMENT THANK YOU"),
    T("C", 1, ("r", 2000, 15000), "LUMP SUM PAYMENT"),
    T("D", 3, ("r", 700, 2200), "INTEREST"),
    T("D", 1, ("fixed", 10.00), "LOAN SERVICE FEE", cat="fee"),
    T("C", 2, ("r", 1100, 2600), "AUTOMATIC PAYMENT", "FROM {acct}"),
    T("D", 1, ("r", 700, 2200), "INTEREST CHARGED", "RATE 6.24% P.A."),
]

TYPED = [
    TY("D", 6, ("r", 12, 310), "EFTPOS", "{grocer} {card}"),
    TY("D", 3, ("r", 35, 160), "EFTPOS", "{fuel} {time}"),
    TY("D", 2, ("r", 4, 38), "POS", "{cafe} {time}"),
    TY("D", 2, ("fx",), "VISA", "{online} USD {fx}"),
    TY("D", 2, ("r", 380, 760), "AP", "{landlord} RENT REF {ref}"),
    TY("D", 2, ("r", 60, 340), "DD", "{utility} {custno}"),
    TY("D", 2, ("r", 90, 1650), "BP", "{council} INV {inv}"),
    TY("D", 2, ("mult", 20, 400, 20), "ATM", "{atm} {time}"),
    TY("D", 1, ("fixed", 5.00), "FEE", "MONTHLY ACCOUNT FEE", cat="fee"),
    TY("D", 2, ("r", 50, 2500), "TFR", "TO {acct}"),
    TY("C", 3, ("r", 1800, 6200), "DC", "{employer} SALARY", cat="salary"),
    TY("C", 1, ("r", 0.15, 24), "INT", "CREDIT INTEREST", cat="interest"),
    TY("C", 2, ("r", 50, 2500), "TFR", "FROM {acct}"),
    TY("C", 1, ("r", 20, 1800), "DEP", "{branch} BRANCH"),
]

TYPED_BIZ = [
    TY("D", 3, ("r", 120, 9800), "AP", "{supplier} INV {invno}"),
    TY("D", 1, ("r", 800, 14000), "DD", "IRD GST PERIOD {ddmm}"),
    TY("D", 1, ("r", 2500, 18000), "BP", "WAGES BATCH {chq}"),
    TY("D", 1, ("r", 12, 85), "FEE", "ACCOUNT FEES", cat="fee"),
    TY("D", 2, ("r", 40, 180), "EFTPOS", "{fuel} {card}"),
    TY("D", 1, ("r", 600, 4800), "DD", "{insurer} POLICY {invno}"),
    TY("C", 4, ("r", 90, 12500), "BP", "{customer} INV {invno}"),
    TY("C", 2, ("r", 300, 8800), "MS", "MERCHANT SETTLEMENT {chq}"),
    TY("C", 1, ("r", 100, 3000), "DEP", "CASH {branch}"),
    TY("C", 2, ("r", 90, 12500), "DC", "{customer} INV {inv}"),
]

PCR = [
    P("D", 5, ("r", 12, 310), "EFTPOS", "{card4}", "{grocer}"),
    P("D", 2, ("r", 380, 760), "{landlord}", "RENT", "WK {wk}"),
    P("D", 2, ("r", 60, 340), "{utility}", "{custno}", "INV {inv}"),
    P("D", 2, ("mult", 20, 400, 20), "ATM", "{atm}", "{time}"),
    P("D", 1, ("fixed", 5.00), "ACCOUNT FEE", "", "", cat="fee"),
    P("D", 2, ("r", 35, 190), "{telco}", "MOBILE", "REF {ref}"),
    P("D", 2, ("fx",), "VISA {online}", "USD {fx}", "{card4}"),
    P("C", 3, ("r", 1800, 6200), "SALARY", "{employer}", "PAY {ddmm}", cat="salary"),
    P("C", 1, ("r", 0.15, 24), "INTEREST", "", "", cat="interest"),
    P("C", 2, ("r", 50, 2500), "TRANSFER", "FROM SAVINGS", "{ref}"),
    P("C", 1, ("r", 5, 180), "REFUND", "{invno}", "{grocer}"),
]

BIZ_PCR = [
    P("D", 3, ("r", 120, 9800), "INV {invno}", "SUPPLIES", "{ref}", payee="{supplier}"),
    P("D", 1, ("r", 800, 14000), "GST", "{ddmm}", "", payee="INLAND REVENUE"),
    P("D", 1, ("r", 2500, 18000), "WAGES", "BATCH {chq}", "", payee="PAYROLL"),
    P("D", 1, ("r", 12, 85), "FEES", "", "", payee="BANK FEES", cat="fee"),
    P("D", 2, ("r", 40, 180), "EFTPOS", "{card4}", "{time}", payee="{fuel}"),
    P("C", 4, ("r", 90, 12500), "INV {invno}", "{ref}", "THANK YOU", payee="{customer}"),
    P("C", 2, ("r", 300, 8800), "SETTLEMENT", "{chq}", "", payee="MERCHANT SVCS"),
    P("C", 1, ("r", 100, 3000), "CASH", "", "{branch}", payee="DEPOSIT"),
]

# At most this many of a category per statement: four salaries in one month is not
# a statement anybody receives.
CAPS = {"salary": 2, "wages": 4, "payment": 2, "interest": 2, "rwt": 1, "ird": 1}

CATALOGS = {"bank": BANK, "card": CARD, "biz": BIZ, "savings": SAV, "loan": LOAN,
            "typed": TYPED, "typed_biz": TYPED_BIZ, "pcr": PCR, "biz_pcr": BIZ_PCR}

CUSTOMERS = {
    "personal": [
        ("J SAMPLE", ["12 EXAMPLE STREET", "SAMPLEVILLE", "TESTBURY 9010"]),
        ("MS A EXAMPLE", ["FLAT 3", "45 SPECIMEN ROAD", "DEMOTOWN 7020"]),
        ("J SAMPLE & K SAMPLE", ["PO BOX 1234", "EXAMPLE BAY 3050"]),
        ("MR P TESTER", ["7 PLACEHOLDER LANE", "RD 2", "MOCKBURN 9310"]),
        ("R DEMO", ["101 TEMPLATE TERRACE", "ILLUSTRATION HEIGHTS", "TESTBURY 9011"]),
    ],
    "biz": [
        ("SAMPLE TRADING LIMITED", ["UNIT 4, 88 FICTION AVENUE", "SAMPLEVILLE 0610"]),
        ("EXAMPLE PLUMBING LTD", ["PO BOX 4321", "DEMOTOWN 7022"]),
        ("DEMO FARMS PARTNERSHIP", ["1450 MOCK VALLEY ROAD", "RD 5", "TESTBURY 9071"]),
    ],
}


def fill(rng, tmpl, st, vals):
    """Fill a description template. vals collects the foreign amount and rate so
    the NZ$ figure can be derived from them, as a real conversion would be."""
    def sub(m):
        k = m.group(1)
        if k in NAMES:
            return rng.choice(NAMES[k])
        if k == "card":
            return "%04d****%04d" % (rng.randint(4000, 5599), rng.randint(0, 9999))
        if k == "card4":
            return "****%04d" % rng.randint(0, 9999)
        if k == "time":
            return "%02d:%02d" % (rng.randint(6, 22), rng.randint(0, 59))
        if k == "fx":
            vals["fx"] = rng.randint(300, 24000)
            return mag(vals["fx"], thousands=False)
        if k in ("rate", "arate"):
            vals["rate"] = rng.randint(5600, 6300) if k == "rate" else rng.randint(8800, 9300)
            return "0.%04d" % vals["rate"]
        if k == "inv":
            return "%d.%02d" % (rng.randint(1000, 9999), rng.randint(0, 99))
        if k == "invno":
            return str(rng.randint(1000, 99999))
        if k == "ref":
            return str(rng.randint(1, 99))
        if k == "acct":
            return "99-%04d-%07d-%02d" % (rng.randint(1, 9999), rng.randint(0, 9999999),
                                          rng.randint(0, 99))
        if k == "custno":
            return str(rng.randint(10 ** 7, 10 ** 9 - 1))
        if k == "ddmm":
            d = st["start"] + dt.timedelta(days=rng.randint(0, (st["end"] - st["start"]).days))
            return "%02d/%02d" % (d.day, d.month)
        if k == "wk":
            return str(rng.randint(1, 52))
        if k == "n":
            return str(rng.randint(2, 9))
        if k == "chq":
            return "%06d" % rng.randint(1, 999999)
        raise GenError("unknown placeholder {%s}" % k)
    return re.sub(r"\{(\w+)\}", sub, tmpl)


def amount_for(rng, spec, vals, scale):
    kind = spec[0]
    if kind == "fx":
        rate = vals.get("rate") or rng.randint(5600, 6300)
        return max(1, int(round(vals["fx"] * 10000.0 / rate)))
    if kind == "fixed":
        return int(round(spec[1] * 100))
    if kind == "mult":
        lo, hi, step = spec[1:]
        return rng.randrange(lo, hi + step, step) * 100
    if kind == "pay":
        return None                      # set once the opening amount owed is known
    lo, hi = spec[1], spec[2]
    return rng.randint(int(lo * 100 * scale), int(hi * 100 * scale))


# What a lines-mode layout prints in an extra text column (a Reference column
# beside the details): some rows carry one, some do not.
REF_T = ["INV {inv}", "{chq}", "REF {ref}", "{invno}", "", "", "WK {wk}"]

EXTEND = ["REF 88412-00", "PARTICULARS RENT", "CODE 0114", "BRANCH 0114",
          "TRACE 556201", "NZ", "AUCKLAND", "WK 12"]


# ---------------------------------------------------------------------------
# The layout record and its defaults.
# ---------------------------------------------------------------------------

DEFAULTS = dict(
    page="A4", font="Helvetica", size=9.0, pitch=None, cpitch=None, head_size=None,
    cols=None, date_fmt="dd Mon", period_fmt="long", period_label="Statement period",
    period_sep=" to ", thousands=True, dollar=False, amount_style="lead",
    balance_style="lead", hang=(), bal_mode="every", head_every_page=True,
    head_style="rule", head_fill=GREY, opening=None, trailer=(),
    closing_label="Closing balance", totals_label="Totals", cf_bf=None,
    page_totals=None, hdr=None, footer=None, page_dx=None, wrap=0.0, wrap_indent=0.0,
    long_desc=0.0, date_once=False, zebra=False, row_rules=False, col_rules=False,
    table_top1=260.0, table_top2=90.0, bottom=72.0, max_rows=None, catalog="bank",
    credits="normal", credit_p=0.17, balance_kind="positive", open_range=(300, 9000),
    sections=None, after=None, stmts=(), note="", customers="personal", scale=1.0,
    card=None, rate_text="", head_gap=None, product="", bank="", acct_prefix="99",
)


class Split:
    """The split-specific choices for one design. v() is a fixed dev/holdout pair;
    ch() and r() draw from DISJOINT dev and holdout menus, so a holdout layout is a
    parameter combination dev never used."""

    def __init__(self, split, arch_id):
        self.split = split
        self.dev = split == "dev"
        self.rng = random.Random(SPLIT_SEED[split] * 1000003
                                 + zlib.crc32(arch_id.encode("utf-8")))

    def v(self, d, h):
        return d if self.dev else h

    def ch(self, d, h):
        return self.rng.choice(d if self.dev else h)

    def r(self, d, h, step=0.5):
        lo, hi = d if self.dev else h
        return round(self.rng.uniform(lo, hi) / step) * step

    def n(self, lo, hi):
        return self.rng.randint(lo, hi)


def layout(S, lid, **kw):
    unknown = set(kw) - set(DEFAULTS)
    if unknown:
        raise GenError("%s: unknown layout keys %s" % (lid, sorted(unknown)))
    L = dict(DEFAULTS)
    L.update(kw)
    L["id"], L["split"] = lid, S.split
    L["size"] = float(L["size"])
    if L["pitch"] is None:
        L["pitch"] = round(L["size"] * 1.7, 2)
    if L["cpitch"] is None:
        L["cpitch"] = round(L["size"] * 1.2, 2)
    if L["head_size"] is None:
        L["head_size"] = L["size"]
    if L["head_gap"] is None:
        L["head_gap"] = L["pitch"] + 6 + (L["size"] + 2 if L["head_style"] == "dashes" else 0)
    L["trailer"] = list(L["trailer"])
    kinds = [c.kind for c in L["cols"]]
    L["kinds"] = kinds
    if kinds[0] != "date":
        raise GenError("%s: the first column must be the date" % lid)
    for a, b in zip(L["cols"], L["cols"][1:]):
        if b.x0 < a.x1 - 0.01:
            raise GenError("%s: columns %r and %r overlap" % (lid, a, b))
    if L["date_fmt"] not in DATE_FMTS:
        raise GenError("%s: unknown date format" % lid)
    if ("totals" in L["trailer"] or "totals_close" in L["trailer"] or L["page_totals"]) \
            and not ("debit" in kinds and "credit" in kinds):
        raise GenError("%s: a totals line needs debit and credit columns" % lid)
    if L["bal_mode"] != "none" and "balance" not in kinds:
        raise GenError("%s: balance mode %s with no balance column" % (lid, L["bal_mode"]))
    if L["cf_bf"] and "balance" not in kinds:
        raise GenError("%s: carried forward needs a balance column" % lid)
    return L


# ---------------------------------------------------------------------------
# Pagination: pure geometry, so a statement can be planned before it is filled.
# ---------------------------------------------------------------------------

def heading_height(L):
    nl = max(len(c.head) for c in L["cols"])
    return (nl - 1) * (L["head_size"] + 1.5) + L["head_gap"]


def after_height(L):
    if not L["after"]:
        return 0.0
    return 10 + (len(L["after"]["rows"]) + 1) * (L["size"] + 4)


def slot_height(L, sl):
    p, cp = L["pitch"], L["cpitch"]
    return sl["pre"] * p + (sl["nlines"] - 1) * cp + p + sl["post"] * p


def paginate(L, slots):
    """Lay the rows out. Returns pages, each a list of (kind, row index, y)."""
    ph = PAGES[L["page"]][1]
    p = L["pitch"]
    bottom = ph - L["bottom"]
    tail = p * ((1 if L["cf_bf"] else 0) + (1 if L["page_totals"] else 0))
    trail = p * len(L["trailer"]) + after_height(L)
    pages, i, n = [], 0, len(slots)
    while i < n:
        first = not pages
        y = L["table_top1"] if first else L["table_top2"]
        items = []
        if first or L["head_every_page"]:
            items.append(("head", None, y))
            y += heading_height(L)
        if not first and L["cf_bf"]:
            items.append(("bf", None, y))
            y += p
        if first and L["opening"]:
            items.append(("open", None, y))
            y += p
        count = 0
        while i < n:
            h = slot_height(L, slots[i])
            extra = trail if i == n - 1 else tail
            end = y + h - p + extra
            if end > bottom or (L["max_rows"] and count >= L["max_rows"]):
                if count == 0:
                    raise GenError("%s: a row does not fit on an empty page" % L["id"])
                break
            items.append(("txn", i, y))
            y += h
            i += 1
            count += 1
        if i < n:
            if L["page_totals"]:
                items.append(("ptot", None, y))
                y += p
            if L["cf_bf"]:
                items.append(("cf", None, y))
                y += p
        else:
            for t in L["trailer"]:
                items.append((t, None, y))
                y += p
            if L["after"]:
                items.append(("after", None, y + 10))
        pages.append(items)
    return pages


def page_rows(pages):
    return [[idx for k, idx, _ in items if k == "txn"] for items in pages]


# ---------------------------------------------------------------------------
# One statement: plan, fill, balance.
# ---------------------------------------------------------------------------

def desc_col(L):
    for c in L["cols"]:
        if c.kind == "desc":
            return c
    return None


def col_of(L, kind):
    for c in L["cols"]:
        if c.kind == kind:
            return c
    return None


def build_statement(L, spec, seed):
    rng = random.Random(seed)
    pw, ph = PAGES[L["page"]]
    measure = lambda s: stringWidth(s, FONTS[L["font"]][0], L["size"])   # noqa: E731

    ym = spec["ym"]
    start = dt.date(ym[0], ym[1], ym[2] if len(ym) > 2 else 1)
    end = add_months(start, spec.get("months", 1)) - dt.timedelta(days=1)
    st = {"start": start, "end": end, "spec": spec}

    cat = CATALOGS[L["catalog"]]
    lines_mode = "type" not in cat[0] and "part" not in cat[0]
    can_wrap = lines_mode and any(len(e["lines"]) == 2 for e in cat)

    # 1. PLAN the shape: how many lines each row takes. Nothing about direction or
    #    amount affects the geometry, so the page breaks are known before the data.
    n_target = spec["n"]
    N = n_target + 40
    nl = [2 if (can_wrap and rng.random() < L["wrap"]) else 1 for _ in range(N)]
    slots = [{"nlines": k, "pre": 0, "post": 0} for k in nl]
    if spec.get("short_last") and not L["sections"]:
        big = page_rows(paginate(L, slots))
        starts = [pr[0] for pr in big[1:]]
        cands = [s + j for s in starts for j in (1, 2) if 8 <= s + j <= 60 and s + j < N]
        if not cands:
            raise GenError("%s: no way to leave a short last page" % L["id"])
        n = min(cands, key=lambda c: (abs(c - n_target), c))
    else:
        n = n_target
    slots = slots[:n]
    section_of = [0] * n
    if L["sections"]:
        k = len(L["sections"])
        cuts = sorted(rng.sample(range(4, n - 3), k - 1))
        bounds = [0] + cuts + [n]
        for s in range(k):
            for i in range(bounds[s], bounds[s + 1]):
                section_of[i] = s
            slots[bounds[s]]["pre"] = 1
            slots[bounds[s + 1] - 1]["post"] = 1
    pages = paginate(L, slots)
    if len(pages) > 4:
        raise GenError("%s: %d pages (the brief says 1-4)" % (L["id"], len(pages)))
    page_of = [0] * n
    for pi, pr in enumerate(page_rows(pages)):
        for i in pr:
            page_of[i] = pi

    # 2. DIRECTION of each row, which is where sparse deposits are arranged.
    plan = spec.get("credits") or L["credits"]
    if plan == "normal" or plan == "loan":
        p_c = L["credit_p"] if plan == "normal" else 0.6
        dirs = ["C" if rng.random() < p_c else "D" for _ in range(n)]
        if "C" not in dirs:
            dirs[rng.randrange(n)] = "C"
    elif plan in ("sparse", "sparse_none_p1", "none_p1", "card"):
        allowed = [i for i in range(n) if plan in ("sparse", "card") or page_of[i] >= 1]
        if not allowed:
            raise GenError("%s: %s needs a second page" % (L["id"], plan))
        if plan == "none_p1":
            dirs = ["C" if (page_of[i] >= 1 and rng.random() < L["credit_p"]) else "D"
                    for i in range(n)]
            if "C" not in dirs:
                dirs[rng.choice(allowed)] = "C"
        else:
            k = rng.choice([1, 2])
            pick = set(rng.sample(allowed, min(k, len(allowed))))
            dirs = ["C" if i in pick else "D" for i in range(n)]
    else:
        raise GenError("unknown credit plan %r" % plan)
    if "D" not in dirs:
        dirs[0] = "D"

    # 3. WHAT each row says, and how much.
    dc = desc_col(L)
    rows = []
    first_credit = True
    used = {}
    for i in range(n):
        d = dirs[i]
        cands = [e for e in cat if e["dir"] == d and len(e["lines"]) == slots[i]["nlines"]]
        cands = [e for e in cands if used.get(e["cat"], 0) < CAPS.get(e["cat"], 10 ** 6)] or cands
        if L["catalog"] == "card" and d == "C" and first_credit:
            cands = [e for e in cands if e["cat"] == "payment"] or cands
        if d == "C":
            first_credit = False
        if not cands:
            raise GenError("%s: catalogue has no %s entry with %d line(s)"
                           % (L["id"], d, slots[i]["nlines"]))
        e = rng.choices(cands, weights=[c["w"] for c in cands])[0]
        used[e["cat"]] = used.get(e["cat"], 0) + 1
        vals = {}
        r = {"dir": d, "cat": e["cat"], "pre": slots[i]["pre"], "post": slots[i]["post"],
             "nlines": slots[i]["nlines"], "page": page_of[i], "section": section_of[i],
             "type": "", "payee": "", "part": "", "code": "", "ref": "", "lines": []}
        if "type" in e:
            r["type"] = e["type"]
            r["lines"] = [fit(measure, fill(rng, e["lines"][0], st, vals),
                              dc.x1 - dc.x0 - PAD_T - 2)]
        elif "part" in e:
            for k in ("payee", "part", "code", "ref"):
                c = col_of(L, k)
                if c is None:
                    continue
                txt = fill(rng, e[k], st, vals)
                if k != "payee":
                    txt = txt[:12].strip()          # the NZ 12-character field
                r[k] = fit(measure, txt, c.x1 - c.x0 - PAD_T - 2)
            if dc is not None:
                raise GenError("%s: a P/C/R layout has no free description column" % L["id"])
        else:
            for j, t in enumerate(e["lines"]):
                indent = L["wrap_indent"] if j else 0.0
                r["lines"].append(fit(measure, fill(rng, t, st, vals),
                                      dc.x1 - dc.x0 - PAD_T - 2 - indent))
            if len(r["lines"]) == 1 and rng.random() < L["long_desc"]:
                # A LONG description that runs up to the first money column: the
                # common real shape where a reference is tacked on the end.
                maxw = dc.x1 - dc.x0 - PAD_T - 2
                s = r["lines"][0]
                for tok in rng.sample(EXTEND, len(EXTEND)):
                    if measure(s + " " + tok) <= maxw:
                        s = s + " " + tok
                r["lines"][0] = s
                r["long"] = maxw - measure(s) < 14
            for k in ("payee", "part", "code", "ref"):
                c = col_of(L, k)
                if c is not None:
                    r[k] = fit(measure, fill(rng, rng.choice(REF_T), st, vals)[:12].strip(),
                               c.x1 - c.x0 - PAD_T - 2)
        if any(not t for t in r["lines"][1:]):
            raise GenError("%s: an empty continuation line" % L["id"])
        r["amt"] = amount_for(rng, e["amt"], vals, L["scale"])
        r["money_like"] = bool(re.search(r"\d+\.\d\d|\d\d:\d\d|\*\*\*\*|\d\d/\d\d|\b\d+ x ",
                                         " ".join(r["lines"] + [r["part"], r["code"], r["ref"]])))
        rows.append(r)

    # 4. DATES: sorted within each section (a card prints by processed date).
    days = (end - start).days + 1
    for s in sorted(set(section_of)):
        idx = [i for i in range(n) if section_of[i] == s]
        ds = sorted(start + dt.timedelta(days=rng.randrange(days)) for _ in idx)
        for i, d in zip(idx, ds):
            if col_of(L, "pdate"):
                rows[i]["pdate"] = d
                rows[i]["date"] = max(start, d - dt.timedelta(days=rng.choice([0, 0, 1, 1, 2, 3])))
            else:
                rows[i]["date"] = d

    # 5. BALANCES. Integer cents throughout.
    kind = L["balance_kind"]
    if kind == "card":
        owed = rng.randint(30000, 480000)
        opening = -owed
        remaining = owed
        for r in rows:
            if r["amt"] is None:
                r["amt"] = max(500, rng.randint(remaining // 4, max(remaining // 2, remaining // 4 + 1)))
                remaining -= r["amt"]
                if remaining < 0:
                    raise GenError("%s: card payments exceed the amount owed" % L["id"])
    for r in rows:
        if r["amt"] is None or r["amt"] <= 0:
            raise GenError("%s: a row with no amount" % L["id"])
    signed = [(-r["amt"] if r["dir"] == "D" else r["amt"]) for r in rows]
    pref, acc = [], 0
    for v in signed:
        acc += v
        pref.append(acc)
    lo_p, hi_p = min(pref), max(pref)
    if kind == "positive":
        floor = rng.randint(4000, 90000)
        base = rng.randint(L["open_range"][0] * 100, L["open_range"][1] * 100)
        opening = max(base, floor - lo_p)
    elif kind == "overdraft":
        q = (hi_p - lo_p) // 4
        lo, hi = -hi_p + q, -lo_p - q
        if lo >= hi:
            raise GenError("%s: cannot make the balance cross zero" % L["id"])
        opening = rng.randint(lo, hi)
    elif kind == "loan":
        opening = -rng.randint(15000000, 65000000)
    elif kind != "card":
        raise GenError("unknown balance kind %r" % kind)
    while opening == 0 or any(opening + x == 0 for x in pref):
        opening += 7
    bal = opening
    for r, v in zip(rows, signed):
        bal += v
        r["bal"] = bal
        r["debit"] = r["amt"] if r["dir"] == "D" else None
        r["credit"] = r["amt"] if r["dir"] == "C" else None
    closing = bal
    if kind == "loan" and (opening >= 0 or any(r["bal"] >= 0 for r in rows)):
        raise GenError("%s: a loan balance went into credit" % L["id"])
    if kind == "overdraft" and not (any(r["bal"] < 0 for r in rows)
                                    and any(r["bal"] > 0 for r in rows)):
        raise GenError("%s: the overdraft statement never crossed zero" % L["id"])

    # A card's limit has to cover what is owed on it, or the summary prints an
    # impossible "available credit 0.00" on a card 1,400 over its limit.
    st["limit"] = None
    if kind == "card":
        limit = L["card"]["limit"]
        peak = max([-opening] + [-r["bal"] for r in rows])
        while peak > 0.8 * limit:
            limit += 100000
        st["limit"] = limit

    # 6. WHICH balances are printed.
    for i, r in enumerate(rows):
        if L["bal_mode"] == "every":
            r["bal_printed"] = True
        elif L["bal_mode"] == "last_of_day":
            r["bal_printed"] = (i == n - 1 or rows[i + 1]["date"] != r["date"])
        else:
            r["bal_printed"] = False

    # 7. The description the truth records: printed pieces in reading order.
    order = [c.kind for c in L["cols"] if c.kind in TEXT_KINDS]
    for r in rows:
        pieces = []
        for k in order:
            if k == "desc":
                pieces.append(r["lines"][0])
            elif r[k]:
                pieces.append(r[k])
        pieces += r["lines"][1:]
        r["desc"] = " ".join(p for p in pieces if p)

    who = rng.choice(CUSTOMERS[L["customers"]])
    st.update(rows=rows, pages=pages, opening=opening, closing=closing,
              cust=who[0], addr=who[1],
              acct="%s-%04d-%07d-%02d" % (L["acct_prefix"], rng.randint(1, 9999),
                                          rng.randint(0, 9999999), rng.randint(0, 99)),
              card_no="4%03d **** **** %04d" % (rng.randint(0, 999), rng.randint(0, 9999)),
              sec_cards=["4%03d **** **** %04d" % (rng.randint(0, 999), rng.randint(0, 9999))
                         for _ in (L["sections"] or [])],
              stno=str(rng.randint(3, 140)),
              branch="%04d %s" % (rng.randint(100, 9999), rng.choice(NAMES["branch"])))
    if L["page_dx"]:
        mode, a = L["page_dx"]
        if mode == "gutter":
            st["dx"] = [0.0 if pi % 2 == 0 else a for pi in range(len(pages))]
        else:
            st["dx"] = [round(rng.uniform(-a, a) * 2) / 2.0 for _ in pages]
    else:
        st["dx"] = [0.0] * len(pages)
    return st


# ---------------------------------------------------------------------------
# Rendering.
# ---------------------------------------------------------------------------

def tok_width(sh, L):
    return max(sh.width(t, L["size"]) for t in ("CR", "DR", "OD"))


def cell(sh, L, c, y, s, bold=False, indent=0.0):
    if c.align == "right":
        x = c.x1 - PAD_M
    elif c.align == "center":
        x = (c.x0 + c.x1) / 2.0
    else:
        x = c.x0 + PAD_T + indent
    a, b = sh.text(x, y, s, L["size"], bold=bold, align=c.align)
    if a < c.x0 - 0.01 or b > c.x1 + 0.01:
        raise GenError("%s: %r (%.1f-%.1f) spills out of column %r"
                       % (L["id"], s, a, b, c))


def money_cell(sh, L, c, y, num, tok, bold=False):
    if c.kind in L["hang"] and c.align == "right":
        # The token sits in its own slot at the right, so the figures stay aligned
        # whether or not a row carries one.
        xr = c.x1 - PAD_M
        if tok:
            sh.text(xr, y, tok, L["size"], bold=bold, align="right")
        a, _ = sh.text(xr - tok_width(sh, L) - 3, y, num, L["size"], bold=bold, align="right")
        if a < c.x0 - 0.01:
            raise GenError("%s: %r spills out of column %r" % (L["id"], num, c))
    else:
        cell(sh, L, c, y, num + (" " + tok if tok else ""), bold=bold)


def holder_style(L, kind):
    if kind in ("debit", "credit"):
        return "plain"
    if kind == "amount":
        return L["amount_style"]
    return L["balance_style"]


def fmt_cell(L, kind, v):
    return fmt_money(v, holder_style(L, kind), L)


def label_region(L):
    texts = [c for c in L["cols"] if c.kind in TEXT_KINDS]
    money = [c for c in L["cols"] if c.kind in MONEY_KINDS]
    return texts[0].x0 + PAD_T, money[0].x0 - 3


def draw_label(sh, L, y, s, bold=False):
    x0, xmax = label_region(L)
    _, b = sh.text(x0, y, s, L["size"], bold=bold)
    if b > xmax:
        raise GenError("%s: label %r runs into the money columns" % (L["id"], s))


def figure_col(L):
    """Where an opening/closing figure goes: the balance column, or on a layout
    without one (a card), the amount column."""
    return col_of(L, "balance") or col_of(L, "amount")


def draw_heading(sh, L, y):
    s = L["head_size"]
    cols = L["cols"]
    nl = max(len(c.head) for c in cols)
    lh = s + 1.5
    x0, x1 = cols[0].x0, cols[-1].x1
    top = y - s - 3
    bot = y + (nl - 1) * lh + 4
    style = L["head_style"]
    ink = None
    if style == "bar":
        sh.rect(x0 - 2, top, x1 - x0 + 4, bot - top, fill=L["head_fill"])
    elif style == "dark":
        sh.rect(x0 - 2, top, x1 - x0 + 4, bot - top, fill=L["head_fill"])
        ink = WHITE
    elif style == "box":
        sh.rect(x0 - 2, top, x1 - x0 + 4, bot - top, stroke=BLACK, width=0.5)
    for c in cols:
        for k, t in enumerate(c.head):
            yy = y + (nl - len(c.head) + k) * lh
            a, b = sh.text(c.anchor(c.head_align), yy, t, s, bold=True,
                           align=c.head_align, color=ink)
            if a < c.x0 - 0.01 or b > c.x1 + 0.01:
                raise GenError("%s: heading %r spills out of %r" % (L["id"], t, c))
    if style in ("rule", "bar"):
        sh.line(x0, bot, x1, bot, width=0.6)
    if style == "rule2":
        sh.line(x0, top - 1, x1, top - 1, width=0.9)
        sh.line(x0, bot, x1, bot, width=0.5)
    if style == "dashes":
        dy = y + (nl - 1) * lh + s + 1
        for c in cols:
            w = c.x1 - c.x0 - PAD_T - PAD_M
            dash = "-" * max(1, int(w / sh.width("-", s)))
            sh.text(c.x0 + PAD_T, dy, dash, s)


def draw_txn(sh, L, st, i, y, show_date):
    r = st["rows"][i]
    p, cp, s = L["pitch"], L["cpitch"], L["size"]
    rec = {"date": None, "pdate": None, "money": {}, "text": []}
    if r["pre"]:
        draw_label(sh, L, y, L["sections"][r["section"]].format(card=st["sec_cards"][r["section"]]),
                   bold=True)
        y += p
    y_last = y + (r["nlines"] - 1) * cp
    if L["zebra"] and r["zebra"]:
        x0, x1 = L["cols"][0].x0, L["cols"][-1].x1
        top = y - 0.35 * s - p / 2.0
        sh.rect(x0 - 2, top, x1 - x0 + 4, (y_last - y) + p, fill=PALE)
    for c in L["cols"]:
        k = c.kind
        if k == "date":
            if show_date:
                rec["date"] = fmt_date(r["date"], L["date_fmt"])
                cell(sh, L, c, y, rec["date"])
        elif k == "pdate":
            rec["pdate"] = fmt_date(r["pdate"], L["date_fmt"])
            cell(sh, L, c, y, rec["pdate"])
        elif k == "desc":
            cell(sh, L, c, y, r["lines"][0])
            rec["text"].append(r["lines"][0])
        elif k in TEXT_KINDS:
            if r[k]:
                cell(sh, L, c, y, r[k])
                rec["text"].append(r[k])
        elif k in ("debit", "credit"):
            if r[k] is not None:
                num, tok = fmt_cell(L, k, r[k])
                money_cell(sh, L, c, y, num, tok)
                rec["money"][k] = (num, tok)
        elif k == "amount":
            v = -r["amt"] if r["dir"] == "D" else r["amt"]
            num, tok = fmt_cell(L, k, v)
            money_cell(sh, L, c, y, num, tok)
            rec["money"][k] = (num, tok)
        elif k == "balance":
            if r["bal_printed"]:
                num, tok = fmt_cell(L, k, r["bal"])
                money_cell(sh, L, c, y, num, tok)
                rec["money"][k] = (num, tok)
    dc = desc_col(L)
    for j, t in enumerate(r["lines"][1:], 1):
        cell(sh, L, dc, y + j * cp, t, indent=L["wrap_indent"])
        rec["text"].append(t)
    if L["row_rules"]:
        x0, x1 = L["cols"][0].x0, L["cols"][-1].x1
        yy = y_last + p / 2.0 - 0.35 * s
        sh.line(x0, yy, x1, yy, width=0.25, color=(0.6, 0.6, 0.6))
    if r["post"]:
        yt = y_last + p
        sec_rows = [x for x in st["rows"] if x["section"] == r["section"]]
        tot = sum((-x["amt"] if x["dir"] == "D" else x["amt"]) for x in sec_rows)
        draw_label(sh, L, yt, "Total for card ending %s" % st["sec_cards"][r["section"]][-4:],
                   bold=True)
        num, tok = fmt_cell(L, "amount", tot)
        money_cell(sh, L, col_of(L, "amount"), yt, num, tok, bold=True)
    r["rec"] = rec
    return y_last


def summary_rows(L, st, keys):
    rows = st["rows"]
    tot_d = sum(r["debit"] or 0 for r in rows)
    tot_c = sum(r["credit"] or 0 for r in rows)
    fees = sum(r["amt"] for r in rows if r["cat"] == "fee")
    card = L["balance_kind"] == "card"

    def bal(v):
        if card:
            num, tok = fmt_money(v, L["amount_style"], L)
        else:
            num, tok = fmt_money(v, L["balance_style"] if "balance" in L["kinds"] else "lead", L)
        return num + (" " + tok if tok else "")

    out = []
    for label, key in keys:
        if key == "open":
            v = bal(st["opening"])
        elif key == "close":
            v = bal(st["closing"])
        elif key == "tot_d":
            v = mag(tot_d, L["thousands"], L["dollar"])
        elif key == "tot_c":
            v = mag(tot_c, L["thousands"], L["dollar"])
        elif key == "fees":
            v = mag(fees, L["thousands"], L["dollar"])
        elif key == "rate":
            v = L["rate_text"]
        elif key == "limit":
            v = mag(st["limit"], L["thousands"], L["dollar"])
        elif key == "avail":
            v = mag(max(0, st["limit"] + st["closing"]), L["thousands"], L["dollar"])
        elif key == "minpay":
            v = mag(max(1000, (-st["closing"] * 3 // 100) // 100 * 100), L["thousands"], L["dollar"])
        elif key == "due":
            v = fmt_date(st["end"] + dt.timedelta(days=25), "d Month yyyy")
        elif key == "count":
            v = str(len(rows))
        elif key == "odlimit":
            v = mag(100000, L["thousands"], L["dollar"])
        elif key is None:
            v = ""
        else:
            raise GenError("unknown summary key %r" % key)
        out.append((label, v))
    return out


def clear_of(xs, anchors, d=10.0):
    return all(abs(x - a) >= d for x in xs for a in anchors)


def nudge(L, xs, lo, hi, what):
    """Shift a header box so none of its figures lines up with a table money
    column: the brief is a summary box that is NOT a fifth column in disguise."""
    anchors = [c.anchor() for c in L["cols"] if c.kind in MONEY_KINDS]
    for t in [0, -6, 6, -12, 12, -18, 18, -24, 24, -30, 30, -36, 36]:
        if clear_of([x + t for x in xs], anchors) and lo + t >= 12 and hi + t <= PAGES[L["page"]][0] - 12:
            return t
    raise GenError("%s: cannot place the %s clear of the table columns" % (L["id"], what))


def draw_box(sh, L, st, B, what):
    """A summary or sidebar box: title, then label ... value rows."""
    sz = B.get("size", L["size"])
    style = B.get("style", "box")
    rows = summary_rows(L, st, B["rows"])
    x, y, w = B["x"], B["y"], B["w"]
    if style == "band":
        cw = w / float(len(rows))
        xs = [x + (k + 1) * cw - 8 for k in range(len(rows))]
        t = nudge(L, xs, x, x + w, what)
        x += t
        for k, (lab, v) in enumerate(rows):
            cx = x + k * cw
            sh.rect(cx, y, cw - 4, 2 * sz + 16, fill=B.get("fill", PALE))
            sh.text(cx + 6, y + sz + 4, lab, sz - 0.5)
            sh.text(cx + cw - 8, y + 2 * sz + 9, v, sz + 1, bold=True, align="right")
        return
    lh = sz + B.get("lead", 5)
    title = B.get("title")
    nrow = len(rows) + (1 if title else 0)
    h = lh * nrow + 7
    t = nudge(L, [x + w - 6], x, x + w, what)
    x += t
    if style == "box":
        sh.rect(x, y, w, h, stroke=B.get("stroke", BLACK), width=0.6)
    elif style == "shaded":
        sh.rect(x, y, w, h, fill=B.get("fill", PALE))
    yy = y + lh + 1
    if title:
        sh.text(x + 6, yy, title, sz + 0.5, bold=True)
        yy += lh
    for lab, v in rows:
        strong = lab.lower().startswith(("closing", "new balance", "amount due"))
        sh.text(x + 6, yy, lab, sz, bold=strong)
        if v:
            sh.text(x + w - 6, yy, v, sz, bold=strong, align="right")
        yy += lh
    for extra in B.get("notes", []):
        sh.text(x + 6, yy, extra, sz - 1)
        yy += sz + 1


def details_rows(L, st, keys):
    p0, p1 = period_text(L["period_fmt"], st["start"], st["end"])
    out = []
    for k in keys:
        if k == "product":
            out.append(("Account", L["product"]))
        elif k == "acct":
            out.append(("Account number", st["acct"]))
        elif k == "period":
            out.append((L["period_label"], p0 + L["period_sep"] + p1))
        elif k == "stno":
            out.append(("Statement number", st["stno"]))
        elif k == "branch":
            out.append(("Branch", st["branch"]))
        elif k == "card":
            out.append(("Card number", st["card_no"]))
        elif k == "issued":
            out.append(("Date issued", fmt_date(st["end"] + dt.timedelta(days=2), "d Month yyyy")))
        elif k == "name":
            out.append(("Account name", st["cust"]))
        else:
            raise GenError("unknown details key %r" % k)
    return out


def draw_first_header(sh, L, st):
    H = L["hdr"]
    pw = sh.w
    left, right = H["left"], H["right"]
    mast = H["mast"]
    color = H.get("color", NAVY)
    if mast == "bar":
        sh.rect(0, 0, pw, H.get("bar_h", 58), fill=color)
        sh.text(left, 38, L["bank"], 18, bold=True, color=H.get("ink", WHITE))
        sh.text(right, 38, H["title"], 11, align="right", color=H.get("ink", WHITE))
    elif mast == "plain":
        sh.text(left, 50, L["bank"], 20, bold=True)
        if H.get("tagline"):
            sh.text(left, 63, H["tagline"], 7.5)
        sh.text(right, 50, H["title"], 12, bold=True, align="right")
    elif mast == "right":
        sh.text(right, 46, L["bank"], 17, bold=True, align="right")
        sh.text(right, 60, H["title"], 9, align="right")
    elif mast == "logo":
        sh.rect(left, 26, 22, 22, fill=color)
        sh.text(left + 30, 44, L["bank"], 16, bold=True)
        sh.text(right, 44, H["title"], 11, bold=True, align="right")
    elif mast == "courier":
        mid = (left + right) / 2.0
        sh.text(mid, 40, L["bank"].upper(), 11, bold=True, align="center")
        sh.text(mid, 54, H["title"].upper(), 9, align="center")
        sh.line(left, 62, right, 62, width=1.0)
    else:
        raise GenError("unknown masthead %r" % mast)
    ax, ay = H["addr"]
    asz = H.get("addr_size", 9)
    for k, s in enumerate([st["cust"]] + st["addr"]):
        sh.text(ax, ay + k * (asz + 2.5), s, asz, bold=(k == 0 and H.get("addr_bold", False)))
    dx_, dy_, lw = H["det"]
    dsz = H.get("det_size", 8.5)
    det = details_rows(L, st, H.get("det_rows", ["acct", "period", "stno"]))
    lw = max(lw, max(sh.width(lab, dsz, H.get("det_bold", False)) for lab, _ in det) + 8)
    for k, (lab, v) in enumerate(det):
        sh.text(dx_, dy_ + k * (dsz + 4), lab, dsz, bold=H.get("det_bold", False))
        sh.text(dx_ + lw, dy_ + k * (dsz + 4), v, dsz)
    if H.get("summary"):
        draw_box(sh, L, st, H["summary"], "summary box")
    if H.get("sidebar"):
        draw_box(sh, L, st, H["sidebar"], "sidebar")
    if H.get("table_title"):
        sh.text(L["cols"][0].x0, L["table_top1"] - L["size"] - 12, H["table_title"],
                L["size"] + 2, bold=True)


def draw_cont_header(sh, L, st, pno, npg):
    H = L["hdr"]
    left, right = H["left"], H["right"]
    p0, p1 = period_text(L["period_fmt"], st["start"], st["end"])
    cont = H.get("cont", "line")
    if cont == "line":
        sh.text(left, 40, "%s  %s" % (L["bank"], L["product"]), 9, bold=True)
        sh.text(left, 52, "Account number %s" % st["acct"], 8)
        sh.text(right, 40, "%s %s%s%s" % (L["period_label"], p0, L["period_sep"], p1), 8,
                align="right")
    elif cont == "bar":
        sh.rect(0, 0, sh.w, 34, fill=H.get("color", NAVY))
        sh.text(left, 22, L["bank"], 12, bold=True, color=H.get("ink", WHITE))
        sh.text(left, 50, "%s %s" % (L["product"], st["acct"]), 8)
        sh.text(right, 50, "%s%s%s" % (p0, L["period_sep"], p1), 8, align="right")
    elif cont == "courier":
        mid = (left + right) / 2.0
        sh.text(mid, 40, L["bank"].upper(), 10, bold=True, align="center")
        sh.text(left, 54, "ACCOUNT %s" % st["acct"], 8)
        sh.text(right, 54, "%s%s%s" % (p0, L["period_sep"], p1), 8, align="right")
    elif cont == "minimal":
        sh.text(left, 44, "%s - continued" % L["product"], 8, bold=True)
    else:
        raise GenError("unknown continuation header %r" % cont)
    if H.get("top_page"):
        sh.text(right, 66 if cont != "minimal" else 44,
                "Page %d of %d" % (pno, npg), 8, align="right")


def draw_footer(sh, L, st, pno, npg):
    F = L["footer"]
    H = L["hdr"]
    left, right = H["left"], H["right"]
    ph = sh.h
    sz = F.get("size", 7)
    y = ph - F.get("y", 26)
    if F.get("rule"):
        sh.line(left, y - sz - 4 - len(F.get("small", [])) * 8, right,
                y - sz - 4 - len(F.get("small", [])) * 8, width=0.4)
    label = F.get("fmt", "Page {p} of {n}").format(p=pno, n=npg)
    pos = F.get("pos", "right")
    if label and not (F.get("first_only") and pno > 1):
        x = {"left": left, "center": (left + right) / 2.0, "right": right}[pos]
        sh.text(x, y, label, sz, align=pos if pos != "center" else "center")
    other = F.get("other")
    if other:
        other = other.format(branch=st["branch"], stno=st["stno"], acct=st["acct"])
        if pos == "left":
            sh.text(right, y, other, sz, align="right")
        else:
            sh.text(left, y, other, sz)
    for k, s in enumerate(F.get("small", [])):
        sh.text(left, y - (k + 1) * 8 - 2, s, 6)
    top = y - len(F.get("small", [])) * 8 - 2 - sz
    if top < ph - L["bottom"] + 4:
        raise GenError("%s: footer reaches into the table area" % L["id"])


def draw_after(sh, L, st, y):
    A = L["after"]
    sz = L["size"]
    rows = st["rows"]
    vals = {
        "fees": sum(r["amt"] for r in rows if r["cat"] == "fee"),
        "interest": sum(r["amt"] for r in rows if r["cat"] == "interest"),
        "rwt": sum(r["amt"] for r in rows if r["cat"] == "rwt"),
        "count": len(rows),
    }
    x, w = A["x"], A["w"]
    t = nudge(L, [x + w - 4], x, x + w, "after-table block")
    x += t
    sh.text(x, y + sz, A["title"], sz + 0.5, bold=True)
    for k, (lab, key) in enumerate(A["rows"]):
        yy = y + (k + 2) * (sz + 4)
        sh.text(x, yy, lab, sz)
        v = str(vals[key]) if key == "count" else mag(vals[key], L["thousands"], L["dollar"])
        sh.text(x + w - 4, yy, v, sz, align="right")


def render(path, L, st):
    pw, ph = PAGES[L["page"]]
    sh = Sheet(path, (pw, ph), L["font"])
    rows = st["rows"]
    npg = len(st["pages"])
    for k, r in enumerate(rows):
        r["zebra"] = (k % 2 == 1)
    tot_d = sum(r["debit"] or 0 for r in rows)
    tot_c = sum(r["credit"] or 0 for r in rows)
    bal_col = col_of(L, "balance")
    last_bal = st["opening"]
    for pi, items in enumerate(st["pages"]):
        sh.begin_page(st["dx"][pi])
        if pi == 0:
            draw_first_header(sh, L, st)
        else:
            draw_cont_header(sh, L, st, pi + 1, npg)
        prev_date = None
        page_idx = [idx for kk, idx, _ in items if kk == "txn"]
        y_min, y_max = None, None
        for kind, idx, y in items:
            if kind == "head":
                draw_heading(sh, L, y)
                y_min = y - L["head_size"] - 3
            elif kind == "txn":
                r = rows[idx]
                show = not (L["date_once"] and prev_date == r["date"])
                yl = draw_txn(sh, L, st, idx, y, show)
                prev_date = r["date"]
                last_bal = r["bal"]
                y_max = yl
                if y_min is None:
                    y_min = y - L["size"] - 3
            elif kind in ("open", "bf", "cf"):
                if kind == "open":
                    if L["opening"].get("date"):
                        cell(sh, L, L["cols"][0], y, fmt_date(st["start"], L["date_fmt"]))
                    draw_label(sh, L, y, L["opening"]["label"])
                    v = st["opening"]
                    fc = figure_col(L)
                else:
                    draw_label(sh, L, y, L["cf_bf"][0] if kind == "cf" else L["cf_bf"][1])
                    v = last_bal
                    fc = bal_col
                if fc.kind == "amount" and L["balance_kind"] != "card":
                    raise GenError("%s: an opening figure in a signed amount column" % L["id"])
                num, tok = fmt_cell(L, fc.kind, v)
                money_cell(sh, L, fc, y, num, tok)
                if y_min is None:
                    y_min = y - L["size"] - 3
            elif kind == "ptot":
                draw_label(sh, L, y, L["page_totals"], bold=True)
                pd = sum(rows[i]["debit"] or 0 for i in page_idx)
                pc = sum(rows[i]["credit"] or 0 for i in page_idx)
                for kk, v in (("debit", pd), ("credit", pc)):
                    num, tok = fmt_cell(L, kk, v)
                    money_cell(sh, L, col_of(L, kk), y, num, tok, bold=True)
            elif kind in ("totals", "totals_close"):
                draw_label(sh, L, y, L["totals_label"], bold=True)
                for kk, v in (("debit", tot_d), ("credit", tot_c)):
                    num, tok = fmt_cell(L, kk, v)
                    money_cell(sh, L, col_of(L, kk), y, num, tok, bold=True)
                if kind == "totals_close":
                    num, tok = fmt_cell(L, "balance", st["closing"])
                    money_cell(sh, L, bal_col, y, num, tok, bold=True)
            elif kind == "close":
                draw_label(sh, L, y, L["closing_label"], bold=True)
                fc = figure_col(L)
                num, tok = fmt_cell(L, fc.kind, st["closing"])
                money_cell(sh, L, fc, y, num, tok, bold=True)
            elif kind == "after":
                draw_after(sh, L, st, y)
            else:
                raise GenError("unknown page item %r" % kind)
            if kind in ("open", "bf", "cf", "ptot", "totals", "totals_close", "close"):
                y_max = y
        if L["col_rules"] and y_min is not None and y_max is not None:
            for c in L["cols"][1:]:
                sh.line(c.x0, y_min, c.x0, y_max + 4, width=0.3, color=(0.55, 0.55, 0.55))
        draw_footer(sh, L, st, pi + 1, npg)
        sh.end_page()
    sh.save()


# ---------------------------------------------------------------------------
# The truth, and the checks it must pass before it is written.
# ---------------------------------------------------------------------------

def cents(x):
    return None if x is None else int(round(x * 100))


def verify(L, st, truth):
    name = truth["case"]
    rows = st["rows"]
    card = L["balance_kind"] == "card"
    # Arithmetic, on the truth as written (floats round-tripped back to cents).
    bal = cents(truth["opening_balance"])
    for k, r in enumerate(truth["rows"]):
        d, c = cents(r["debit"]), cents(r["credit"])
        if (d is None) == (c is None) or (d is not None and d <= 0) or (c is not None and c <= 0):
            raise GenError("%s row %d: needs exactly one positive debit or credit" % (name, k))
        bal = bal - (d or 0) + (c or 0)
        if r["balance"] is not None and cents(r["balance"]) != bal:
            raise GenError("%s row %d: printed balance %s, arithmetic says %s"
                           % (name, k, r["balance"], bal / 100.0))
        if not re.fullmatch(r"\d{4}-\d\d-\d\d", r["date"]):
            raise GenError("%s row %d: bad date %r" % (name, k, r["date"]))
    if bal != cents(truth["closing_balance"]):
        raise GenError("%s: rows do not reach the closing balance" % name)
    if truth["row_count"] != len(truth["rows"]) or not 8 <= len(rows) <= 60:
        raise GenError("%s: row count %d" % (name, len(rows)))
    # The PDF against the truth: what was printed must read back to it.
    for k, (r, t) in enumerate(zip(rows, truth["rows"])):
        rec = r.get("rec")
        if rec is None:
            raise GenError("%s row %d: never drawn" % (name, k))
        for key in ("date", "pdate"):
            s = rec[key]
            if s is None:
                continue
            hits = [d for d in daterange(st["start"] - dt.timedelta(days=45),
                                         st["end"] + dt.timedelta(days=10))
                    if fmt_date(d, L["date_fmt"]) == s]
            want = r["date"] if key == "date" else r["pdate"]
            if hits != [want]:
                raise GenError("%s row %d: printed %s %r reads as %s, truth %s"
                               % (name, k, key, s, hits, want))
        if rec["date"] is not None and dt.date.fromisoformat(t["date"]) != r["date"]:
            raise GenError("%s row %d: truth date disagrees" % (name, k))
        if " ".join(rec["text"]) != t["description"]:
            raise GenError("%s row %d: printed %r, truth %r"
                           % (name, k, " ".join(rec["text"]), t["description"]))
        m = rec["money"]
        for kk in ("debit", "credit"):
            if kk in m and parse_money(*m[kk]) != cents(t[kk]):
                raise GenError("%s row %d: %s cell %r" % (name, k, kk, m[kk]))
        if "amount" in m:
            v = parse_money(*m["amount"], card=card)
            want = -cents(t["debit"]) if t["debit"] is not None else cents(t["credit"])
            if v != want:
                raise GenError("%s row %d: amount cell %r reads %d, truth %d"
                               % (name, k, m["amount"], v, want))
        printed = [kk for kk in ("debit", "credit", "amount") if kk in m]
        if len(printed) != 1:
            raise GenError("%s row %d: %d amount cells printed" % (name, k, len(printed)))
        if ("balance" in m) != (t["balance"] is not None):
            raise GenError("%s row %d: balance printed/truth disagree" % (name, k))
        if "balance" in m and parse_money(*m["balance"]) != cents(t["balance"]):
            raise GenError("%s row %d: balance cell %r" % (name, k, m["balance"]))
    if L["date_fmt"] in YEARLESS and "period" not in L["hdr"].get("det_rows", ["period"]):
        raise GenError("%s: a year-less date with no printed period" % name)


def daterange(a, b):
    d = a
    while d <= b:
        yield d
        d += dt.timedelta(days=1)


def features(L, st):
    f = ["cols:" + "|".join(L["kinds"]), "date:" + L["date_fmt"],
         "font:%s" % L["font"], "size:%g" % L["size"], "page:" + L["page"],
         "pages:%d" % len(st["pages"])]
    if L["date_fmt"] in YEARLESS:
        f.append("date_yearless")
    if st["start"].year != st["end"].year:
        f.append("period_crosses_year")
    f.append("money:thousands" if L["thousands"] else "money:no_thousands")
    if L["dollar"]:
        f.append("money:dollar")
    if "amount" in L["kinds"]:
        f.append("sign:" + L["amount_style"])
    if "balance" in L["kinds"]:
        f.append("bal:" + L["balance_style"])
    if L["hang"]:
        f.append("token_column")
    if L["bal_mode"] == "last_of_day":
        f.append("balance_last_of_day")
    if "balance" not in L["kinds"]:
        f.append("no_balance_column")
    f.append("heading_every_page" if L["head_every_page"] else "heading_page1_only")
    if any(len(c.head) > 1 for c in L["cols"]):
        f.append("two_line_heading")
    if any(c.align == "center" for c in L["cols"] if c.kind in MONEY_KINDS):
        f.append("money_centred")
    if L["opening"]:
        f.append("opening_line")
    if "close" in L["trailer"]:
        f.append("closing_line")
    if "totals" in L["trailer"] or "totals_close" in L["trailer"]:
        f.append("totals_line")
    if L["cf_bf"]:
        f.append("carried_forward")
    if L["page_totals"]:
        f.append("page_totals")
    if L["hdr"].get("summary"):
        f.append("summary_box")
    if L["hdr"].get("sidebar"):
        f.append("sidebar")
    if L["after"]:
        f.append("after_table_block")
    if L["sections"]:
        f.append("card_sections")
    if any(st["dx"]):
        f.append("page_x_offset")
    if L["date_once"]:
        f.append("date_once_per_day")
    if L["zebra"]:
        f.append("zebra")
    if L["col_rules"] or L["row_rules"]:
        f.append("rules")
    if L["head_style"] in ("dark", "bar"):
        f.append("shaded_heading")
    rows = st["rows"]
    if any(r["nlines"] > 1 for r in rows):
        f.append("multiline_desc")
    if any(r.get("long") for r in rows):
        f.append("long_desc")
    if any(r["money_like"] for r in rows):
        f.append("money_like_desc")
    if "type" in L["kinds"]:
        f.append("type_column")
    if "code" in L["kinds"]:
        f.append("particulars_code_reference")
    pr = page_rows(st["pages"])
    if len(pr) > 1 and min(len(x) for x in pr) <= 2:
        f.append("short_page")
    n_c = sum(1 for r in rows if r["dir"] == "C")
    if L["balance_kind"] != "card" and n_c <= 2:
        f.append("sparse_credits")
    if len(pr) > 1 and not any(rows[i]["dir"] == "C" for i in pr[0]):
        f.append("no_credits_page1")
    if any(r["bal"] < 0 for r in rows) and L["balance_kind"] != "card":
        f.append("negative_balance")
    return f


def build_case(L, k, spec, out_dir):
    name = "%s_%d" % (L["id"], k + 1)
    seed = SPLIT_SEED[L["split"]] + zlib.crc32(("%s:%d" % (L["id"], k)).encode("utf-8"))
    st = build_statement(L, spec, seed)
    want_short = spec.get("short_last")
    pr = page_rows(st["pages"])
    if want_short and len(pr[-1]) > 2:
        raise GenError("%s: asked for a short last page, got %d rows" % (name, len(pr[-1])))
    plan = spec.get("credits") or L["credits"]
    if plan in ("sparse_none_p1", "none_p1") and any(st["rows"][i]["dir"] == "C" for i in pr[0]):
        raise GenError("%s: a deposit landed on page 1" % name)
    pdf = os.path.join(out_dir, name + ".pdf")
    render(pdf, L, st)
    rows = st["rows"]
    feats = features(L, st)
    truth = {
        "case": name,
        "generator": GENERATOR,
        "note": "%s split. %s (%s, fictional). %s" % (L["split"], L["bank"], L["product"], L["note"]),
        "layout": L["id"],
        "features": feats,
        "opening_balance": st["opening"] / 100.0,
        "closing_balance": st["closing"] / 100.0,
        "row_count": len(rows),
        "rows": [{"date": r["date"].isoformat(),
                  "description": r["desc"],
                  "debit": None if r["debit"] is None else r["debit"] / 100.0,
                  "credit": None if r["credit"] is None else r["credit"] / 100.0,
                  "balance": r["bal"] / 100.0 if r["bal_printed"] else None}
                 for r in rows],
    }
    verify(L, st, truth)
    with open(os.path.join(out_dir, name + ".truth.json"), "w") as f:
        json.dump(truth, f, indent=1, sort_keys=True)
    return name, truth, len(st["pages"])


# ---------------------------------------------------------------------------
# THE DESIGNS. Each is a plausible bank's statement, named for a fictional bank
# and noting which real shape it follows. S.v / S.ch / S.r give dev one set of
# parameters and holdout a disjoint one.
# ---------------------------------------------------------------------------

ARCH = []


def design(splits="both"):
    def deco(fn):
        ARCH.append((fn.__name__, splits, fn))
        return fn
    return deco


def small_print(bank, phone="0800 000 000"):
    return ["Please check this statement carefully and tell us about anything that looks wrong "
            "within 30 days. Call %s." % phone,
            "%s is a fictional bank. This statement is synthetic test data." % bank]


@design()
def kauri_everyday(S):
    size = S.v(8, 8.5)
    left, right = S.r((36, 42), (48, 54)), S.r((553, 558), (540, 546))
    two = S.v(False, True)

    def hd(w):
        return (w, "($)") if two else (w,)
    cols = mkcols(left, right, [
        ("date", S.r((38, 42), (32, 36)), ("Date",)),
        ("desc", None, S.v(("Transaction type and details",), ("Transaction details",))),
        ("debit", S.r((64, 70), (72, 78)), hd("Withdrawals")),
        ("credit", S.r((62, 68), (66, 72)), hd("Deposits")),
        ("balance", S.r((72, 80), (80, 86)), hd("Balance")),
    ])
    sx = S.r((318, 326), (296, 304))
    return layout(
        S, "kauri_everyday", bank="Kauri Bank", product="Everyday Account",
        note="ANZ-like everyday account: type line over details line, balance printed only on "
             "the last transaction of each day, page totals and a period totals line.",
        font="Helvetica", size=size, pitch=round(size * S.v(1.75, 1.6), 2), cols=cols,
        date_fmt=S.v("dd Mon", "d Mon"), period_fmt="long", catalog="bank", wrap=0.4,
        bal_mode="last_of_day", opening=dict(label="OPENING BALANCE", date=S.v(True, False)),
        page_totals="Totals at end of page", trailer=["totals_close"],
        totals_label="Totals at end of period", head_style="rule",
        hdr=dict(mast="bar", color=NAVY, title="Account statement", left=left, right=right,
                 addr=(left, 100), det=(sx, 96, 84), det_rows=["product", "acct", "period", "stno"],
                 summary=dict(style="box", x=sx, y=154, w=right - sx - 8, title="Account summary",
                              rows=[("Opening balance", "open"), ("Total withdrawals", "tot_d"),
                                    ("Total deposits", "tot_c"), ("Closing balance", "close")]),
                 table_title="Account transactions"),
        table_top1=272, table_top2=84,
        footer=dict(pos="right", other="Kauri Bank New Zealand Limited",
                    small=small_print("Kauri Bank")),
        bottom=S.v(70, 74),
        stmts=[dict(n=S.n(30, 40), ym=S.v((2026, 1), (2026, 3))),
               dict(n=S.n(18, 26), ym=S.v((2025, 12, 15), (2025, 11)), short_last=True)])


@design()
def kauri_visa(S):
    size = S.v(8, 7.5)
    left, right = S.r((40, 44), (30, 34)), S.r((550, 556), (562, 566))
    two = S.v(True, False)
    cols = mkcols(left, right, [
        ("date", S.r((52, 56), (40, 44)), ("Transaction", "date") if two else ("Date",)),
        ("pdate", S.r((46, 50), (44, 48)), ("Processed", "date") if two else ("Processed",)),
        ("desc", None, ("Transaction details",)),
        ("amount", S.r((74, 80), (64, 70)), ("Amount", "NZ$") if two else ("Amount $",)),
    ])
    return layout(
        S, "kauri_visa", bank="Kauri Bank", product="Visa Platinum",
        note="ANZ-like credit card: transaction and processed dates, purchases plain, "
             "payments marked CR, foreign-currency line under overseas purchases, no running balance.",
        font="Helvetica", size=size, cols=cols, date_fmt=S.v("dd Mon", "dd MON"),
        period_fmt=S.v("long", "short"), catalog="card", credits="card", balance_kind="card",
        bal_mode="none", amount_style="card_cr", hang=S.v((), ("amount",)), wrap=0.3,
        opening=S.v(dict(label="Opening balance"), None), trailer=["close"],
        closing_label="Closing balance", head_style=S.v("bar", "rule"),
        card=dict(limit=S.ch([500000, 800000], [1000000, 1200000])), rate_text="20.95% p.a.",
        hdr=dict(mast="bar", color=NAVY, title="Credit card statement", left=left, right=right,
                 addr=(left, 104), det=(S.r((300, 310), (330, 336)), 100, 76),
                 det_rows=["card", "period", "issued"],
                 summary=dict(style="box", x=left, y=S.v(170, 176), w=S.r((220, 236), (240, 250)),
                              title="Account summary",
                              rows=[("Opening balance", "open"), ("Purchases & debits", "tot_d"),
                                    ("Payments & credits", "tot_c"), ("Closing balance", "close")]),
                 sidebar=dict(style="shaded", x=S.r((330, 340), (344, 350)), y=S.v(170, 176),
                              w=S.r((200, 210), (206, 212)), title="Payment due",
                              rows=[("Minimum payment", "minpay"), ("Due date", "due"),
                                    ("Credit limit", "limit"), ("Available credit", "avail"),
                                    ("Purchase rate", "rate")])),
        table_top1=S.v(286, 292), table_top2=S.v(86, 92),
        footer=dict(pos=S.v("center", "left"), small=small_print("Kauri Bank")),
        stmts=[dict(n=S.n(22, 34), ym=S.v((2026, 2, 17), (2026, 4, 9)))] +
              S.v([dict(n=S.n(10, 16), ym=(2025, 10, 17))], []))


@design()
def harbour_pcr(S):
    font, size = S.v(("Helvetica", 7.5), ("Times", 8))
    left, right = S.r((30, 34), (40, 46)), S.r((562, 566), (552, 556))
    cols = mkcols(left, right, [
        ("date", S.r((44, 48), (36, 40)), ("Date",)),
        ("part", None, ("Particulars",)),
        ("code", S.r((58, 62), (64, 70)), ("Code",)),
        ("ref", S.r((74, 80), (66, 72)), ("Reference",)),
        ("debit", S.r((60, 64), (66, 70)), S.v(("Withdrawals",), ("Debit",))),
        ("credit", S.r((58, 62), (62, 66)), S.v(("Deposits",), ("Credit",))),
        ("balance", S.r((66, 72), (70, 76)), ("Balance",)),
    ])
    return layout(
        S, "harbour_pcr", bank="Harbour Bank", product="Streamline Account",
        note="ASB-like NZ layout: Particulars, Code and Reference as separate text columns "
             "(12-character fields), shaded alternate rows.",
        font=font, size=size, cols=cols, date_fmt=S.v("dd/mm/yyyy", "dd/mm/yy"),
        period_fmt="num", period_sep=" - ", catalog="pcr", zebra=True,
        head_every_page=S.v(True, False), head_style="dark", head_fill=(0.15, 0.15, 0.15),
        opening=dict(label="Opening Balance"), trailer=["close"], closing_label="Closing Balance",
        hdr=dict(mast="bar", color=YELLOW, ink=BLACK, title="Statement of account",
                 left=left, right=right, addr=(left, S.v(96, 104)),
                 det=(S.r((320, 330), (290, 300)), S.v(96, 104), 80),
                 det_rows=["name", "acct", "period", "branch"],
                 summary=dict(style="band", x=left, y=S.v(166, 172), w=right - left,
                              rows=[("Opening balance", "open"), ("Total debits", "tot_d"),
                                    ("Total credits", "tot_c"), ("Closing balance", "close")])),
        table_top1=S.v(236, 244), table_top2=S.v(80, 86),
        footer=dict(pos="right", other="Branch {branch}", small=small_print("Harbour Bank")),
        stmts=[dict(n=S.n(36, 48), ym=S.v((2026, 5), (2026, 6)))] +
              S.v([], [dict(n=S.n(12, 18), ym=(2025, 8))]))


@design()
def southern_bf(S):
    size = S.v(9, 8)
    left, right = S.r((46, 52), (36, 40)), S.r((548, 552), (556, 560))
    cols = mkcols(left, right, [
        ("date", S.r((58, 62), (60, 64)), ("Date",)),
        ("desc", None, S.v(("Transactions",), ("Description",))),
        ("debit", S.r((70, 76), (64, 68)), ("Withdrawals",)),
        ("credit", S.r((66, 72), (62, 66)), ("Deposits",)),
        ("balance", S.r((76, 82), (72, 76)), ("Balance",)),
    ])
    return layout(
        S, "southern_bf", bank="Southern Cross Bank", product="Totally Free Account",
        note="BNZ-like: date printed once per day, balance brought forward / carried forward "
             "at page breaks, summary band across the page.",
        font="Helvetica", size=size, cols=cols, date_fmt=S.v("d Mon yyyy", "dd Mon yyyy"),
        period_fmt="short", catalog="bank", wrap=0.2, date_once=True,
        cf_bf=S.v(("Balance carried forward", "Balance brought forward"),
                  ("Carried forward", "Brought forward")),
        opening=dict(label="Balance brought forward"), trailer=["close"],
        pitch=round(size * S.v(2.0, 2.2), 2), max_rows=S.v(None, 26), head_style="rule2",
        hdr=dict(mast="logo", color=(0.0, 0.25, 0.55), title="Statement", left=left, right=right,
                 addr=(left, 92), det=(S.r((310, 316), (330, 336)), 92, S.v(86, 80)),
                 det_rows=["acct", "period", "stno"],
                 summary=dict(style="band", x=left, y=S.v(150, 156), w=right - left,
                              rows=[("Opening balance", "open"), ("Withdrawals", "tot_d"),
                                    ("Deposits", "tot_c"), ("Closing balance", "close")]),
                 cont="line", top_page=True),
        table_top1=S.v(222, 230), table_top2=S.v(92, 96),
        footer=dict(pos="left", fmt="Page {p}", small=small_print("Southern Cross Bank")),
        stmts=[dict(n=S.n(50, 58), ym=S.v((2026, 3), (2026, 7))),
               dict(n=S.n(34, 40), ym=S.v((2025, 7), (2025, 5)), short_last=True)])


@design()
def tasman_crdr(S):
    font, size = S.v(("Helvetica", 8.5), ("Helvetica", 9))
    left, right = S.r((40, 44), (50, 56)), S.r((552, 556), (544, 548))
    cols = mkcols(left, right, [
        ("date", S.r((52, 56), (48, 52)), ("Date",)),
        ("desc", None, S.v(("Transaction description",), ("Details",))),
        ("debit", S.r((62, 66), (58, 62)), ("Debit",)),
        ("credit", S.r((62, 66), (58, 62)), ("Credit",)),
        ("balance", S.r((82, 88), (84, 90)), ("Balance",)),
    ])
    return layout(
        S, "tasman_crdr", bank="Tasman Banking Corporation", product="Choice Everyday",
        note="Westpac-like: Debit / Credit columns, balance carries a CR or DR token, account "
             "dips into overdraft, long descriptions run up to the debit column.",
        font=font, size=size, cols=cols, date_fmt=S.v("dd/mm/yyyy", "dd-mm-yy"),
        period_fmt=S.v("long", "num"), catalog="bank", long_desc=0.35, wrap=0.0,
        balance_style="drcr", hang=S.v((), ("balance",)), balance_kind="overdraft",
        trailer=["close"], head_style="rule", head_every_page=True,
        hdr=dict(mast="plain", title="Statement of account", tagline="Tasman Banking Corporation "
                 "New Zealand Limited", left=left, right=right, addr=(left, 104),
                 det=(S.r((330, 338), (300, 306)), 100, 80),
                 det_rows=["acct", "period", "issued"],
                 summary=dict(style="box", x=S.r((330, 338), (300, 306)), y=150,
                              w=S.r((200, 210), (226, 236)), title="Summary",
                              rows=[("Opening balance", "open"), ("Debits", "tot_d"),
                                    ("Credits", "tot_c"), ("Closing balance", "close")])),
        table_top1=258, table_top2=S.v(88, 80),
        footer=dict(pos="center", small=small_print("Tasman Banking Corporation")),
        stmts=[dict(n=S.n(24, 34), ym=S.v((2026, 4), (2026, 2)))] +
              S.v([dict(n=S.n(10, 14), ym=(2025, 11))], [dict(n=S.n(40, 46), ym=(2025, 10))]))


@design()
def fern_moneyout(S):
    font, size = S.v(("Helvetica", 9), ("Times", 9))
    left, right = S.r((50, 56), (40, 44)), S.r((545, 550), (553, 557))
    cols = mkcols(left, right, [
        ("date", S.r((62, 66), (60, 64)), ("Date",)),
        ("desc", None, ("Description",)),
        ("debit", S.r((70, 76), (74, 80)), S.v(("Money out",), ("Paid out",))),
        ("credit", S.r((70, 76), (72, 78)), S.v(("Money in",), ("Paid in",))),
        ("balance", S.r((76, 82), (80, 86)), ("Balance",)),
    ])
    return layout(
        S, "fern_moneyout", bank="Fern Bank", product="Free Up Account",
        note="Kiwibank-like: weekday dates with no year, $ figures, shaded alternate rows, an "
             "interest and fees sidebar.",
        font=font, size=size, cols=cols, date_fmt=S.v("Dow dd Mon", "Dow d Mon"),
        period_fmt="long", catalog="bank", wrap=0.15, dollar=True, zebra=True,
        head_every_page=S.v(False, True), head_style="bar", head_fill=(0.85, 0.93, 0.85),
        trailer=["close"], opening=S.v(None, dict(label="Opening balance")),
        rate_text=S.v("0.10% p.a.", "0.25% p.a."),
        hdr=dict(mast="logo", color=GREEN, title="Your statement", left=left, right=right,
                 addr=(left, 96), det=(left, 160, 90), det_rows=["acct", "period"],
                 sidebar=dict(style="box", x=S.r((350, 360), (330, 340)), y=88,
                              w=S.r((180, 190), (210, 216)), title="Your account at a glance",
                              rows=[("Opening balance", "open"), ("Money out", "tot_d"),
                                    ("Money in", "tot_c"), ("Closing balance", "close"),
                                    ("Interest rate", "rate"), ("Fees this period", "fees")])),
        table_top1=S.v(232, 226), table_top2=S.v(70, 84),
        footer=dict(pos="right", small=small_print("Fern Bank")),
        stmts=[dict(n=S.n(36, 46), ym=S.v((2026, 6), (2026, 8))),
               dict(n=S.n(14, 20), ym=S.v((2025, 12, 20), (2026, 1, 12)))])


@design()
def rata_signed(S):
    font, size = S.v(("Times", 9), ("Helvetica", 8))
    left, right = S.r((54, 60), (40, 44)), S.r((540, 546), (552, 556))
    cols = mkcols(left, right, [
        ("date", S.r((48, 52), (56, 60)), ("Date",)),
        ("desc", None, ("Details",)),
        ("amount", S.r((70, 76), (80, 86)), ("Amount",)),
        ("balance", S.r((76, 82), (80, 86)), ("Balance",)),
    ])
    return layout(
        S, "rata_signed", bank="Rata Savings Bank", product="Everyday",
        note="One signed Amount column (leading minus on money out) beside a running balance; "
             "opening balance as the first line, closing balance as the last.",
        font=font, size=size, cols=cols, date_fmt=S.v("dd-mm-yy", "dd-mm-yyyy"),
        period_fmt=S.v("short", "long"), catalog="bank", wrap=S.v(0.0, 0.2), wrap_indent=S.v(0, 6),
        amount_style="lead", opening=dict(label="Opening balance"), trailer=["close"],
        head_style="rule2",
        hdr=dict(mast="right", title="Account statement", left=left, right=right,
                 addr=(left, 92), det=(left, 160, 96), det_rows=["acct", "period", "stno"],
                 summary=dict(style="box", x=S.r((320, 330), (340, 350)), y=104,
                              w=S.r((190, 200), (190, 196)), title="Account summary",
                              rows=[("Opening balance", "open"), ("Total paid out", "tot_d"),
                                    ("Total paid in", "tot_c"), ("Closing balance", "close")])),
        table_top1=S.v(232, 236), table_top2=80,
        footer=dict(pos="right", other="Statement {stno}", small=small_print("Rata Savings Bank")),
        stmts=[dict(n=S.n(20, 30), ym=S.v((2026, 7), (2026, 9)))] +
              S.v([dict(n=S.n(44, 52), ym=(2026, 8))], []))


@design()
def coastal_mainframe(S):
    size = S.v(8, 7.5)
    left, right = S.r((36, 40), (44, 48)), S.r((556, 560), (548, 552))
    cols = mkcols(left, right, [
        ("date", S.r((50, 54), (40, 44)), ("DATE",)),
        ("desc", None, ("DESCRIPTION",)),
        ("amount", S.r((84, 90), (90, 96)), ("AMOUNT",)),
        ("balance", S.r((90, 96), (96, 102)), ("BALANCE",)),
    ])
    return layout(
        S, "coastal_mainframe", bank="Coastal Credit Union", product="Savings Plus",
        note="Monospaced mainframe print: trailing minus on amounts and balances, dashed "
             "heading underline, BALANCE B/F and C/F lines.",
        font="Courier", size=size, pitch=round(size * S.v(1.5, 1.45), 2), cols=cols,
        date_fmt=S.v("dd MON yy", "dd MON"), period_fmt="upper", period_sep=" TO ",
        period_label="PERIOD", catalog="bank", thousands=S.v(False, True),
        amount_style="trail", balance_style="trail", balance_kind="overdraft",
        head_style="dashes", cf_bf=("BALANCE C/F", "BALANCE B/F"),
        opening=dict(label="BALANCE B/F"), trailer=["close"], closing_label="CLOSING BALANCE",
        hdr=dict(mast="courier", title="Statement of account", left=left, right=right,
                 addr=(left, 90), addr_size=8, det=(S.r((316, 324), (300, 306)), 90, 82),
                 det_size=8, det_rows=["acct", "period", "stno"], cont="courier"),
        table_top1=S.v(170, 176), table_top2=80, max_rows=S.v(36, 40),
        footer=dict(pos="center", fmt="PAGE {p} OF {n}", size=7),
        stmts=[dict(n=S.n(48, 58), ym=S.v((2026, 1), (2026, 4)), short_last=True)] +
              S.v([], [dict(n=S.n(16, 22), ym=(2026, 5))]))


@design()
def rimu_paren(S):
    size = S.v(10, 9)
    left, right = S.r((50, 54), (40, 44)), S.r((560, 564), (570, 574))
    cols = mkcols(left, right, [
        ("date", S.r((96, 100), (92, 96)), ("Date",)),
        ("desc", None, ("Particulars",)),
        ("amount", S.r((82, 88), (78, 84)), ("Amount", "($)")),
        ("balance", S.r((82, 88), (84, 90)), ("Balance", "($)")),
    ])
    return layout(
        S, "rimu_paren", bank="Rimu Building Society", product="Investment Share Account",
        note="Building-society style on US Letter: money out in parentheses, full month names, "
             "two-line headings repeated on every page.",
        page="Letter", font="Times", size=size, cols=cols, date_fmt="d Month yyyy",
        period_fmt="long", catalog=S.v("savings", "bank"), credits=S.v("normal", "normal"),
        credit_p=S.v(0.4, 0.3), amount_style="paren", balance_style="paren",
        opening=dict(label="Opening balance", date=True), trailer=["close"], head_style="rule",
        hdr=dict(mast="plain", title="Statement", tagline="A mutual building society",
                 left=left, right=right, addr=(left, 104), addr_size=10,
                 det=(S.r((320, 330), (340, 350)), 104, 88), det_size=9,
                 det_rows=["acct", "period", "stno"],
                 summary=dict(style="shaded", x=left, y=176, w=S.r((230, 240), (250, 260)),
                              title="Summary", size=9,
                              rows=[("Opening balance", "open"), ("Withdrawals", "tot_d"),
                                    ("Deposits", "tot_c"), ("Closing balance", "close")])),
        table_top1=S.v(296, 300), table_top2=S.v(84, 90),
        footer=dict(pos="center", fmt="Page {p} of {n}", small=small_print("Rimu Building Society")),
        stmts=[dict(n=S.n(16, 26), ym=S.v((2026, 1), (2026, 7)), months=S.v(3, 2))])


@design()
def pukeko_drcr(S):
    size = S.v(7, 7.5)
    left, right = S.r((44, 48), (34, 38)), S.r((548, 552), (558, 562))
    cols = mkcols(left, right, [
        ("date", S.r((50, 54), (46, 50)), ("Date",)),
        ("desc", None, ("Details",)),
        ("amount", S.r((70, 76), (76, 82)), ("Amount",)),
        ("balance", S.r((74, 80), (80, 86)), ("Balance",)),
    ])
    return layout(
        S, "pukeko_drcr", bank="Pukeko Bank", product="Business Edge",
        note="Signed Amount with a separate DR / CR token after the figure, and the balance "
             "carrying its own CR / DR token.",
        font="Helvetica", size=size, cols=cols, date_fmt=S.v("d Mon yyyy", "dd Mon yy"),
        period_fmt="short", catalog="biz", customers="biz", scale=S.v(1.0, 0.6),
        amount_style="drcr", balance_style="drcr", hang=("amount", "balance"),
        balance_kind="overdraft", wrap=0.2, trailer=["close"], head_style="bar",
        opening=dict(label="Opening balance"),
        hdr=dict(mast="right", title="Business statement", left=left, right=right,
                 addr=(left, 88), addr_bold=True, det=(left, 150, 84),
                 det_rows=["name", "acct", "period", "branch"],
                 summary=dict(style="box", x=S.r((340, 350), (320, 330)), y=96,
                              w=S.r((180, 190), (200, 210)), title="Account summary", size=7.5,
                              rows=[("Opening balance", "open"), ("Total debits", "tot_d"),
                                    ("Total credits", "tot_c"), ("Closing balance", "close")])),
        table_top1=S.v(222, 214), table_top2=S.v(76, 72),
        footer=dict(pos="right", other="Pukeko Bank Ltd - Branch {branch}"),
        stmts=[dict(n=S.n(40, 55), ym=S.v((2026, 2), (2026, 10)))] +
              S.v([], [dict(n=S.n(20, 28), ym=(2026, 11))]))


@design()
def totara_type(S):
    font, size = S.v(("Helvetica", 8), ("Courier", 7.5))
    left, right = S.r((36, 40), (46, 50)), S.r((556, 560), (548, 552))
    cols = mkcols(left, right, [
        ("date", S.r((50, 54), (50, 54)), ("Date",)),
        ("type", S.r((44, 48), (40, 44)), ("Type",)),
        ("desc", None, ("Details",)),
        ("amount", S.r((70, 76), (84, 90)), ("Amount",)),
        ("balance", S.r((74, 80), (84, 90)), ("Balance",)),
    ])
    return layout(
        S, "totara_type", bank="Totara Bank", product="Everyday Plus",
        note="Westpac-like transaction-type column (EFTPOS, AP, DD, BP, DC ...) before the "
             "details, one signed Amount column.",
        font=font, size=size, cols=cols, date_fmt=S.v("dd/mm/yyyy", "d/mm/yyyy"),
        period_fmt="num", catalog="typed", amount_style=S.v("lead", "paren"),
        balance_style="lead", trailer=["close"], head_style=S.v("rule", "box"),
        row_rules=S.v(True, False),
        hdr=dict(mast="bar", color=MAROON, title="Statement", left=left, right=right,
                 addr=(left, 98), det=(S.r((330, 340), (300, 310)), 98, 80),
                 det_rows=["acct", "period", "stno"],
                 sidebar=dict(style="box", x=S.r((330, 340), (300, 310)), y=146,
                              w=S.r((200, 210), (230, 240)), title="Important information",
                              rows=[("Interest rate", "rate"), ("Fees this period", "fees")],
                              notes=["Fees are charged on the last business day."])),
        rate_text=S.v("0.05% p.a.", "0.50% p.a."),
        table_top1=S.v(236, 244), table_top2=80,
        footer=dict(pos="left", other="Ref {stno}-{acct}", small=small_print("Totara Bank")),
        stmts=[dict(n=S.n(26, 36), ym=S.v((2026, 3), (2026, 5))),
               dict(n=S.n(8, 12), ym=S.v((2026, 4), (2026, 6)))])


@design()
def matai_paidout(S):
    size = S.v(8.5, 9.5)
    left, right = S.r((40, 44), (52, 58)), S.r((552, 556), (540, 546))
    cols = mkcols(left, right, [
        ("date", S.r((62, 66), (88, 92)), ("Date",)),
        ("desc", None, ("Description",)),
        ("debit", S.r((64, 70), (68, 74)), S.v(("Paid out", "($)"), ("Payments", "$"))),
        ("credit", S.r((64, 70), (68, 74)), S.v(("Paid in", "($)"), ("Receipts", "$"))),
        ("balance", S.r((72, 78), (76, 82)), S.v(("Balance", "($)"), ("Balance", "$"))),
    ])
    return layout(
        S, "matai_paidout", bank="Matai Bank", product="Cheque Account",
        note="Two-line money headings, headings printed on page 1 only, multi-line "
             "descriptions with an indented second line.",
        font="Helvetica", size=size, cols=cols, date_fmt=S.v("d Mon yyyy", "d Month yyyy"),
        period_fmt="short", catalog="bank", wrap=0.35, wrap_indent=S.v(8, 0),
        head_every_page=False, head_style="rule", trailer=["totals", "close"],
        totals_label="Totals", opening=dict(label="Opening balance", date=True),
        hdr=dict(mast="plain", title="Statement of account", left=left, right=right,
                 addr=(left, 100), det=(S.r((310, 320), (300, 306)), 100, 84),
                 det_rows=["acct", "period", "stno", "branch"],
                 cont="minimal", top_page=True),
        table_top1=S.v(190, 196), table_top2=S.v(70, 76),
        footer=dict(pos="right", small=small_print("Matai Bank")[:1]),
        stmts=[dict(n=S.n(40, 52), ym=S.v((2026, 9), (2026, 2)), short_last=True)])


@design()
def kahikatea_land(S):
    size = S.v(8, 7.5)
    left = S.r((34, 38), (44, 48))
    right = S.r((636, 640), (626, 630))
    cols = mkcols(left, right, [
        ("date", S.r((48, 52), (42, 46)), ("Date",)),
        ("payee", None, S.v(("Payee",), ("Other party",))),
        ("part", S.r((72, 76), (68, 72)), ("Particulars",)),
        ("code", S.r((64, 68), (66, 70)), ("Code",)),
        ("ref", S.r((68, 72), (70, 74)), ("Reference",)),
        ("debit", S.r((70, 74), (66, 70)), S.v(("Payments",), ("Withdrawals",))),
        ("credit", S.r((70, 74), (66, 70)), S.v(("Receipts",), ("Deposits",))),
        ("balance", S.r((76, 80), (74, 78)), ("Balance",)),
    ])
    sx = right + S.r((22, 26), (28, 32))
    return layout(
        S, "kahikatea_land", bank="Kahikatea Bank", product="Business Cheque",
        note="Landscape business statement: Payee plus Particulars / Code / Reference, "
             "Payments and Receipts, and a sidebar beside the table on page 1.",
        page="A4L", font="Helvetica", size=size, cols=cols, date_fmt=S.v("dd/mm/yyyy", "dd/mm/yy"),
        period_fmt="long", catalog="biz_pcr", customers="biz", scale=0.8,
        col_rules=S.v(True, False), head_style="box", trailer=["totals_close"],
        totals_label="Closing totals", opening=dict(label="Opening balance"),
        rate_text="5.10% p.a.",
        hdr=dict(mast="bar", color=TEAL, title="Business account statement", left=left,
                 right=842 - left, addr=(left, 92), det=(S.r((330, 340), (360, 370)), 92, 90),
                 det_rows=["name", "acct", "period", "stno"],
                 sidebar=dict(style="box", x=sx, y=S.v(176, 184), w=842 - left - sx,
                              title="Your banker", size=7,
                              rows=[("Overdraft rate", "rate"), ("Fees", "fees"),
                                    ("Transactions", "count")],
                              notes=["Business line 0800 000 001", "Mon-Fri 8am-6pm"])),
        table_top1=S.v(176, 184), table_top2=S.v(74, 80),
        footer=dict(pos="right", other="Kahikatea Bank Limited  |  {branch}",
                    small=small_print("Kahikatea Bank")[1:]),
        stmts=[dict(n=S.n(30, 44), ym=S.v((2026, 6), (2026, 3)))] +
              S.v([], [dict(n=S.n(50, 58), ym=(2026, 4))]))


@design()
def kauri_homeloan(S):
    size = S.v(9, 8.5)
    left, right = S.r((40, 44), (48, 52)), S.r((552, 556), (544, 548))
    cols = mkcols(left, right, [
        ("date", S.r((88, 92), (64, 68)), ("Date",)),
        ("desc", None, ("Transaction details",)),
        ("debit", S.r((72, 78), (76, 82)), ("Debit",)),
        ("credit", S.r((72, 78), (76, 82)), ("Credit",)),
        ("balance", S.r((92, 98), (96, 102)), ("Balance",)),
    ])
    return layout(
        S, "kauri_homeloan", bank="Kauri Bank", product="Home Loan - Floating",
        note="Loan account: the balance is owed throughout and printed with a DR token; "
             "repayments are credits, interest is a debit; large figures over a quarter.",
        font="Helvetica", size=size, cols=cols, date_fmt=S.v("d Month yyyy", "dd Mon yyyy"),
        period_fmt="long", catalog="loan", credits="loan", balance_kind="loan",
        balance_style="dr_only", opening=dict(label="Opening balance"), trailer=["close"],
        head_style="bar", rate_text=S.v("6.24% p.a.", "5.99% p.a."),
        hdr=dict(mast="bar", color=NAVY, title="Home loan statement", left=left, right=right,
                 addr=(left, 100), det=(S.r((320, 330), (300, 310)), 96, 84),
                 det_rows=["acct", "period", "issued"],
                 summary=dict(style="box", x=S.r((320, 330), (300, 310)), y=146,
                              w=S.r((200, 208), (226, 236)), title="Loan summary",
                              rows=[("Opening balance", "open"), ("Interest and fees", "tot_d"),
                                    ("Repayments", "tot_c"), ("Closing balance", "close"),
                                    ("Interest rate", "rate")])),
        table_top1=S.v(266, 272), table_top2=84,
        footer=dict(pos="right", small=small_print("Kauri Bank")),
        stmts=[dict(n=S.n(9, 14), ym=S.v((2026, 1), (2026, 4)), months=3)] +
              S.v([dict(n=S.n(8, 10), ym=(2025, 10), months=3)], []))


@design()
def southern_saver(S):
    font, size = S.v(("Times", 10), ("Helvetica", 9.5))
    left, right = S.r((56, 60), (46, 50)), S.r((540, 544), (550, 554))
    cols = mkcols(left, right, [
        ("date", S.r((50, 54), (54, 58)), ("Date",)),
        ("desc", None, ("Details",)),
        ("debit", S.r((70, 76), (76, 80)), ("Withdrawals",)),
        ("credit", S.r((64, 70), (64, 68)), ("Deposits",)),
        ("balance", S.r((74, 80), (78, 84)), ("Balance",)),
    ])
    return layout(
        S, "southern_saver", bank="Southern Cross Bank", product="Online Saver",
        note="Savings account where the Deposits column is nearly empty: one or two "
             "deposits, none on page 1 in the first statement.",
        font=font, size=size, cols=cols, date_fmt=S.v("dd Mon", "dd MON"), period_fmt="short",
        catalog="savings", credits="sparse", opening=dict(label="Opening balance"),
        trailer=["close"], max_rows=S.v(15, 13), head_style="rule2",
        rate_text=S.v("3.10% p.a.", "2.85% p.a."), open_range=(4000, 20000),
        hdr=dict(mast="logo", color=(0.0, 0.25, 0.55), title="Savings statement",
                 left=left, right=right, addr=(left, 96),
                 det=(S.r((320, 330), (300, 310)), 96, 84), det_rows=["acct", "period"],
                 sidebar=dict(style="shaded", x=S.r((320, 330), (300, 310)), y=136,
                              w=S.r((190, 200), (220, 230)),
                              rows=[("Interest rate", "rate"), ("Fees this period", "fees")])),
        table_top1=S.v(216, 210), table_top2=86,
        footer=dict(pos="left", fmt="Page {p} of {n}", other="Online Saver"),
        stmts=[dict(n=S.n(18, 24), ym=S.v((2026, 2), (2026, 3)), credits="sparse_none_p1"),
               dict(n=S.n(10, 14), ym=S.v((2026, 8), (2026, 9)))])


@design()
def harbour_centred(S):
    size = S.v(9, 8.5)
    left, right = S.r((40, 44), (48, 52)), S.r((554, 558), (546, 550))
    cols = mkcols(left, right, [
        ("date", S.r((62, 66), (60, 64)), ("Date",), None, "center"),
        ("desc", None, ("Description",)),
        ("debit", S.r((70, 76), (74, 80)), ("Debit",), "center"),
        ("credit", S.r((70, 76), (74, 80)), ("Credit",), "center"),
        ("balance", S.r((80, 86), (82, 88)), ("Balance",), "center"),
    ])
    return layout(
        S, "harbour_centred", bank="Harbour Bank", product="Orbit Account",
        note="Money centred in its columns rather than right-aligned; long descriptions.",
        font="Helvetica", size=size, cols=cols, date_fmt=S.v("d Mon yyyy", "dd/mm/yyyy"),
        period_fmt="short", catalog="bank", long_desc=0.3, head_style="dark",
        head_fill=(0.2, 0.2, 0.2), opening=dict(label="Opening balance"), trailer=["close"],
        hdr=dict(mast="bar", color=YELLOW, ink=BLACK, title="Account statement", left=left,
                 right=right, addr=(left, 98), det=(S.r((316, 324), (300, 306)), 98, 84),
                 det_rows=["acct", "period", "stno"],
                 summary=dict(style="band", x=left, y=158, w=right - left,
                              rows=[("Opening", "open"), ("Debits", "tot_d"),
                                    ("Credits", "tot_c"), ("Closing", "close")])),
        table_top1=226, table_top2=84,
        footer=dict(pos="right", small=small_print("Harbour Bank")),
        stmts=[dict(n=S.n(24, 34), ym=S.v((2026, 10), (2026, 11)))])


@design()
def pounamu_card(S):
    font, size = S.v(("Helvetica", 7), ("Times", 8))
    left, right = S.r((36, 40), (46, 50)), S.r((556, 560), (548, 552))
    cols = mkcols(left, right, [
        ("date", S.r((48, 52), (46, 50)), ("Date",)),
        ("pdate", S.r((50, 54), (48, 52)), ("Processed",)),
        ("desc", None, ("Details",)),
        ("amount", S.r((60, 66), (66, 72)), ("Amount",)),
    ])
    return layout(
        S, "pounamu_card", bank="Pounamu Card Services", product="Mastercard Low Rate",
        note="Credit card where payments and refunds print with a leading minus; "
             "processed date column; heading on every page.",
        font=font, size=size, cols=cols, date_fmt=S.v("dd/mm/yyyy", "dd-mm-yy"),
        period_fmt="num", catalog="card", credits="card", balance_kind="card",
        bal_mode="none", amount_style="card_minus", wrap=0.25, trailer=["close"],
        closing_label="New balance", head_style="rule", card=dict(limit=S.ch([300000], [450000])),
        rate_text="13.95% p.a.",
        hdr=dict(mast="right", title="Mastercard statement", left=left, right=right,
                 addr=(left, 96), det=(left, 160, 76), det_rows=["card", "period"],
                 summary=dict(style="box", x=S.r((330, 340), (310, 320)), y=90,
                              w=S.r((200, 210), (226, 236)), title="Statement summary",
                              rows=[("Previous balance", "open"), ("Purchases", "tot_d"),
                                    ("Payments and credits", "tot_c"), ("New balance", "close"),
                                    ("Minimum payment", "minpay"), ("Payment due", "due")])),
        table_top1=S.v(230, 236), table_top2=S.v(80, 76), max_rows=S.v(None, 22),
        footer=dict(pos="center", small=small_print("Pounamu Card Services")),
        stmts=[dict(n=S.n(40, 56), ym=S.v((2026, 5, 3), (2026, 7, 21)))])


@design()
def tasman_od(S):
    size = S.v(8.5, 8)
    left, right = S.r((44, 48), (34, 38)), S.r((550, 554), (558, 562))
    cols = mkcols(left, right, [
        ("date", S.r((40, 44), (34, 38)), ("Date",)),
        ("desc", None, ("Particulars",)),
        ("debit", S.r((64, 70), (62, 66)), ("Withdrawals",)),
        ("credit", S.r((64, 70), (62, 66)), ("Deposits",)),
        ("balance", S.r((78, 84), (82, 88)), ("Balance",)),
    ])
    return layout(
        S, "tasman_od", bank="Tasman Banking Corporation", product="Overdraft Account",
        note="Overdrawn balances carry an OD token after the figure.",
        font="Helvetica", size=size, cols=cols, date_fmt=S.v("dd Mon", "d Mon"),
        period_fmt="long", catalog="bank", wrap=0.2, balance_style="od",
        hang=S.v(("balance",), ()), balance_kind="overdraft", opening=dict(label="Opening balance"),
        trailer=["close"], head_style="rule",
        hdr=dict(mast="plain", title="Statement", left=left, right=right, addr=(left, 100),
                 det=(S.r((316, 324), (296, 304)), 100, 84), det_rows=["acct", "period", "stno"],
                 summary=dict(style="box", x=S.r((316, 324), (296, 304)), y=146,
                              w=S.r((204, 210), (232, 240)), title="Summary",
                              rows=[("Opening balance", "open"), ("Closing balance", "close"),
                                    ("Overdraft limit", "odlimit")])),
        table_top1=232, table_top2=86,
        footer=dict(pos="right", other="Tasman Banking Corporation"),
        stmts=[dict(n=S.n(20, 30), ym=S.v((2026, 6), (2026, 1)))])


@design()
def fern_duplex(S):
    size = S.v(8, 8.5)
    left, right = S.r((40, 44), (44, 48)), S.r((546, 550), (540, 544))
    cols = mkcols(left, right, [
        ("date", S.r((38, 42), (40, 44)), ("Date",)),
        ("desc", None, ("Details",)),
        ("debit", S.r((66, 70), (68, 72)), ("Withdrawals",)),
        ("credit", S.r((62, 66), (64, 68)), ("Deposits",)),
        ("balance", S.r((72, 78), (74, 80)), ("Balance",)),
    ])
    return layout(
        S, "fern_duplex", bank="Fern Bank", product="Everyday Account",
        note="Printed for duplex binding: even pages shifted right by a gutter, so the table "
             "sits at a different x on alternate pages; balance on the last row of each day.",
        font="Helvetica", size=size, cols=cols, date_fmt=S.v("dd Mon", "dd Mon"),
        period_fmt="long", catalog="bank", wrap=0.25, bal_mode="last_of_day",
        page_dx=("gutter", S.v(7.0, 5.0)), max_rows=S.v(16, 18), opening=dict(label="Opening balance"),
        trailer=["close"], head_style="rule",
        hdr=dict(mast="logo", color=GREEN, title="Statement", left=left, right=right,
                 addr=(left, 96), det=(S.r((316, 324), (300, 306)), 96, 80),
                 det_rows=["acct", "period"],
                 summary=dict(style="box", x=S.r((316, 324), (300, 306)), y=126,
                              w=S.r((196, 204), (210, 218)), title="Summary",
                              rows=[("Opening balance", "open"), ("Withdrawals", "tot_d"),
                                    ("Deposits", "tot_c"), ("Closing balance", "close")])),
        table_top1=226, table_top2=80,
        footer=dict(pos=S.v("right", "center"), small=small_print("Fern Bank")[1:]),
        stmts=[dict(n=S.n(52, 60), ym=S.v((2026, 3), (2026, 10)))])


@design()
def rata_jitter(S):
    font, size = S.v(("Times", 9.5), ("Helvetica", 9))
    left, right = S.r((48, 52), (40, 44)), S.r((546, 550), (550, 554))
    cols = mkcols(left, right, [
        ("date", S.r((48, 52), (60, 64)), ("Date",)),
        ("desc", None, ("Particulars",)),
        ("debit", S.r((66, 72), (70, 76)), S.v(("Debits",), ("Withdrawals",))),
        ("credit", S.r((66, 72), (66, 72)), S.v(("Credits",), ("Lodgements",))),
        ("balance", S.r((74, 80), (76, 82)), ("Balance",)),
    ])
    return layout(
        S, "rata_jitter", bank="Rata Savings Bank", product="Cheque Account",
        note="Scanned-and-reprinted feel: each page's table lands a few points left or right "
             "of the last; continuation pages are short.",
        font=font, size=size, cols=cols, date_fmt=S.v("dd-mm-yy", "dd-Mon-yy"),
        period_fmt="short", catalog="bank", wrap=0.1, page_dx=("jitter", 4.0),
        max_rows=S.v(18, 15), opening=dict(label="Opening balance"), trailer=["close"],
        head_style="rule",
        hdr=dict(mast="right", title="Statement", left=left, right=right, addr=(left, 92),
                 det=(left, 150, 90), det_rows=["acct", "period"], cont="line"),
        table_top1=S.v(210, 204), table_top2=86,
        footer=dict(pos="center"),
        stmts=[dict(n=S.n(36, 44), ym=S.v((2026, 4), (2026, 5)), short_last=True)])


@design()
def kea_biz_type(S):
    font, size = S.v(("Helvetica", 8), ("Times", 8.5))
    left, right = S.r((34, 38), (42, 46)), S.r((558, 562), (550, 554))
    cols = mkcols(left, right, [
        ("date", S.r((48, 52), (42, 46)), ("Date",)),
        ("type", S.r((36, 40), (36, 40)), ("Type",)),
        ("desc", None, ("Details",)),
        ("debit", S.r((66, 72), (68, 74)), ("Debit",)),
        ("credit", S.r((66, 72), (68, 74)), ("Credit",)),
        ("balance", S.r((74, 80), (78, 84)), ("Balance",)),
    ])
    return layout(
        S, "kea_biz_type", bank="Kea Business Bank", product="Business Transaction Account",
        note="Busy business account over several pages: type column plus separate Debit / "
             "Credit, $ figures with thousands separators.",
        font=font, size=size, cols=cols, date_fmt=S.v("dd/mm/yyyy", "dd/mm/yy"),
        period_fmt="num", catalog="typed_biz", customers="biz", dollar=True, scale=S.v(1.0, 0.7),
        open_range=(8000, 60000), max_rows=S.v(22, 20), head_style="bar",
        head_fill=(0.88, 0.9, 0.95), opening=dict(label="Opening balance"),
        trailer=["totals", "close"], totals_label="Period totals",
        page_dx=S.v(None, ("jitter", 2.5)),
        hdr=dict(mast="logo", color=(0.8, 0.35, 0.0), title="Business statement", left=left,
                 right=right, addr=(left, 94), addr_bold=True,
                 det=(S.r((330, 336), (310, 316)), 94, 80),
                 det_rows=["acct", "period", "stno", "branch"],
                 summary=dict(style="band", x=left, y=158, w=right - left, size=8,
                              rows=[("Opening balance", "open"), ("Total debits", "tot_d"),
                                    ("Total credits", "tot_c"), ("Closing balance", "close")])),
        table_top1=226, table_top2=S.v(80, 86),
        footer=dict(pos="right", other="Kea Business Bank  -  {branch}",
                    small=small_print("Kea Business Bank")),
        stmts=[dict(n=S.n(54, 60), ym=S.v((2026, 8), (2026, 6)))])


@design()
def coastal_cu(S):
    size = S.v(8, 7)
    left, right = S.r((40, 44), (32, 36)), S.r((552, 556), (560, 564))
    cols = mkcols(left, right, [
        ("date", S.r((52, 56), (48, 52)), ("DATE",)),
        ("desc", None, ("DETAILS",)),
        ("debit", S.r((80, 86), (76, 82)), ("WITHDRAWALS",)),
        ("credit", S.r((70, 76), (66, 72)), ("DEPOSITS",)),
        ("balance", S.r((76, 82), (72, 78)), ("BALANCE",)),
    ])
    return layout(
        S, "coastal_cu", bank="Coastal Credit Union", product="Everyday Access",
        note="Monospaced credit-union print with vertical column rules, no thousands "
             "separators, headings on page 1 only.",
        font="Courier", size=size, pitch=round(size * 1.6, 2), cols=cols,
        date_fmt=S.v("dd-mm-yy", "dd/mm/yy"), period_fmt="upper", period_sep=" TO ",
        period_label="PERIOD", catalog="bank", thousands=False, wrap=0.15,
        head_every_page=False, head_style="dashes", col_rules=True,
        opening=dict(label="OPENING BALANCE"), trailer=["close"], closing_label="CLOSING BALANCE",
        hdr=dict(mast="courier", title="Member statement", left=left, right=right,
                 addr=(left, 88), addr_size=8, det=(S.r((320, 330), (334, 340)), 88, 72),
                 det_size=8, det_rows=["acct", "period", "branch"], cont="courier"),
        table_top1=162, table_top2=78,
        footer=dict(pos="right", fmt="PAGE {p}"),
        stmts=[dict(n=S.n(30, 40), ym=S.v((2026, 1), (2026, 2)))] +
              S.v([dict(n=S.n(12, 16), ym=(2026, 2))], []))


@design()
def aotea_notice(S):
    size = S.v(9, 10)
    left, right = S.r((54, 58), (44, 48)), S.r((540, 544), (548, 552))
    cols = mkcols(left, right, [
        ("date", S.r((96, 100), (66, 70)), ("Date",)),
        ("desc", None, ("Transaction",)),
        ("debit", S.r((68, 74), (72, 78)), S.v(("Payments",), ("Withdrawals",))),
        ("credit", S.r((68, 74), (66, 72)), S.v(("Receipts",), ("Deposits",))),
        ("balance", S.r((76, 82), (80, 86)), ("Balance",)),
    ])
    return layout(
        S, "aotea_notice", bank="Aotea Building Society", product="32 Day Notice Saver",
        note="Quiet notice-saver account: few transactions, a two-page statement whose "
             "second page holds one or two rows.",
        font="Times", size=size, cols=cols, date_fmt=S.v("d Month yyyy", "d Mon yyyy"),
        period_fmt="long", catalog="savings", credits="sparse", max_rows=S.v(9, 7),
        opening=dict(label="Opening balance", date=True), trailer=["close"], head_style="rule2",
        rate_text=S.v("4.15% p.a.", "3.95% p.a."), open_range=(6000, 40000),
        hdr=dict(mast="plain", title="Statement", tagline="Aotea Building Society - members first",
                 left=left, right=right, addr=(left, 104), addr_size=10,
                 det=(S.r((316, 322), (300, 306)), 104, 84), det_size=9,
                 det_rows=["acct", "period"],
                 summary=dict(style="box", x=S.r((316, 322), (300, 306)), y=140,
                              w=S.r((200, 206), (236, 244)), size=9, title="Your savings",
                              rows=[("Opening balance", "open"), ("Closing balance", "close"),
                                    ("Interest rate", "rate")])),
        table_top1=244, table_top2=90,
        footer=dict(pos="center", small=small_print("Aotea Building Society")),
        stmts=[dict(n=S.n(8, 12), ym=S.v((2026, 1), (2026, 4)), months=3, short_last=True)])


@design()
def kauri_bizcard(S):
    size = S.v(8, 7.5)
    left, right = S.r((40, 44), (48, 52)), S.r((552, 556), (544, 548))
    cols = mkcols(left, right, [
        ("date", S.r((40, 44), (34, 38)), ("Date",)),
        ("pdate", S.r((48, 52), (44, 48)), ("Processed",)),
        ("desc", None, ("Transaction details",)),
        ("amount", S.r((72, 78), (76, 82)), ("Amount",)),
    ])
    return layout(
        S, "kauri_bizcard", bank="Kauri Bank", product="Business Visa",
        note="Business credit card with a section per cardholder: a cardholder heading and a "
             "card sub-total line inside the table (neither is a transaction).",
        font="Helvetica", size=size, cols=cols, date_fmt=S.v("dd Mon", "d Mon"),
        period_fmt="short", catalog="card", credits="card", balance_kind="card",
        bal_mode="none", amount_style="card_cr", hang=S.v(("amount",), ()), wrap=0.2,
        customers="biz", sections=["J SAMPLE  {card}", "K EXAMPLE  {card}"],
        trailer=["close"], closing_label="Closing balance", head_style="bar",
        card=dict(limit=2000000), rate_text="18.95% p.a.",
        hdr=dict(mast="bar", color=NAVY, title="Business card statement", left=left,
                 right=right, addr=(left, 100), addr_bold=True,
                 det=(S.r((316, 324), (300, 306)), 100, 78), det_rows=["card", "period", "issued"],
                 summary=dict(style="box", x=S.r((316, 324), (300, 306)), y=144,
                              w=S.r((200, 208), (226, 236)), title="Account summary",
                              rows=[("Opening balance", "open"), ("Purchases", "tot_d"),
                                    ("Payments", "tot_c"), ("Closing balance", "close"),
                                    ("Credit limit", "limit")])),
        table_top1=S.v(260, 254), table_top2=84,
        footer=dict(pos="right", small=small_print("Kauri Bank")),
        stmts=[dict(n=S.n(30, 40), ym=S.v((2026, 7, 5), (2026, 9, 5)))])


@design()
def matai_fees(S):
    size = S.v(8.5, 8)
    left, right = S.r((44, 48), (36, 40)), S.r((548, 552), (556, 560))
    cols = mkcols(left, right, [
        ("date", S.r((62, 66), (58, 62)), ("Date",)),
        ("desc", None, ("Transaction details",)),
        ("debit", S.r((66, 72), (70, 76)), ("Withdrawals",)),
        ("credit", S.r((66, 72), (62, 68)), ("Deposits",)),
        ("balance", S.r((72, 78), (76, 82)), ("Balance",)),
    ])
    return layout(
        S, "matai_fees", bank="Matai Bank", product="Everyday Account",
        note="Fees-and-interest summary block printed under the table (figures that are "
             "not transactions), weekday dates with no year.",
        font="Helvetica", size=size, cols=cols, date_fmt="Dow dd Mon",
        period_fmt="long", catalog="bank", wrap=0.2, opening=dict(label="Opening balance"),
        trailer=["close"], head_style="rule", row_rules=S.v(False, True),
        after=dict(title="Fees and interest this period", x=S.r((60, 70), (300, 310)),
                   w=S.r((200, 210), (190, 200)),
                   rows=[("Account fees", "fees"), ("Credit interest", "interest"),
                         ("Transactions", "count")]),
        hdr=dict(mast="plain", title="Account statement", left=left, right=right,
                 addr=(left, 98), det=(S.r((320, 330), (300, 310)), 98, 84),
                 det_rows=["acct", "period", "stno"],
                 summary=dict(style="lines", x=S.r((320, 330), (300, 310)), y=134,
                              w=S.r((190, 200), (232, 240)),
                              rows=[("Opening balance", "open"), ("Closing balance", "close")])),
        table_top1=214, table_top2=82,
        footer=dict(pos="left", other="Matai Bank Limited", small=small_print("Matai Bank")[:1]),
        stmts=[dict(n=S.n(26, 36), ym=S.v((2026, 11), (2026, 12)))] +
              S.v([], [dict(n=S.n(44, 50), ym=(2027, 1))]))


# ---- designs only dev sees ------------------------------------------------

@design("dev")
def rimu_reference(S):
    size = 9
    left, right = 42, 554
    cols = mkcols(left, right, [
        ("date", 92, ("Date",)),
        ("desc", None, ("Details",)),
        ("ref", 74, ("Reference",)),
        ("debit", 64, ("Withdrawals",)),
        ("credit", 60, ("Deposits",)),
        ("balance", 74, ("Balance",)),
    ])
    return layout(
        S, "rimu_reference", bank="Rimu Building Society", product="Everyday Account",
        note="A separate Reference text column between the details and the money.",
        font="Times", size=size, cols=cols, date_fmt="d Month yyyy", period_fmt="long",
        catalog="bank", opening=dict(label="Opening balance"), trailer=["close"],
        head_style="rule",
        hdr=dict(mast="plain", title="Statement", left=left, right=right, addr=(left, 100),
                 det=(318, 100, 84), det_rows=["acct", "period", "stno"]),
        table_top1=186, table_top2=84,
        footer=dict(pos="center", small=small_print("Rimu Building Society")),
        stmts=[dict(n=24, ym=(2026, 2))])


@design("dev")
def westgate_trail(S):
    size = 9
    left, right = 46, 550
    cols = mkcols(left, right, [
        ("date", 52, ("Date",)),
        ("desc", None, ("Description",)),
        ("amount", 78, ("Amount",)),
        ("balance", 80, ("Balance",)),
    ])
    return layout(
        S, "westgate_trail", bank="Westgate Bank", product="Flexi Account",
        note="Signed amounts with a trailing minus in a proportional font; balance "
             "brought / carried forward lines.",
        font="Helvetica", size=size, cols=cols, date_fmt="dd MON yy", period_fmt="short",
        catalog="bank", long_desc=0.3, amount_style="trail", balance_style="trail",
        cf_bf=("Carried forward", "Brought forward"), opening=dict(label="Brought forward"),
        trailer=["close"], max_rows=22, head_style="bar",
        hdr=dict(mast="bar", color=MAROON, title="Statement", left=left, right=right,
                 addr=(left, 96), det=(320, 96, 80), det_rows=["acct", "period"],
                 summary=dict(style="box", x=320, y=132, w=200, title="Summary",
                              rows=[("Opening balance", "open"), ("Closing balance", "close")])),
        table_top1=214, table_top2=80,
        footer=dict(pos="right"),
        stmts=[dict(n=40, ym=(2026, 5)), dict(n=16, ym=(2026, 6))])


@design("dev")
def tui_youth(S):
    size = 10
    left, right = 50, 546
    cols = mkcols(left, right, [
        ("date", 82, ("Date",)),
        ("desc", None, ("What for",)),
        ("debit", 70, ("Money out",)),
        ("credit", 70, ("Money in",)),
        ("balance", 76, ("Balance",)),
    ])
    return layout(
        S, "tui_youth", bank="Tui Bank", product="Youth Account",
        note="Large type, few rows, $ figures, a single page with a sidebar of tips.",
        font="Helvetica", size=size, cols=cols, date_fmt="Dow dd Mon", period_fmt="long",
        catalog="bank", dollar=True, thousands=True, credit_p=0.35, zebra=True,
        trailer=["close"], head_style="bar", head_fill=(0.85, 0.9, 1.0),
        rate_text="1.00% p.a.", open_range=(50, 600),
        hdr=dict(mast="logo", color=(0.2, 0.4, 0.8), title="Your statement", left=left,
                 right=right, addr=(left, 96), det=(left, 160, 96), det_rows=["acct", "period"],
                 sidebar=dict(style="shaded", x=330, y=90, w=190, title="Good to know",
                              rows=[("Interest rate", "rate"), ("Fees this period", "fees")])),
        table_top1=226, table_top2=84,
        footer=dict(pos="center", small=small_print("Tui Bank")),
        stmts=[dict(n=12, ym=(2026, 4))])


@design("dev")
def harbour_biz_receipts(S):
    size = 7.5
    left, right = 36, 560
    cols = mkcols(left, right, [
        ("date", 46, ("Date",)),
        ("payee", None, ("Payee / payer",)),
        ("part", 66, ("Particulars",)),
        ("code", 56, ("Code",)),
        ("ref", 62, ("Reference",)),
        ("debit", 62, ("Payments",)),
        ("credit", 62, ("Receipts",)),
        ("balance", 70, ("Balance",)),
    ])
    return layout(
        S, "harbour_biz_receipts", bank="Harbour Bank", product="Business Current Account",
        note="Portrait business statement with Payee plus Particulars / Code / Reference.",
        font="Helvetica", size=size, cols=cols, date_fmt="dd/mm/yyyy", period_fmt="num",
        period_sep=" - ", catalog="biz_pcr", customers="biz", scale=0.5, zebra=True,
        head_style="dark", head_fill=(0.15, 0.15, 0.15), opening=dict(label="Opening balance"),
        trailer=["totals_close"], totals_label="Totals",
        hdr=dict(mast="bar", color=YELLOW, ink=BLACK, title="Business statement", left=left,
                 right=right, addr=(left, 96), det=(320, 96, 80),
                 det_rows=["name", "acct", "period", "branch"]),
        table_top1=176, table_top2=80,
        footer=dict(pos="right", other="Branch {branch}"),
        stmts=[dict(n=46, ym=(2026, 9))])


@design("dev")
def pateke_paren_dollar(S):
    size = 8.5
    left, right = 40, 552
    cols = mkcols(left, right, [
        ("date", 50, ("Date",)),
        ("desc", None, ("Transaction",)),
        ("amount", 80, ("Amount",)),
        ("balance", 84, ("Balance",)),
    ])
    return layout(
        S, "pateke_paren_dollar", bank="Pateke Bank", product="Smart Saver",
        note="Parenthesised money out with a $ prefix, account summary box.",
        font="Helvetica", size=size, cols=cols, date_fmt="dd-mm-yy", period_fmt="short",
        catalog="bank", dollar=True, amount_style="paren", balance_style="paren",
        wrap=0.25, wrap_indent=10, opening=dict(label="Opening balance"), trailer=["close"],
        head_style="rule",
        hdr=dict(mast="right", title="Statement", left=left, right=right, addr=(left, 96),
                 det=(left, 156, 90), det_rows=["acct", "period", "stno"],
                 summary=dict(style="box", x=330, y=96, w=200, title="Account summary",
                              rows=[("Opening balance", "open"), ("Money out", "tot_d"),
                                    ("Money in", "tot_c"), ("Closing balance", "close")])),
        table_top1=226, table_top2=80,
        footer=dict(pos="left"),
        stmts=[dict(n=28, ym=(2026, 10)), dict(n=50, ym=(2026, 11))])


# ---- designs only holdout sees ------------------------------------------------

@design("holdout")
def pohutukawa_value(S):
    size = 8
    left, right = 38, 558
    cols = mkcols(left, right, [
        ("date", 48, ("Date",)),
        ("pdate", 48, ("Value", "date")),
        ("desc", None, ("Details",)),
        ("debit", 66, ("Debit",)),
        ("credit", 66, ("Credit",)),
        ("balance", 78, ("Balance",)),
    ])
    return layout(
        S, "pohutukawa_value", bank="Pohutukawa Bank", product="Foreign Currency Call",
        note="Transaction date and value date side by side; truth date is the first.",
        font="Helvetica", size=size, cols=cols, date_fmt="dd-Mon-yy", period_fmt="short",
        catalog="bank", balance_style="drcr", opening=dict(label="Opening balance"),
        trailer=["close"], head_style="rule2",
        hdr=dict(mast="bar", color=MAROON, title="Account statement", left=left, right=right,
                 addr=(left, 98), det=(310, 98, 84), det_rows=["acct", "period", "stno"],
                 summary=dict(style="box", x=310, y=146, w=220, title="Summary",
                              rows=[("Opening balance", "open"), ("Debits", "tot_d"),
                                    ("Credits", "tot_c"), ("Closing balance", "close")])),
        table_top1=252, table_top2=84,
        footer=dict(pos="right", small=small_print("Pohutukawa Bank")),
        stmts=[dict(n=26, ym=(2026, 3))])


@design("holdout")
def kowhai_plus(S):
    size = 9
    left, right = 50, 546
    cols = mkcols(left, right, [
        ("date", 76, ("Date",)),
        ("desc", None, ("Description",)),
        ("amount", 84, ("Amount",)),
        ("balance", 84, ("Balance",)),
    ])
    return layout(
        S, "kowhai_plus", bank="Kowhai Digital Bank", product="Spend Account",
        note="App-bank print: explicit + and - on every amount, $ figures, no rules.",
        font="Helvetica", size=size, pitch=17, cols=cols, date_fmt="dd Mon yyyy",
        period_fmt="long", catalog="bank", dollar=True, amount_style="plus",
        balance_style="lead", wrap=0.2, trailer=["close"], head_style="none",
        hdr=dict(mast="logo", color=(0.85, 0.65, 0.0), title="Monthly statement", left=left,
                 right=right, addr=(left, 96), det=(left, 156, 96), det_rows=["acct", "period"],
                 summary=dict(style="shaded", x=330, y=90, w=190, title="This month",
                              rows=[("Started with", "open"), ("Spent", "tot_d"),
                                    ("Received", "tot_c"), ("Ended with", "close")])),
        table_top1=232, table_top2=72,
        footer=dict(pos="center", fmt="{p}/{n}"),
        stmts=[dict(n=34, ym=(2026, 8)), dict(n=14, ym=(2026, 9))])


@design("holdout")
def kotare_land_type(S):
    size = 8.5
    left, right = 48, 720
    cols = mkcols(left, right, [
        ("date", 64, ("DATE",)),
        ("type", 56, ("TYPE",)),
        ("desc", None, ("TRANSACTION DETAILS",)),
        ("amount", 96, ("AMOUNT",)),
        ("balance", 104, ("BALANCE",)),
    ])
    return layout(
        S, "kotare_land_type", bank="Kotare Bank", product="Business Online",
        note="Landscape page, upper-case headings, type column, signed amount with trailing "
             "minus, balance with OD.",
        page="A4L", font="Helvetica", size=size, cols=cols, date_fmt="dd MON yy",
        period_fmt="upper", catalog="typed_biz", customers="biz", scale=0.5,
        amount_style="trail", balance_style="od", balance_kind="overdraft",
        opening=dict(label="OPENING BALANCE"), trailer=["close"], closing_label="CLOSING BALANCE",
        head_style="bar", max_rows=20,
        hdr=dict(mast="bar", color=TEAL, title="Business statement", left=left, right=794,
                 addr=(left, 92), det=(420, 92, 90), det_rows=["name", "acct", "period"]),
        table_top1=168, table_top2=80,
        footer=dict(pos="right", other="Kotare Bank Ltd"),
        stmts=[dict(n=44, ym=(2026, 2))])


@design("holdout")
def weka_pcr(S):
    size = 8
    left, right = 44, 552
    cols = mkcols(left, right, [
        ("date", 52, ("Date",)),
        ("part", None, ("Particulars",)),
        ("code", 64, ("Code",)),
        ("ref", 70, ("Reference",)),
        ("debit", 64, ("Payments", "$")),
        ("credit", 64, ("Receipts", "$")),
        ("balance", 72, ("Balance", "$")),
    ])
    return layout(
        S, "weka_pcr", bank="Weka Bank", product="Personal Cheque",
        note="Particulars / Code / Reference with two-line money headings; balance with OD.",
        font="Times", size=size, cols=cols, date_fmt="d/mm/yyyy", period_fmt="long",
        catalog="pcr", balance_style="od", hang=("balance",), balance_kind="overdraft",
        opening=dict(label="Opening balance"), trailer=["close"], head_style="rule",
        head_every_page=False,
        hdr=dict(mast="right", title="Statement", left=left, right=right, addr=(left, 92),
                 det=(left, 150, 90), det_rows=["acct", "period", "stno"],
                 summary=dict(style="box", x=330, y=88, w=200, title="Summary",
                              rows=[("Opening balance", "open"), ("Payments", "tot_d"),
                                    ("Receipts", "tot_c"), ("Closing balance", "close")])),
        table_top1=228, table_top2=72,
        footer=dict(pos="center", small=small_print("Weka Bank")),
        stmts=[dict(n=38, ym=(2026, 6))])


@design("holdout")
def moa_card_letter(S):
    size = 8.5
    left, right = 44, 568
    cols = mkcols(left, right, [
        ("date", 92, ("Trans date",)),
        ("pdate", 92, ("Posted",)),
        ("desc", None, ("Description",)),
        ("amount", 84, ("Amount",)),
    ])
    return layout(
        S, "moa_card_letter", bank="Moa Card Company", product="Rewards Visa",
        note="US Letter credit card: full month dates, payments marked CR in a token slot.",
        page="Letter", font="Times", size=size, cols=cols, date_fmt="d Month yyyy",
        period_fmt="long", catalog="card", credits="card", balance_kind="card",
        bal_mode="none", amount_style="card_cr", hang=("amount",), wrap=0.3,
        opening=dict(label="Previous balance"), trailer=["close"], closing_label="New balance",
        head_style="rule", card=dict(limit=700000), rate_text="22.95% p.a.",
        hdr=dict(mast="plain", title="Card statement", left=left, right=right, addr=(left, 98),
                 det=(330, 98, 76), det_rows=["card", "period"],
                 sidebar=dict(style="box", x=330, y=132, w=220, title="Pay by",
                              rows=[("Payment due", "due"), ("Minimum payment", "minpay"),
                                    ("Credit limit", "limit")])),
        table_top1=232, table_top2=84,
        footer=dict(pos="right", small=small_print("Moa Card Company")),
        stmts=[dict(n=30, ym=(2026, 5, 11))])


# ---------------------------------------------------------------------------

def designs_for(split):
    out = []
    for name, splits, fn in ARCH:
        if splits in ("both", split):
            L = fn(Split(split, name))
            if L["id"] != name:
                raise GenError("design %s returned id %s" % (name, L["id"]))
            out.append(L)
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--out", help="where to write the PDFs and truth files")
    ap.add_argument("--split", choices=sorted(SPLIT_SEED), default="dev")
    ap.add_argument("--only", default=None,
                    help="build just the cases whose name contains this")
    ap.add_argument("--list", action="store_true")
    a = ap.parse_args()

    try:
        Ls = designs_for(a.split)
    except GenError as e:
        sys.exit("GENERATOR SELF-CHECK FAILED: %s" % e)
    if a.list:
        for L in Ls:
            for k, _ in enumerate(L["stmts"]):
                name = "%s_%d" % (L["id"], k + 1)
                if not a.only or a.only in name:
                    print("%-26s %s" % (name, L["note"]))
        return 0
    if not a.out:
        ap.error("--out is required (or use --list to see the cases)")

    os.makedirs(a.out, exist_ok=True)
    index, tags = [], {}
    for L in Ls:
        for k, spec in enumerate(L["stmts"]):
            name = "%s_%d" % (L["id"], k + 1)
            if a.only and a.only not in name:
                continue
            try:
                name, truth, npg = build_case(L, k, spec, a.out)
            except GenError as e:
                sys.exit("GENERATOR SELF-CHECK FAILED in %s: %s" % (name, e))
            print("%-26s %2d rows %d page(s)  %s" % (name, truth["row_count"], npg, L["note"][:60]))
            index.append({"case": name, "layout": L["id"], "rows": truth["row_count"],
                          "pages": npg, "features": truth["features"]})
            for t in truth["features"]:
                tags[t] = tags.get(t, 0) + 1
    with open(os.path.join(a.out, "index.json"), "w") as f:
        json.dump(index, f, indent=1)
    print("\n%d statement(s) from %d layout(s) written to %s (%s split)"
          % (len(index), len({x["layout"] for x in index}), a.out, a.split))
    return 0


if __name__ == "__main__":
    sys.exit(main())
